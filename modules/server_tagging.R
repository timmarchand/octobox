# =============================================================================
# modules/server_tagging.R - POS Tagging Engine -------------------------------
# =============================================================================

taggingServer <- function(id, tokens, meta_filter, shared_values = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Model Lifecycle Management -------------------------------------------
    
    model_loaded <- reactiveVal(FALSE)
    udpipe_model <- reactiveVal(NULL)
    
    # Observe: Model Loading Logic ----
    observeEvent(input$enable_tagging, {
      if (input$enable_tagging && !model_loaded()) {
        
        withProgress(message = 'Loading UDPipe language model...', value = 0, {
          model_path <- "english-ewt-ud-2.5-191206.udpipe"
          
          # Download if missing ----
          if (!file.exists(model_path)) {
            incProgress(0.3, detail = "Downloading model (one-time setup)...")
            tryCatch({
              udpipe::udpipe_download_model(language = "english-ewt", model_dir = ".")
            }, error = function(e) {
              showNotification(paste("Download failed:", e$message), type = "error")
              updateCheckboxInput(session, "enable_tagging", value = FALSE)
              return()
            })
          }
          
          # Load into Memory ----
          incProgress(0.6, detail = "Loading model into memory...")
          tryCatch({
            model <- udpipe::udpipe_load_model(model_path)
            udpipe_model(model)
            model_loaded(TRUE)
            incProgress(1, detail = "Model ready!")
          }, error = function(e) {
            showNotification(paste("Load failed:", e$message), type = "error")
            updateCheckboxInput(session, "enable_tagging", value = FALSE)
          })
        })
      }
    })
    
    
    # 2. Annotation Pipeline --------------------------------------------------
    
    tagged_results <- eventReactive(input$run_tagger, {
      req(input$enable_tagging, model_loaded(), udpipe_model())
      
      # Prepare Data & Filter ----
      toks <- tokens()
      meta_all <- quanteda::docvars(toks, "meta")
      filter_meta <- meta_filter()
      
      if (!is.null(filter_meta) && length(filter_meta) > 0) {
        keep <- meta_all %in% filter_meta
        toks <- toks[keep]
        meta_all <- meta_all[keep]
      }
      
      # Quanteda to UDPipe format ----
      tokens_list <- quanteda::as.list(toks)
      text_vector <- sapply(tokens_list, paste, collapse = " ")
      doc_ids <- names(tokens_list) %||% paste0("Text_", seq_along(tokens_list))
      
      # Corpus Size Safety Check ----
      total_words <- sum(sapply(tokens_list, length))
      if (sum(nchar(text_vector)) > 5000000) {
        showNotification("Corpus too large (>100k words). Use filters.", type = "error")
        return(NULL)
      }
      
      # Run Annotation ----
      withProgress(message = 'Tagging corpus with UDPipe...', value = 0, {
        incProgress(0.3, detail = paste("Processing", length(text_vector), "docs..."))
        
        annotated <- tryCatch({
          udpipe::udpipe_annotate(udpipe_model(), x = text_vector, doc_id = doc_ids)
        }, error = function(e) {
          showNotification(paste("Tagging failed:", e$message), type = "error")
          return(NULL)
        })
        
        req(annotated)
        incProgress(0.8, detail = "Finalizing data frame...")
        
        df <- as.data.frame(annotated)
        meta_lookup <- setNames(meta_all, doc_ids)
        df$meta <- meta_lookup[df$doc_id] # Attach meta
        
        # Shared State Update ----
        if (!is.null(shared_values)) {
          shared_values$has_tagged_tokens <- TRUE
          shared_values$tagged_df <- df
        }
        
        return(list(df = df, n_docs = length(unique(df$doc_id)), n_tokens = nrow(df)))
      })
    })
    
    
    # 3. Output Formatting Logic ----------------------------------------------
    
    formatted_output <- reactive({
      req(tagged_results())
      df <- tagged_results()$df
      tag_col <- if (input$tag_format == "xpos") "xpos" else "upos"
      
      if (input$output_format == "inline") {
        df %>%
          dplyr::group_by(doc_id) %>%
          dplyr::summarize(tagged_text = paste0(token, "_", .data[[tag_col]], collapse = " "), .groups = "drop")
        
      } else if (input$output_format == "vertical") {
        df %>%
          dplyr::select(doc_id, sentence_id, token_id, token, !!tag_col) %>%
          dplyr::mutate(tagged = paste0(token, "_", .data[[tag_col]]))
        
      } else {
        df %>% dplyr::select(meta, doc_id, sentence_id, token_id, token, lemma, upos, xpos, feats)
      }
    })
    
    
    # 4. Render UI Components -------------------------------------------------
    
    output$has_results <- reactive({ !is.null(tagged_results()) })
    outputOptions(output, "has_results", suspendWhenHidden = FALSE)
    
    output$tagging_summary <- renderUI({
      req(tagged_results())
      stats <- tagged_results()
      div(class = "alert alert-success",
          HTML(paste0("<strong>✅ Tagging Complete!</strong><br/>", 
                      "Docs: ", stats$n_docs, " | Tokens: ", format(stats$n_tokens, big.mark = ","))))
    })
    
    output$tagged_text <- renderPrint({
  req(formatted_output())
  result <- formatted_output()
  df <- tagged_results()$df
  
  # Get unique doc_ids and sort them NUMERICALLY
  all_doc_ids <- unique(result$doc_id)
  
  # Sort intelligently: extract numbers if present, otherwise alphabetical
  sorted_doc_ids <- tryCatch({
    # Try to extract numbers from doc_ids (e.g., "Text_10" → 10)
    doc_numbers <- as.numeric(gsub("\\D", "", all_doc_ids))
    if (any(is.na(doc_numbers))) {
      # Fall back to alphabetical if extraction fails
      sort(all_doc_ids)
    } else {
      # Sort by extracted numbers
      all_doc_ids[order(doc_numbers)]
    }
  }, error = function(e) {
    sort(all_doc_ids)
  })
  
  # Calculate padding for display
  n_docs <- length(sorted_doc_ids)
  padding <- nchar(as.character(n_docs))
  
  # Custom printing with Meta Headers ----
  for (i in seq_along(sorted_doc_ids)) {
    doc <- sorted_doc_ids[i]
    doc_meta <- df$meta[df$doc_id == doc][1]
    
    # Use zero-padded numbers for display
    display_num <- sprintf(paste0("%0", padding, "d"), i)
    cat("=== Text ", display_num, " <", doc_meta, "> ===\n", sep = "")
    
    if (input$output_format == "inline") {
      cat(result$tagged_text[result$doc_id == doc], "\n\n")
    } else {
      cat(paste(result$tagged[result$doc_id == doc], collapse = "\n"), "\n\n")
    }
  }
})
    
    output$tagged_table <- DT::renderDT({
      req(formatted_output(), input$output_format == "table")
      DT::datatable(formatted_output(), options = list(pageLength = 25, scrollX = TRUE), filter = "top")
    })
    
    
    # 5. Download Handler -----------------------------------------------------
    
    output$download_tagged <- downloadHandler(
      filename = function() {
        ext <- if (input$output_format == "table") ".csv" else ".txt"
        paste0("tagged_corpus_", Sys.Date(), ext)
      },
      content = function(file) {
        req(formatted_output())
        result <- formatted_output()
        
        if (input$output_format == "table") {
          readr::write_csv(result, file)
        } else {
          # Use the same logic as renderPrint but write to file ----
          output_lines <- capture.output(output$tagged_text())
          writeLines(output_lines, file)
        }
      }
    )
  })
}