# =============================================================================
# modules/server_tokenization.R - Tokenization Engine -------------------------
# =============================================================================

library(shiny)
library(quanteda)
library(dplyr)
library(digest)

tokenizationServer <- function(id, selected_text_and_meta, meta_filter_global, values) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Performance & State Tracking -----------------------------------------
    
    # Local metrics for monitoring engine health
    tokenization_metrics <- reactiveValues(
      last_tokenization_time = NULL,
      tokens_processed = 0,
      cache_hits = 0,
      processing_large_corpus = FALSE
    )
    
    # Main data reactive containers
    token_data <- reactiveVal(NULL)
    tokenization_complete <- reactiveVal(FALSE)
    
    
    # 2. Main Tokenization Logic (observeEvent) -------------------------------
    
    observeEvent(input$run_tokenize, {
      req(selected_text_and_meta())
      overall_start_time <- Sys.time()
      
      withProgress(message = 'Optimized Tokenization...', value = 0, {
        
        # Data Extraction & Filtering ----
        text <- selected_text_and_meta()$text
        meta <- selected_text_and_meta()$meta
        filter_meta <- meta_filter_global()
        
        incProgress(0.1, detail = "Applying filters...")
        
        if (!is.null(filter_meta)) {
          keep <- meta %in% filter_meta
          text <- text[keep]
          meta <- meta[keep]
        }
        
        # Validation & Cleaning ----
        incProgress(0.2, detail = "Validating text data...")
        valid_idx <- !is.na(text) & nchar(trimws(text)) > 0
        text <- text[valid_idx]
        meta <- meta[valid_idx]
        
        if (length(text) == 0) {
          token_data(NULL)
          tokenization_complete(FALSE)
          showNotification("No valid text data.", type = "error")
          return(NULL)
        }
        
        # Performance Threshold Check ----
        total_chars <- sum(nchar(text))
        corpus_size <- length(text)
        tokenization_metrics$processing_large_corpus <- total_chars > 100000 || corpus_size > 1000
        
        # Optimized Caching Mechanism ----
        incProgress(0.3, detail = "Checking cache...")
        text_hash <- digest::digest(list(text, input$remove_punct))
        cache_key <- paste0("processed_text_", text_hash)
        
        if (exists(cache_key, envir = .GlobalEnv)) {
          text_processed <- get(cache_key, envir = .GlobalEnv)
          tokenization_metrics$cache_hits <- tokenization_metrics$cache_hits + 1
        } else {
          text_processed <- process_text_for_tokenization_optimized(text, remove_punct = input$remove_punct %||% TRUE)
          assign(cache_key, text_processed, envir = .GlobalEnv)
        }
        
        # Quanteda Processing ----
        incProgress(0.6, detail = "Creating corpus...")
        corpus_df <- data.frame(doc_id = seq_along(text_processed), text = text_processed, meta = meta, stringsAsFactors = FALSE)
        corp <- quanteda::corpus(corpus_df, text_field = "text")
        
        incProgress(0.8, detail = "Tokenizing...")
        toks <- quanteda::tokens(
          corp, 
          remove_punct = input$remove_punct %||% TRUE, 
          remove_symbols = TRUE,
          remove_numbers = input$remove_numbers %||% FALSE,
          split_hyphens = !tokenization_metrics$processing_large_corpus, # Speed opt
          include_docvars = TRUE
        )
        
        # Finalizing & Metadata ----
        quanteda::docvars(toks, "meta") <- meta
        token_data(toks)
        values$token_data <- toks
        tokenization_complete(TRUE)
        
        # Update Stats ----
        elapsed_time <- as.numeric(Sys.time() - overall_start_time, units = "secs")
        tokenization_metrics$tokens_processed <- sum(quanteda::ntoken(toks))
        tokenization_metrics$last_tokenization_time <- elapsed_time
        
        incProgress(1, detail = "Complete!")
        if (tokenization_metrics$processing_large_corpus) gc()
      })
    })
    
    
    # 3. UI Status Outputs ----------------------------------------------------
    
    output$tokenization_complete <- reactive({
      tokenization_complete() && !is.null(token_data())
    })
    outputOptions(output, "tokenization_complete", suspendWhenHidden = FALSE)
    
    output$tokenization_status <- renderText({
      req(token_data())
      toks <- token_data()
      paste0("✓ Documents: ", quanteda::ndoc(toks), 
             " | Tokens: ", format(sum(quanteda::ntoken(toks)), big.mark = ","))
    })
    
    
    # 4. Download Handlers ----------------------------------------------------
    
    # CSV Export: Detailed breakdown ----
    output$download_tokens_csv <- downloadHandler(
      filename = function() { paste0("tokenized_data_", Sys.Date(), ".csv") },
      content = function(file) {
        req(token_data())
        toks <- token_data()
        
        # Process list for export
        tokens_list <- as.list(toks)
        meta_values <- quanteda::docvars(toks, "meta")
        
        token_df_list <- lapply(seq_along(tokens_list), function(i) {
          if (length(tokens_list[[i]]) == 0) return(NULL)
          data.frame(
            token = as.character(tokens_list[[i]]),
            document_id = i,
            meta_group = meta_values[i],
            position = seq_along(tokens_list[[i]]),
            stringsAsFactors = FALSE
          )
        })
        
        all_tokens_df <- dplyr::bind_rows(token_df_list)
        readr::write_csv(all_tokens_df, file)
      }
    )
    
    # TXT Export: Simple list ----
    output$download_tokens_txt <- downloadHandler(
      filename = function() { paste0("tokens_list_", Sys.Date(), ".txt") },
      content = function(file) {
        req(token_data())
        all_tokens <- unlist(as.list(token_data()))
        writeLines(all_tokens, file, useBytes = TRUE)
      }
    )
    
    
    # 5. Module Return --------------------------------------------------------
    
    return(list(
      token_data = token_data,
      meta_filter = reactive(input$meta_filter_global),
      performance_metrics = tokenization_metrics
    ))
    
  }) # End moduleServer
}