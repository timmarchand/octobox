# =============================================================================
# FIXED KEYWORD SERVER
# =============================================================================

keywordServer <- function(id, token_data, selected_text_and_meta, meta_filter, tagged_data = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Check if tagged data is available for UI conditional logic ----
    output$tagged_available <- reactive({
      !is.null(tagged_data) && !is.null(tagged_data()) && tagged_data()$available
    })
    outputOptions(output, "tagged_available", suspendWhenHidden = FALSE)
    
    # # 2. Helper to convert tagged DF to quanteda tokens ----
    # # Note: Ensure this function exists in your global utilities or here
    # convert_tagged_to_tokens <- function(df, tag_type) {
    #   # This assumes your tagged_data()$df has columns: doc_id, token, lemma, pos, upos
    #   if (tag_type == "pos_only") {
    #     txt <- aggregate(pos ~ doc_id, data = df, paste, collapse = " ")
    #   } else if (tag_type == "lemma") {
    #     df$val <- paste0(df$lemma, "_", df$pos)
    #     txt <- aggregate(val ~ doc_id, data = df, paste, collapse = " ")
    #   } else if (tag_type == "upos") {
    #     df$val <- paste0(df$token, "_", df$upos)
    #     txt <- aggregate(val ~ doc_id, data = df, paste, collapse = " ")
    #   } else { # xpos default
    #     df$val <- paste0(df$token, "_", df$pos)
    #     txt <- aggregate(val ~ doc_id, data = df, paste, collapse = " ")
    #   }
    #   
    #   # Convert to quanteda tokens and re-attach metadata
    #   toks <- quanteda::tokens(quanteda::corpus(txt, text_field = "val", docid_field = "doc_id"))
    #   
    #   # Sync docvars from the original token_data
    #   quanteda::docvars(toks) <- quanteda::docvars(token_data())
    #   return(toks)
    # }

    # 3. Active tokens - switch logic ----
    active_tokens <- reactive({
      # Use %||% to handle cases where UI hasn't initialized yet
      use_tagged <- input$use_tagged_tokens %||% FALSE
      
      if (use_tagged) {
        req(tagged_data())
        req(tagged_data()$df)
        cat("Using TAGGED tokens for keyword analysis\n")
        return(convert_tagged_to_tokens(tagged_data()$df, input$tag_column %||% "xpos"))
      } else {
        cat("Using REGULAR tokens for keyword analysis\n")
        return(token_data())
      }
    })

    # 4. Target/Reference Dynamic Selection ----
    # FIXED: Using 'meta_filter' (the argument) instead of 'meta_filter_global'
    observe({
      tryCatch({
        data <- selected_text_and_meta()
        current_filter <- meta_filter() # Use the reactive argument

        if (is.null(data) || is.null(data$meta) || is.null(current_filter)) {
          updateCheckboxGroupInput(session, "keyword_target", choices = NULL)
          return()
        }

        available_choices <- intersect(unique(data$meta), current_filter)
        available_choices <- available_choices[!is.na(available_choices) & available_choices != ""]

        if (length(available_choices) > 0) {
          updateCheckboxGroupInput(session, "keyword_target",
                                   choices = sort(available_choices),
                                   selected = input$keyword_target) # Persist selection
        }
      }, error = function(e) cat("Error in Selection Update:", e$message, "\n"))
    })

  # 5. TARGET/REFERENCE SELECTION HANDLERS ----
    # =============================================================================

    # 5.1. Target: Select All
    observeEvent(input$select_all_targets, {
      data <- selected_text_and_meta()
      current_f <- meta_filter()
      req(data$meta, current_f)
      
      available <- sort(intersect(unique(data$meta), current_f))
      # Filter out the default "pasted_text" if other meta exists
      if(length(available) > 1) available <- available[available != "pasted_text"]
      
      updateCheckboxGroupInput(session, "keyword_target", selected = available)
    })

    # 5.2. Target: Clear
    observeEvent(input$clear_targets, {
      updateCheckboxGroupInput(session, "keyword_target", selected = character(0))
    })

    # 5.3. Dynamic Update: Choices for Target
    observe({
      data <- selected_text_and_meta()
      current_f <- meta_filter()
      
      # CRITICAL: Stop the update if data is mid-refresh or empty
      req(data$meta, current_f)
      if(all(data$meta == "pasted_text") && length(current_f) == 0) return()

      available <- sort(intersect(unique(data$meta), current_f))
      available <- available[!is.na(available) & available != ""]
      
      # Only update if the choices actually changed to prevent flickering/resetting
      current_choices <- sort(as.character(available))
      
      updateCheckboxGroupInput(session, "keyword_target",
                               choices = current_choices,
                               selected = input$keyword_target)
    })

    # 5.4. Sync Reference choices when Target changes
    observe({
      data <- selected_text_and_meta()
      current_f <- meta_filter()
      req(data$meta, current_f, input$keyword_target)

      available <- intersect(unique(data$meta), current_f)
      # Reference is Available MINUS Target
      ref_choices <- sort(setdiff(available, input$keyword_target))

      updateCheckboxGroupInput(session, "keyword_reference",
                               choices = ref_choices,
                               selected = input$keyword_reference)
    })

    # 5.5. Reference: Select All
    observeEvent(input$select_all_reference, {
      data <- selected_text_and_meta()
      current_f <- meta_filter()
      req(data$meta, current_f)
      
      available <- intersect(unique(data$meta), current_f)
      target_sel <- input$keyword_target %||% character(0)
      to_select <- sort(setdiff(available, target_sel))
      
      updateCheckboxGroupInput(session, "keyword_reference", selected = to_select)
    })

    # 5.6. Reference: Deselect All
    observeEvent(input$deselect_all_reference, {
      updateCheckboxGroupInput(session, "keyword_reference", selected = character(0))
    })


    # 6. FIXED KEYWORD ANALYSIS LOGIC ----
    # =============================================================================
    keyword_results <- eventReactive(input$run_keyword_analysis, {
      req(active_tokens())
      req(input$keyword_target)
      req(input$keyword_reference)
      req(length(input$keyword_target) > 0)
      req(length(input$keyword_reference) > 0)

      withProgress(message = 'Running keyword analysis...', value = 0, {

        tryCatch({
          showNotification("Starting keyword analysis...", type = "message", duration = 3)

          toks <- active_tokens()
          n <- input$keyword_ngram_n
          top_n <- input$keyword_top_n

          incProgress(0.2, detail = "Processing tokens...")

          # Applying stopword filtering BEFORE n-gram generation
          tokens_processed <- toks

          if (input$use_stopwords %||% FALSE) {
            incProgress(0.3, detail = "Applying stop words filter...")

            stopwords_list <- create_stopword_list(
              input$stopword_language %||% "en",
              input$include_contractions %||% TRUE,
              input$custom_stopwords %||% "",
              input$custom_stopword_mode %||% "add"
            )

            cat("Using", length(stopwords_list), "stop words for keyword analysis\n")
            tokens_processed <- quanteda::tokens_remove(tokens_processed, stopwords_list, case_insensitive = TRUE)

            total_tokens_after <- sum(quanteda::ntoken(tokens_processed))
            if (total_tokens_after == 0) {
              showNotification("Warning: No tokens remain after stopword filtering. Try using fewer stopwords.", type = "warning")
              return(NULL)
            }
            cat("Tokens after stopword removal:", total_tokens_after, "\n")
          }

          incProgress(0.4, detail = "Generating n-grams...")
          if (n > 1) {
            toks_ngrams <- quanteda::tokens_ngrams(tokens_processed, n = n)
          } else {
            toks_ngrams <- tokens_processed
          }

          incProgress(0.5, detail = "Creating document-feature matrix...")
          dfm_ngrams <- quanteda::dfm(toks_ngrams)

          if (quanteda::nfeat(dfm_ngrams) == 0) {
            showNotification("No features found in document-feature matrix", type = "error")
            return(NULL)
          }

          incProgress(0.6, detail = "Processing groups...")
          meta_values <- quanteda::docvars(dfm_ngrams, "meta")

          if (is.null(meta_values) || length(meta_values) == 0) {
            showNotification("Error: No metadata found for documents", type = "error")
            return(NULL)
          }

          doc_groups <- ifelse(meta_values %in% input$keyword_target, "target", "reference")
          all_selected_groups <- c(input$keyword_target, input$keyword_reference)
          keep_docs <- meta_values %in% all_selected_groups

          if (sum(keep_docs) == 0) {
            showNotification("Error: No documents match the selected groups", type = "error")
            return(NULL)
          }

          dfm_filtered <- dfm_ngrams[keep_docs, ]
          doc_groups_filtered <- doc_groups[keep_docs]
          meta_filtered <- meta_values[keep_docs]

          incProgress(0.7, detail = "Calculating frequencies...")
          dfm_grouped <- quanteda::dfm_group(dfm_filtered, groups = doc_groups_filtered)

          group_names <- rownames(dfm_grouped)
          if (!"target" %in% group_names || !"reference" %in% group_names) {
            showNotification("Error: Missing target or reference group after filtering.", type = "error")
            return(NULL)
          }

          freq_df <- as.data.frame(as.matrix(dfm_grouped))
          freq_df$group <- rownames(freq_df)

          freq_long <- freq_df %>%
            tidyr::pivot_longer(cols = -group, names_to = "feature", values_to = "count") %>%
            tidyr::pivot_wider(names_from = group, values_from = count, values_fill = 0)

          incProgress(0.8, detail = "Calculating log-odds...")

          if ("target" %in% names(freq_long) && "reference" %in% names(freq_long)) {
            # Log-odds calculation with smoothing
            logodds_df <- freq_long %>%
              mutate(
                target_smooth = target + 1,
                reference_smooth = reference + 1,
                target_total = sum(target) + nrow(.),
                reference_total = sum(reference) + nrow(.),
                log_odds = log((target_smooth / target_total) / (reference_smooth / reference_total)),
                abs_log_odds = abs(log_odds),
                group = ifelse(log_odds > 0, paste(input$keyword_target, collapse = " + "), "Reference")
              ) %>%
              arrange(desc(abs_log_odds)) %>%
              select(feature, log_odds, abs_log_odds, group, target, reference)
          } else {
            showNotification("Error: Unable to calculate log-odds", type = "error")
            return(NULL)
          }

          dfm_by_meta <- quanteda::dfm_group(dfm_filtered, groups = meta_filtered)
          meta_freq <- as.data.frame(as.matrix(dfm_by_meta))
          meta_freq$meta <- rownames(meta_freq)

          meta_freq_long <- meta_freq %>%
            tidyr::pivot_longer(cols = -meta, names_to = "feature", values_to = "count") %>%
            group_by(meta) %>%
            mutate(
              total = sum(count),
              relative_freq = count / total * 1000
            ) %>%
            ungroup()

          incProgress(1, detail = "Complete!")
          showNotification("Keyword analysis completed!", type = "message")

          final_results <- list(
            logodds = logodds_df,
            meta_freq = meta_freq_long,
            target = input$keyword_target,
            reference = input$keyword_reference,
            n = n,
            stopwords_used = input$use_stopwords,
            stopword_language = if(input$use_stopwords) input$stopword_language else NULL
          )

          return(final_results)

        }, error = function(e) {
          showNotification(paste("Keyword analysis error:", e$message), type = "error")
          cat("Full error details:", paste(traceback(), collapse = "\n"), "\n")
          return(NULL)
        })
      })
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # 7. OUTPUT HANDLERS ----
    # =============================================================================

    # 7.1. Summary Statistics
    output$keyword_summary <- renderTable({
      req(keyword_results())
      results <- keyword_results()

      summary_df <- data.frame(
        Metric = c("Target Groups",
                   "Reference Groups",
                   "N-gram Size",
                   "Stop Words Removed",
                   "Total Features Analyzed",
                   "Features Favoring Target",
                   "Features Favoring Reference"),
        Value = c(paste(results$target, collapse = ", "),
                  paste(results$reference, collapse = ", "),
                  results$n,
                  ifelse(results$stopwords_used,
                         paste("Yes (", results$stopword_language, ")", sep = ""),
                         "No"),
                  nrow(results$logodds),
                  sum(results$logodds$log_odds > 0),
                  sum(results$logodds$log_odds < 0))
      )
      summary_df
    })

    # 7.2. Comparison Plot
    output$keyword_comparison_plot <- renderPlot({
      req(keyword_results())
      results <- keyword_results()

      if (is.null(results$logodds) || nrow(results$logodds) == 0) {
        return(ggplot() +
                 annotate("text", x = 1, y = 1,
                          label = "No keyword results to plot.\nRun keyword analysis first.",
                          size = 6) +
                 theme_void())
      }

      tryCatch({
        create_enhanced_keyword_plot(
          results,
          input$keyword_top_n,
          input$plot_type,
          input$show_counts
        )
      }, error = function(e) {
        cat("Plot error:", e$message, "\n")
        tryCatch({
          create_simple_keyword_plot(results, input$keyword_top_n)
        }, error = function(e2) {
          ggplot() +
            annotate("text", x = 1, y = 1,
                     label = paste("Plot error. Please check console for details."),
                     size = 5) +
            theme_void()
        })
      })
    })

    # 7.3. Plot Download Handler
    output$download_keyword_plot <- downloadHandler(
      filename = function() {
        paste0("keyword_analysis_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(keyword_results())
        results <- keyword_results()

        if (is.null(results$logodds) || nrow(results$logodds) == 0) {
          p <- ggplot() + annotate("text", x = 1, y = 1, label = "No results", size = 6) + theme_void()
          ggsave(file, plot = p, width = 10, height = 6, dpi = 300, bg = "white")
          return()
        }

        tryCatch({
          p <- create_enhanced_keyword_plot(results, input$keyword_top_n, input$plot_type, input$show_counts)
          ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
        }, error = function(e) {
          p <- create_simple_keyword_plot(results, input$keyword_top_n)
          ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
        })
      }
    )

    # 7.4. Detailed Scores Table
    output$keyword_scores_table <- DT::renderDT({
      req(keyword_results())
      results <- keyword_results()

      detailed_df <- results$logodds %>%
        left_join(
          results$meta_freq %>%
            filter(meta %in% results$target) %>%
            group_by(feature) %>%
            summarise(target_count = sum(count), target_rel_freq = mean(relative_freq)),
          by = "feature"
        ) %>%
        left_join(
          results$meta_freq %>%
            filter(meta %in% results$reference) %>%
            group_by(feature) %>%
            summarise(reference_count = sum(count), reference_rel_freq = mean(relative_freq)),
          by = "feature"
        ) %>%
        mutate(
          target_count = replace_na(target_count, 0),
          reference_count = replace_na(reference_count, 0),
          target_rel_freq = round(replace_na(target_rel_freq, 0), 2),
          reference_rel_freq = round(replace_na(reference_rel_freq, 0), 2),
          log_odds = round(log_odds, 3)
        ) %>%
        select(feature, log_odds, group, target_count, target_rel_freq,
               reference_count, reference_rel_freq) %>%
        arrange(desc(abs(log_odds)))

      DT::datatable(detailed_df,
                    options = list(pageLength = 25),
                    colnames = c("Feature", "Log-Odds", "Favors",
                                 paste(paste(results$target, collapse = "+"), "Count"),
                                 paste(paste(results$target, collapse = "+"), "Freq/1000"),
                                 "Ref Count", "Ref Freq/1000"))
    })

    # 7.5. CSV Download Handler
    output$download_keyword_csv <- downloadHandler(
      filename = function() {
        paste0("keyword_analysis_multiple_targets_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(keyword_results())
        results <- keyword_results()

        detailed_df <- results$logodds %>%
          left_join(
            results$meta_freq %>%
              filter(meta %in% results$target) %>%
              group_by(feature) %>%
              summarise(target_count = sum(count), target_rel_freq = mean(relative_freq)),
            by = "feature"
          ) %>%
          left_join(
            results$meta_freq %>%
              filter(meta %in% results$reference) %>%
              group_by(feature) %>%
              summarise(reference_count = sum(count), reference_rel_freq = mean(relative_freq)),
            by = "feature"
          ) %>%
          mutate(
            target_groups = paste(results$target, collapse = "; "),
            reference_groups = paste(results$reference, collapse = "; "),
            ngram_size = results$n,
            stopwords_removed = results$stopwords_used,
            stopword_language = results$stopword_language %||% "None",
            target_count = replace_na(target_count, 0),
            reference_count = replace_na(reference_count, 0),
            target_rel_freq = round(replace_na(target_rel_freq, 0), 3),
            reference_rel_freq = round(replace_na(reference_rel_freq, 0), 3),
            log_odds = round(log_odds, 4),
            analysis_type = "Multiple Target Groups"
          ) %>%
          select(feature, log_odds, favors = group, target_groups, reference_groups,
                 target_count, target_rel_freq, reference_count, reference_rel_freq,
                 ngram_size, stopwords_removed, stopword_language, analysis_type) %>%
          arrange(desc(abs(log_odds)))

        readr::write_csv(detailed_df, file)
      }
    )

  }) # Close moduleServer
} # Close keywordServer function