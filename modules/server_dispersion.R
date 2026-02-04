# =============================================================================
# modules/server_dispersion.R - Dispersion Analysis Server (Phase 1) ----
# =============================================================================

dispersionServer <- function(id, token_data, meta_filter, tagged_data = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Check if tagged data is available
    output$tagged_available <- reactive({
      !is.null(tagged_data) && !is.null(tagged_data()) && tagged_data()$available
    })
    outputOptions(output, "tagged_available", suspendWhenHidden = FALSE)
    
    # Active tokens - switch between regular and tagged
  active_tokens <- reactive({
  if (input$use_tagged_tokens %||% FALSE) {
    req(tagged_data())
    req(tagged_data()$df)
    
    cat("Using TAGGED tokens for dispersion analysis\n")
    convert_tagged_to_tokens(tagged_data()$df, input$tag_column %||% "xpos")
  } else {
    cat("Using REGULAR tokens for dispersion analysis\n")
    token_data()
  }
})
    
    # 1. HELPER FUNCTIONS: DISPERSION CALCULATIONS ----

    # Gries's DP (Deviation of Proportions)
    calculate_dp <- function(frequencies, corpus_sizes) {
      if (sum(frequencies) == 0) return(NA)

      # Normalized proportions
      observed_props <- frequencies / sum(frequencies)
      expected_props <- corpus_sizes / sum(corpus_sizes)

      # DP calculation: sum of absolute differences / 2
      dp <- sum(abs(observed_props - expected_props)) / 2

      return(round(dp, 4))
    }

    # Range (proportion of parts containing the word)
    calculate_range <- function(frequencies) {
      n_parts_with_word <- sum(frequencies > 0)
      n_parts_total <- length(frequencies)

      range_prop <- n_parts_with_word / n_parts_total
      return(round(range_prop * 100, 2))  # Return as percentage
    }

    # 2. MAIN DISPERSION ANALYSIS LOGIC ----

    dispersion_results <- eventReactive(input$run_dispersion, {
      req(active_tokens())
      req(input$search_term)
      req(nchar(trimws(input$search_term)) > 0)

      withProgress(message = 'Calculating dispersion...', value = 0, {

        search_term <- tolower(trimws(input$search_term))
        use_regex <- input$use_regex %||% FALSE  # GET REGEX SETTING

        incProgress(0.2, detail = "Processing tokens...")

        # Get tokens
        toks <- active_tokens()

        # Apply meta filter
        meta_all <- quanteda::docvars(toks, "meta")
        filter_meta <- meta_filter()

        if (!is.null(filter_meta) && length(filter_meta) > 0) {
          keep <- meta_all %in% filter_meta
          toks <- toks[keep]
          meta_all <- meta_all[keep]
        }

        if (quanteda::ndoc(toks) == 0) {
          showNotification("No documents after filtering", type = "error")
          return(NULL)
        }

        incProgress(0.4, detail = "Searching for term...")

# Convert to character and lowercase
tokens_list <- quanteda::as.list(toks)
tokens_lower <- lapply(tokens_list, tolower)


        
        

        # Determine grouping level
        if (input$dispersion_level == "meta") {
          # Group by meta
          unique_groups <- unique(meta_all)
          group_labels <- unique_groups

          # Combine tokens by meta group
          grouped_tokens <- list()
          group_sizes <- numeric(length(unique_groups))

          for (i in seq_along(unique_groups)) {
            group_docs <- which(meta_all == unique_groups[i])
            grouped_tokens[[i]] <- unlist(tokens_lower[group_docs])
            group_sizes[i] <- length(grouped_tokens[[i]])
          }

        } else {
          # Individual texts
          group_labels <- names(tokens_lower)
          if (is.null(group_labels)) {
            group_labels <- paste0("Text_", seq_along(tokens_lower))
          }
          grouped_tokens <- tokens_lower
          group_sizes <- sapply(grouped_tokens, length)
        }

        incProgress(0.6, detail = "Counting frequencies...")
        
        # UPDATED: Smart search for tagged vs regular tokens
# UPDATED: Smart search for tagged vs regular tokens
is_tagged_search <- input$use_tagged_tokens %||% FALSE

if (is_tagged_search) {
  # Tagged token search logic
  if (startsWith(search_term, "_")) {
    # POS-only search: _noun matches anything_NOUN
    search_pattern <- paste0("\\w+", search_term)  # Keep lowercase from search
    use_regex <- TRUE
  } else if (grepl("_", search_term)) {
    # Already has tag: run_noun or run_verb
    search_pattern <- search_term  # Keep as-is
    use_regex <- FALSE
  } else {
    # Word without tag: run matches run_verb, run_noun, etc.
    search_pattern <- paste0(search_term, "_\\w+")
    use_regex <- TRUE
  }
} else {
  # Regular token search
  search_pattern <- search_term
}

# ADD DEBUG HERE:
cat("=== DEBUG: Token inspection ===\n")
cat("First 20 tokens from first doc:\n")
print(head(tokens_lower[[1]], 20))
cat("Search term:", search_term, "\n")
cat("Is tagged search:", is_tagged_search, "\n")
cat("Use regex:", use_regex, "\n")
cat("Search pattern:", search_pattern, "\n")
cat("===============================\n")

# Count occurrences - USE CASE INSENSITIVE MATCHING FOR TAGGED TOKENS
if (use_regex) {
  frequencies <- sapply(grouped_tokens, function(tokens) {
    sum(grepl(search_pattern, tokens, perl = TRUE, ignore.case = TRUE))  # ← Already has ignore.case
  })
} else {
  # For exact match with tagged tokens, make case insensitive
  if (is_tagged_search) {
    frequencies <- sapply(grouped_tokens, function(tokens) {
      sum(tolower(tokens) == tolower(search_pattern))  # ← ADD tolower() to both sides
    })
  } else {
    # Regular tokens - keep original behavior
    frequencies <- sapply(grouped_tokens, function(tokens) {
      sum(tokens == search_pattern)
    })
  }
}

        # UPDATED: Count occurrences using regex or exact match
        # CORRECT:
if (use_regex) {
  frequencies <- sapply(grouped_tokens, function(tokens) {
    sum(grepl(search_pattern, tokens, perl = TRUE, ignore.case = TRUE))
  })
} else {
  frequencies <- sapply(grouped_tokens, function(tokens) {
    sum(tokens == search_pattern)
  })
}

        # Check if term was found
        if (sum(frequencies) == 0) {
          search_type <- if(use_regex) "pattern" else "term"
          showNotification(
            paste0(search_type, " '", search_term, "' not found in corpus"),
            type = "warning",
            duration = 5
          )
          return(NULL)
        }

        incProgress(0.8, detail = "Calculating dispersion measures...")

        # Calculate dispersion measures
        dp_value <- calculate_dp(frequencies, group_sizes)
        range_value <- calculate_range(frequencies)

        # Create results dataframe
        results <- data.frame(
          group = group_labels,
          frequency = frequencies,
          corpus_size = group_sizes,
          rel_freq_per_1000 = round((frequencies / group_sizes) * 1000, 2),
          stringsAsFactors = FALSE
        )

        # Add overall statistics
        overall_stats <- list(
          search_term = search_term,
          use_regex = use_regex,  # STORE REGEX FLAG
          total_occurrences = sum(frequencies),
          total_corpus_size = sum(group_sizes),
          dp = dp_value,
          range = range_value,
          n_groups = length(group_labels),
          measure = input$dispersion_measure,
          level = input$dispersion_level
        )

        incProgress(1, detail = "Complete!")

        return(list(
          results = results,
          stats = overall_stats
        ))
      })
    })

    # 3. OUTPUT: SUMMARY & STATUS ----

    output$has_results <- reactive({
      !is.null(dispersion_results())
    })
    outputOptions(output, "has_results", suspendWhenHidden = FALSE)

    output$dispersion_summary <- renderUI({
      req(dispersion_results())

      stats <- dispersion_results()$stats

      # Check if regex was used
      search_label <- if(stats$use_regex %||% FALSE) {
        paste0("<strong>Search pattern (regex):</strong> \"", stats$search_term, "\"")
      } else {
        paste0("<strong>Search term:</strong> \"", stats$search_term, "\"")
      }

      # Build summary text
      summary_parts <- c()

      summary_parts <- c(summary_parts,
        search_label,  # UPDATED LINE
        paste0("<strong>Total occurrences:</strong> ", stats$total_occurrences),
        paste0("<strong>Corpus size:</strong> ", format(stats$total_corpus_size, big.mark = ","), " tokens"),
        paste0("<strong>Groups analyzed:</strong> ", stats$n_groups, " ",
               ifelse(stats$level == "meta", "meta categories", "texts"))
      )

      if (stats$measure %in% c("dp", "both")) {
        interpretation <- if (stats$dp < 0.2) {
          "very even distribution"
        } else if (stats$dp < 0.4) {
          "moderately even distribution"
        } else if (stats$dp < 0.6) {
          "moderately uneven distribution"
        } else {
          "very uneven distribution"
        }

        summary_parts <- c(summary_parts,
          paste0("<strong>Gries's DP:</strong> ", stats$dp, " (", interpretation, ")")
        )
      }

      if (stats$measure %in% c("range", "both")) {
        summary_parts <- c(summary_parts,
          paste0("<strong>Range:</strong> ", stats$range, "% of ",
                 ifelse(stats$level == "meta", "categories", "texts"))
        )
      }

      HTML(paste(summary_parts, collapse = "<br/>"))
    })

    # 4. OUTPUT: RESULTS TABLE ----

    output$dispersion_table <- DT::renderDT({
      req(dispersion_results())

      results <- dispersion_results()$results

      # Sort by frequency descending
      results <- results[order(-results$frequency), ]

      DT::datatable(results,
                    options = list(
                      pageLength = 15,
                      scrollX = TRUE,
                      columnDefs = list(
                        list(className = "dt-right", targets = 1:3)
                      )
                    ),
                    colnames = c("Group", "Frequency", "Corpus Size", "Freq per 1000 tokens"),
                    rownames = FALSE) %>%
        DT::formatStyle("frequency",
                        background = DT::styleColorBar(range(results$frequency), "#2E86AB"),
                        backgroundSize = "98% 88%",
                        backgroundRepeat = "no-repeat",
                        backgroundPosition = "center")
    })

    # 5. OUTPUT: BAR PLOT ----

    output$dispersion_plot <- renderPlot({
      req(dispersion_results())

      results <- dispersion_results()$results
      stats <- dispersion_results()$stats

      # Check if we should facet by meta
      facet_by_meta <- input$facet_by_meta %||% FALSE

      # Add meta information if analyzing at text level
      if (stats$level == "text" && facet_by_meta) {
        # Get meta information from tokens
        toks <- active_tokens()
        meta_all <- quanteda::docvars(toks, "meta")
        filter_meta <- meta_filter()

        if (!is.null(filter_meta) && length(filter_meta) > 0) {
          keep <- meta_all %in% filter_meta
          meta_all <- meta_all[keep]
        }

        # Match meta to results
        doc_names <- names(toks)
        if (!is.null(filter_meta) && length(filter_meta) > 0) {
          doc_names <- doc_names[keep]
        }

        # Create a lookup for meta groups
        meta_lookup <- setNames(meta_all, doc_names)

        # Add meta column to results
        results$meta_group <- sapply(results$group, function(g) {
          if (g %in% names(meta_lookup)) {
            return(meta_lookup[[g]])
          } else {
            return("Unknown")
          }
        })
      }

      # Create bar plot
      if (stats$level == "text" && facet_by_meta && "meta_group" %in% names(results)) {
        # Faceted by meta group
        p <- ggplot2::ggplot(results, ggplot2::aes(x = reorder(group, frequency), y = frequency)) +
          ggplot2::geom_col(fill = "#2E86AB", alpha = 0.8) +
          ggplot2::coord_flip() +
          ggplot2::facet_wrap(~meta_group, scales = "free_y") +
          ggplot2::labs(
            title = paste0("Distribution of \"", stats$search_term, "\" by Meta Group"),
            subtitle = paste0("Overall DP = ", stats$dp, " | Range = ", stats$range, "%"),
            x = "Text",
            y = "Frequency"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 14, face = "bold"),
            plot.subtitle = ggplot2::element_text(size = 11, color = "gray50"),
            axis.text.y = ggplot2::element_text(size = 8),
            strip.text = ggplot2::element_text(face = "bold", size = 10),
            strip.background = ggplot2::element_rect(fill = "#e3f2fd", color = NA)
          )

      } else {
        # Single plot (original)
        results$group <- factor(results$group, levels = results$group[order(results$frequency)])

        p <- ggplot2::ggplot(results, ggplot2::aes(x = group, y = frequency)) +
          ggplot2::geom_col(fill = "#2E86AB", alpha = 0.8) +
          ggplot2::coord_flip() +
          ggplot2::labs(
            title = paste0("Distribution of \"", stats$search_term, "\" across ",
                           ifelse(stats$level == "meta", "Meta Groups", "Texts")),
            subtitle = paste0("DP = ", stats$dp, " | Range = ", stats$range, "%"),
            x = ifelse(stats$level == "meta", "Meta Group", "Text"),
            y = "Frequency"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 14, face = "bold"),
            plot.subtitle = ggplot2::element_text(size = 11, color = "gray50"),
            axis.text.y = ggplot2::element_text(size = 9)
          )
      }

      return(p)
    })

    # 6. OUTPUT: BARCODE PLOT & DYNAMIC UI ----

    output$dispersion_barcode <- renderPlot({
      req(dispersion_results())

      facet_by_meta <- isolate(input$facet_by_meta %||% FALSE)
      barcode_granularity <- input$barcode_granularity %||% "corpus"  # NEW

      results <- dispersion_results()$results
      stats <- dispersion_results()$stats

      # GET REGEX SETTING
      use_regex <- stats$use_regex %||% FALSE

      # Get token positions for the search term
      toks <- active_tokens()
      meta_all <- quanteda::docvars(toks, "meta")
      filter_meta <- meta_filter()

      if (!is.null(filter_meta) && length(filter_meta) > 0) {
        keep <- meta_all %in% filter_meta
        toks <- toks[keep]
        meta_all <- meta_all[keep]
      }

      # Get all unique meta groups FIRST
      all_meta_groups <- unique(meta_all)

      # Find all positions where term appears
      tokens_list <- quanteda::as.list(toks)
      tokens_lower <- lapply(tokens_list, tolower)

      search_term <- tolower(trimws(stats$search_term))

      # Collect positions - MODIFIED for text-level granularity
      positions_data <- list()

      for (i in seq_along(tokens_lower)) {
        tokens <- tokens_lower[[i]]

        # UPDATED: Use regex or exact match
        if (use_regex) {
          matches <- grep(search_term, tokens, perl = TRUE, ignore.case = TRUE)
        } else {
          matches <- which(tokens == search_term)
        }

        if (length(matches) > 0) {
          positions_data[[i]] <- data.frame(
            position = matches,
            doc_length = length(tokens),
            text = names(tokens_list)[i] %||% paste0("Text_", i),
            text_id = i,  # NEW: track text ID
            meta = meta_all[i],
            stringsAsFactors = FALSE
          )
        }
      }

      if (length(positions_data) == 0) {
        # No matches at all - show message
        p <- ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = paste0("No occurrences of \"", stats$search_term, "\" found"),
                            size = 6, color = "gray50") +
          ggplot2::theme_void()
        return(p)
      }

      positions_df <- dplyr::bind_rows(positions_data)

      # Total occurrences count
      total_occurrences <- nrow(positions_df)

      # Ensure ALL meta groups are represented
      positions_df$meta <- factor(positions_df$meta, levels = all_meta_groups)

      # NEW: Handle different granularity levels
      if (stats$level == "text" && barcode_granularity == "text") {
        # Individual text level - one plot per text

        # Get all text names
        all_texts <- names(tokens_list)

        # Add empty texts (those without matches)
        texts_with_hits <- unique(positions_df$text)
        texts_without_hits <- setdiff(all_texts, texts_with_hits)

        if (length(texts_without_hits) > 0) {
          empty_texts <- data.frame(
            position = 1,
            doc_length = 1,
            text = texts_without_hits,
            text_id = which(all_texts %in% texts_without_hits),
            meta = meta_all[which(all_texts %in% texts_without_hits)],
            is_placeholder = TRUE,
            stringsAsFactors = FALSE
          )

          positions_df$is_placeholder <- FALSE
          positions_df <- rbind(positions_df, empty_texts)
        } else {
          positions_df$is_placeholder <- FALSE
        }

        # Ensure text is a factor with all levels
        positions_df$text <- factor(positions_df$text, levels = all_texts)

        # Create barcode plot faceted by individual text
        p <- ggplot2::ggplot(positions_df, ggplot2::aes(x = position, y = 1)) +
          ggplot2::scale_y_continuous(limits = c(0.5, 1.5)) +
          ggplot2::labs(
            title = paste0("Dispersion Plot: \"", stats$search_term, "\" by Text"),
            subtitle = paste0("Total: ", total_occurrences, " occurrences across ", length(all_texts), " texts"),
            x = "Position in Text",
            y = ""
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 14, face = "bold"),
            plot.subtitle = ggplot2::element_text(size = 11, color = "gray50"),
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank()
          )

        # Add points
        p <- p + ggplot2::geom_point(
          data = positions_df[!positions_df$is_placeholder, ],
          size = 2, alpha = 0.6, color = "#2E86AB"
        )

        # Facet by individual text
        p <- p +
          ggplot2::facet_wrap(~text, scales = "free_x", ncol = 1, drop = FALSE) +
          ggplot2::theme(
            strip.text = ggplot2::element_text(face = "bold", size = 8),
            strip.background = ggplot2::element_rect(fill = "#e3f2fd", color = NA),
            panel.grid.major.x = ggplot2::element_line(color = "gray80", linetype = "dashed")
          ) +
          ggplot2::scale_x_continuous(
            breaks = function(limits) {
              if (is.na(limits[1]) || is.na(limits[2]) || limits[1] == limits[2]) {
                return(numeric(0))
              }
              seq(limits[1], limits[2], length.out = 6)
            },
            labels = function(breaks) {
              if (length(breaks) == 0) return(character(0))
              paste0(seq(0, 100, 20), "%")
            }
          )

        return(p)

      } else if (stats$level == "text" && barcode_granularity == "meta") {
        # Meta group level (existing facet_by_meta behavior)

        # For empty meta groups, add invisible placeholder
        meta_with_hits <- unique(as.character(positions_df$meta))
        meta_without_hits <- setdiff(all_meta_groups, meta_with_hits)

        if (length(meta_without_hits) > 0) {
          empty_facets <- data.frame(
            position = 1,
            doc_length = 1,
            text = NA_character_,
            text_id = NA,
            meta = factor(meta_without_hits, levels = all_meta_groups),
            is_placeholder = TRUE,
            stringsAsFactors = FALSE
          )

          positions_df$is_placeholder <- FALSE
          positions_df <- rbind(positions_df, empty_facets)
        } else {
          positions_df$is_placeholder <- FALSE
        }

        # Create barcode plot
        p <- ggplot2::ggplot(positions_df, ggplot2::aes(x = position, y = 1)) +
          ggplot2::scale_y_continuous(limits = c(0.5, 1.5)) +
          ggplot2::labs(
            title = paste0("Dispersion Plot: \"", stats$search_term, "\" by Meta Group"),
            subtitle = paste0("Total: ", total_occurrences, " occurrences"),
            x = "Position in Text",
            y = ""
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 14, face = "bold"),
            plot.subtitle = ggplot2::element_text(size = 11, color = "gray50"),
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank()
          )

        # Add points
        p <- p + ggplot2::geom_point(
          data = positions_df[!positions_df$is_placeholder, ],
          size = 2, alpha = 0.6, color = "#2E86AB"
        )

        # Facet by meta
        p <- p +
          ggplot2::facet_wrap(~meta, scales = "free_x", ncol = 1, drop = FALSE) +
          ggplot2::theme(
            strip.text = ggplot2::element_text(face = "bold", size = 10),
            strip.background = ggplot2::element_rect(fill = "#e3f2fd", color = NA),
            panel.grid.major.x = ggplot2::element_line(color = "gray80", linetype = "dashed")
          ) +
          ggplot2::scale_x_continuous(
            breaks = function(limits) {
              if (is.na(limits[1]) || is.na(limits[2]) || limits[1] == limits[2]) {
                return(numeric(0))
              }
              seq(limits[1], limits[2], length.out = 6)
            },
            labels = function(breaks) {
              if (length(breaks) == 0) return(character(0))
              paste0(seq(0, 100, 20), "%")
            }
          )

        return(p)

      } else {
        # Whole corpus (single plot - original behavior for barcode_granularity == "corpus")

        positions_df$is_placeholder <- FALSE

        # Check if we should color by meta (user toggle)
        color_by_meta <- input$barcode_color_by_meta %||% TRUE
        has_multiple_groups <- length(unique(positions_df$meta)) > 1

        if (color_by_meta && has_multiple_groups) {
          # Color-coded by meta group WITH JITTER

          # Add jitter position based on meta group
          # Each meta group gets a consistent y-position
          meta_levels <- levels(positions_df$meta)
          n_groups <- length(meta_levels)

          # Create y-positions spread between 0.6 and 1.4
          y_positions <- seq(0.6, 1.4, length.out = n_groups)

          # Assign y-position based on meta group
          positions_df$y_pos <- y_positions[as.numeric(positions_df$meta)]

          p <- ggplot2::ggplot(positions_df, ggplot2::aes(x = position, y = y_pos, color = meta)) +
            ggplot2::geom_point(size = 2.5, alpha = 0.8) +
            ggplot2::scale_y_continuous(limits = c(0.5, 1.5)) +
            ggplot2::scale_color_viridis_d(option = "viridis", name = "Meta Group") +
            ggplot2::labs(
              title = paste0("Dispersion Plot: \"", stats$search_term, "\" across Corpus"),
              subtitle = paste0("Total: ", total_occurrences, " occurrences | Color & position = meta group"),
              x = "Position in Corpus (by text order)",
              y = ""
            ) +
            ggplot2::theme_minimal() +
            ggplot2::theme(
              plot.title = ggplot2::element_text(size = 14, face = "bold"),
              plot.subtitle = ggplot2::element_text(size = 11, color = "gray50"),
              axis.text.y = ggplot2::element_blank(),
              axis.ticks.y = ggplot2::element_blank(),
              panel.grid.major.y = ggplot2::element_blank(),
              panel.grid.minor.y = ggplot2::element_blank(),
              legend.position = "bottom",
              legend.title = ggplot2::element_text(size = 10, face = "bold")
            )
        } else {
          # Single color
          p <- ggplot2::ggplot(positions_df, ggplot2::aes(x = position, y = 1)) +
            ggplot2::geom_point(size = 2, alpha = 0.6, color = "#2E86AB") +
            ggplot2::scale_y_continuous(limits = c(0.5, 1.5)) +
            ggplot2::labs(
              title = paste0("Dispersion Plot: \"", stats$search_term, "\" across Corpus"),
              subtitle = paste0("Total: ", total_occurrences, " occurrences"),
              x = "Position in Corpus (by text order)",
              y = ""
            ) +
            ggplot2::theme_minimal() +
            ggplot2::theme(
              plot.title = ggplot2::element_text(size = 14, face = "bold"),
              plot.subtitle = ggplot2::element_text(size = 11, color = "gray50"),
              axis.text.y = ggplot2::element_blank(),
              axis.ticks.y = ggplot2::element_blank(),
              panel.grid.major.y = ggplot2::element_blank(),
              panel.grid.minor.y = ggplot2::element_blank()
            )
        }

        return(p)
      }
    })

    # Dynamic Height for barcode plot
    output$barcode_plot_ui <- renderUI({
      req(dispersion_results())

      ns <- session$ns

      barcode_granularity <- input$barcode_granularity %||% "corpus"
      stats <- dispersion_results()$stats

      # Calculate height based on granularity
      if (stats$level == "text" && barcode_granularity == "text") {
        # Individual text level
        toks <- active_tokens()
        n_texts <- quanteda::ndoc(toks)

        # Height per text
        if (n_texts <= 5) {
          height <- n_texts * 120
        } else if (n_texts <= 10) {
          height <- n_texts * 80
        } else {
          height <- n_texts * 60
        }

        height <- min(height, 1500)  # Cap at 1500px

      } else if (stats$level == "text" && barcode_granularity == "meta") {
        # Meta group level
        toks <- active_tokens()
        meta_all <- quanteda::docvars(toks, "meta")
        filter_meta <- meta_filter()

        if (!is.null(filter_meta) && length(filter_meta) > 0) {
          keep <- meta_all %in% filter_meta
          meta_all <- meta_all[keep]
        }

        n_facets <- length(unique(meta_all))

        if (n_facets <= 3) {
          height <- n_facets * 100
        } else if (n_facets <= 6) {
          height <- n_facets * 85
        } else {
          height <- n_facets * 70
        }

        height <- min(height, 1200)
      } else {
        # Whole corpus
        height <- 200
      }

      plotOutput(ns("dispersion_barcode"), height = paste0(height, "px"))
    })

    # 7. OUTPUT: HEATMAP & LINE PLOTS ----

    output$dispersion_heatmap <- renderPlot({
      req(dispersion_results())

      # Heatmap only makes sense for meta-level analysis
      if (input$dispersion_level != "meta") {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 1, y = 1,
                                   label = "Heatmap visualization is only available\nwhen analyzing by Meta Groups",
                                   size = 6, color = "gray50") +
                 ggplot2::theme_void())
      }

      results <- dispersion_results()$results
      stats <- dispersion_results()$stats

      # Create heatmap data
      results$rel_freq_scaled <- scale(results$rel_freq_per_1000)[,1]

      p <- ggplot2::ggplot(results, ggplot2::aes(x = 1, y = group, fill = rel_freq_per_1000)) +
        ggplot2::geom_tile(color = "white", size = 1) +
        ggplot2::geom_text(ggplot2::aes(label = frequency), color = "white", fontface = "bold") +
        ggplot2::scale_fill_gradient2(
          low = "#f7fbff",
          mid = "#6baed6",
          high = "#08519c",
          midpoint = median(results$rel_freq_per_1000),
          name = "Freq/1000"
        ) +
        ggplot2::labs(
          title = paste0("Heatmap: \"", stats$search_term, "\" Distribution"),
          subtitle = "Darker colors = higher relative frequency",
          x = "",
          y = "Meta Group"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 14, face = "bold"),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank()
        )

      return(p)
    })

    output$dispersion_line <- renderPlot({
      req(dispersion_results())

      results <- dispersion_results()$results
      stats <- dispersion_results()$stats

      # Check if we should facet by meta
      facet_by_meta <- input$facet_by_meta %||% FALSE

      # Add meta information if analyzing at text level
      if (stats$level == "text" && facet_by_meta) {
        # Get meta information from tokens
        toks <- active_tokens()
        meta_all <- quanteda::docvars(toks, "meta")
        filter_meta <- meta_filter()

        if (!is.null(filter_meta) && length(filter_meta) > 0) {
          keep <- meta_all %in% filter_meta
          meta_all <- meta_all[keep]
        }

        # Match meta to results
        doc_names <- names(toks)
        if (!is.null(filter_meta) && length(filter_meta) > 0) {
          doc_names <- doc_names[keep]
        }

        # Create a lookup for meta groups
        meta_lookup <- setNames(meta_all, doc_names)

        # Add meta column to results
        results$meta_group <- sapply(results$group, function(g) {
          if (g %in% names(meta_lookup)) {
            return(meta_lookup[[g]])
          } else {
            return("Unknown")
          }
        })

        # ENSURE ALL META GROUPS REPRESENTED
        all_meta_groups <- unique(meta_all)
        results$meta_group <- factor(results$meta_group, levels = all_meta_groups)
      }

      # Add position for ordering
      results$position <- seq_len(nrow(results))

      if (stats$level == "text" && facet_by_meta && "meta_group" %in% names(results)) {
        # Faceted line plot
        p <- ggplot2::ggplot(results, ggplot2::aes(x = position, y = rel_freq_per_1000)) +
          ggplot2::geom_line(color = "#2E86AB", size = 1) +
          ggplot2::geom_point(color = "#2E86AB", size = 3) +
          ggplot2::facet_wrap(~meta_group, scales = "free_x", drop = FALSE) +  # CHANGED from "free"
          ggplot2::scale_y_continuous(limits = c(0, NA)) +  # FORCE Y-AXIS TO START AT ZERO
          ggplot2::labs(
            title = paste0("Relative Frequency Trend: \"", stats$search_term, "\" by Meta Group"),
            subtitle = paste0("Overall DP = ", stats$dp, " | Range = ", stats$range, "%"),
            x = "Text",
            y = "Relative Frequency (per 1000 tokens)"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 14, face = "bold"),
            plot.subtitle = ggplot2::element_text(size = 11, color = "gray50"),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 7),
            strip.text = ggplot2::element_text(face = "bold", size = 10),
            strip.background = ggplot2::element_rect(fill = "#e3f2fd", color = NA)
          )
      } else {
        # Single line plot
        p <- ggplot2::ggplot(results, ggplot2::aes(x = position, y = rel_freq_per_1000)) +
          ggplot2::geom_line(color = "#2E86AB", size = 1) +
          ggplot2::geom_point(color = "#2E86AB", size = 3) +
          ggplot2::geom_hline(
            yintercept = mean(results$rel_freq_per_1000),
            linetype = "dashed",
            color = "red",
            alpha = 0.5
          ) +
          ggplot2::scale_y_continuous(limits = c(0, NA)) +  # FORCE Y-AXIS TO START AT ZERO
          ggplot2::labs(
            title = paste0("Relative Frequency Trend: \"", stats$search_term, "\""),
            subtitle = paste0("Red line = mean frequency (",
                             round(mean(results$rel_freq_per_1000), 2), " per 1000)"),
            x = ifelse(stats$level == "meta", "Meta Group", "Text"),
            y = "Relative Frequency (per 1000 tokens)"
          ) +
          ggplot2::scale_x_continuous(
            breaks = results$position,
            labels = results$group
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 14, face = "bold"),
            plot.subtitle = ggplot2::element_text(size = 11, color = "gray50"),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8)
          )
      }

      return(p)
    })

    # 8. DOWNLOAD HANDLERS ----

    output$download_dispersion_csv <- downloadHandler(
      filename = function() {
        paste0("dispersion_analysis_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(dispersion_results())

        results <- dispersion_results()$results
        stats <- dispersion_results()$stats

        # Create header with metadata
        header_info <- data.frame(
          analysis_type = "Lexical Dispersion Analysis",
          search_term = stats$search_term,
          total_occurrences = stats$total_occurrences,
          corpus_size = stats$total_corpus_size,
          gries_dp = ifelse(stats$measure %in% c("dp", "both"), stats$dp, "Not calculated"),
          range_percent = ifelse(stats$measure %in% c("range", "both"), stats$range, "Not calculated"),
          dispersion_level = stats$level,
          n_groups = stats$n_groups,
          analysis_date = as.character(Sys.time()),
          stringsAsFactors = FALSE
        )

        # Write header and data
        readr::write_csv(header_info, file)
        suppressMessages(readr::write_csv(
          data.frame(separator = "--- DISPERSION RESULTS START BELOW ---"),
          file, append = TRUE
        ))
        readr::write_csv(results, file, append = TRUE)
      }
    )

    output$download_dispersion_plot <- downloadHandler(
      filename = function() {
        viz_type <- input$viz_type %||% "bar"
        facet_suffix <- if(input$facet_by_meta %||% FALSE) "_faceted" else ""
        paste0("dispersion_", viz_type, facet_suffix, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(dispersion_results())
        # Logic for plot export would go here...
      }
    )

  })
}