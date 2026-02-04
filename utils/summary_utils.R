# =============================================================================
# utils/summary_utils.R - Token Summary Analysis ----
# =============================================================================
# Purpose: Vocabulary richness (TTR) with optimized random sampling.
# Reference: Jarvis (2002), Zenker & Kyle (2021).
# =============================================================================

library(shiny)
library(dplyr)
library(DT)
library(quanteda)
library(quanteda.textstats)

# -----------------------------------------------------------------------------
# 1. HELPER: CAPPED SAMPLED TTR CALCULATION ----
# -----------------------------------------------------------------------------

calculate_sampled_ttr <- function(doc_tokens, window_size = 400, max_samples = 200) {
  doc_length <- length(doc_tokens)

  if (doc_length < window_size) {
    return(list(ttr = length(unique(doc_tokens)) / doc_length, samples = 0))
  }

  # Calculate maximum possible windows
  max_possible_windows <- doc_length - window_size + 1

  # Cap at max_samples (default 200)
  n_samples <- min(max_possible_windows, max_samples)

  # Use RANDOM sampling instead of exhaustive overlapping (much faster)
  sample_starts <- if (n_samples == max_possible_windows) {
    1:n_samples  # Small text - use all windows
  } else {
    sample(1:max_possible_windows, size = n_samples, replace = FALSE)  # Random sample
  }

  # Calculate TTR for each sampled window
  window_ttrs <- vapply(sample_starts, function(start) {
    window_tokens <- doc_tokens[start:(start + window_size - 1)]
    length(unique(window_tokens)) / window_size
  }, numeric(1))

  return(list(
    ttr = mean(window_ttrs),
    samples = n_samples,
    sd = sd(window_ttrs)
  ))
}

# -----------------------------------------------------------------------------
# 2. SERVER MODULE: TOKEN SUMMARY ----
# -----------------------------------------------------------------------------

tokenSummaryServer <- function(id, token_data, meta_filter) {
  moduleServer(id, function(input, output, session) {

    cat("Token summary server loaded and connected\n")

    # 2.1. Reactive: Calculation Logic ----
    summary_data <- reactive({
      req(token_data())

      tokens <- token_data()
      if (is.null(tokens) || !quanteda::is.tokens(tokens)) {
        return(NULL)
      }

      # Get view type from the correct input ID
      view_type <- input$summary_view %||% "meta"

      withProgress(message = 'Calculating corpus statistics...', value = 0, {

        incProgress(0.2, detail = "Processing tokens...")

        # Get metadata
        meta_info <- quanteda::docvars(tokens, "meta")
        if (is.null(meta_info)) {
          meta_info <- rep("unknown", quanteda::ndoc(tokens))
        }

        # Apply meta filter if available
        filter_meta <- meta_filter()
        if (!is.null(filter_meta) && length(filter_meta) > 0) {
          keep <- meta_info %in% filter_meta
          tokens <- tokens[keep]
          meta_info <- meta_info[keep]
        }

        incProgress(0.4, detail = "Calculating basic statistics...")

        # Calculate basic statistics
        doc_names <- names(tokens)
        n_tokens <- quanteda::ntoken(tokens)
        n_types <- quanteda::ntype(tokens)

        incProgress(0.6, detail = "Computing TTR measures...")

        # Calculate traditional TTR
        ttr_traditional <- n_types / n_tokens
        ttr_traditional[is.nan(ttr_traditional) | is.infinite(ttr_traditional)] <- 0

        # OPTIMIZED: Calculate sampled TTR with capped random sampling
        sampled_ttr_results <- lapply(seq_along(tokens), function(i) {
          doc_tokens <- as.character(tokens[[i]])
          calculate_sampled_ttr(doc_tokens, window_size = 400, max_samples = 200)
        })

        # FIXED: Extract values more robustly
        sampled_ttr_values <- vapply(sampled_ttr_results, function(x) {
          if (is.list(x) && "ttr" %in% names(x) && is.numeric(x$ttr)) {
            return(x$ttr)
          } else {
            return(0)
          }
        }, numeric(1))

        sampled_ttr_samples <- vapply(sampled_ttr_results, function(x) {
          if (is.list(x) && "samples" %in% names(x) && is.numeric(x$samples)) {
            return(as.integer(x$samples))
          } else {
            return(0L)
          }
        }, integer(1))


        incProgress(0.8, detail = "Organizing results...")

        # Create base data frame
        base_df <- data.frame(
          document = doc_names,
          meta = meta_info,
          tokens = n_tokens,
          types = n_types,
          ttr = round(ttr_traditional, 4),
          sampled_ttr = round(sampled_ttr_values, 4),
          ttr_samples = sampled_ttr_samples,
          stringsAsFactors = FALSE
        )

        # 2.2. View Logic: Grouping ----
        if (view_type == "corpus") {
          all_tokens_combined <- unlist(as.list(tokens))
          total_tokens <- length(all_tokens_combined)
          total_types <- length(unique(all_tokens_combined))

          if (total_tokens >= 400) {
            corpus_result <- calculate_sampled_ttr(all_tokens_combined,
                                                   window_size = 400,
                                                   max_samples = 200)
            corpus_sampled_ttr <- corpus_result$ttr
            corpus_ttr_samples <- corpus_result$samples
          } else {
            corpus_sampled_ttr <- total_types / total_tokens
            corpus_ttr_samples <- 0
          }

          result_df <- data.frame(
            category = "Whole Corpus",
            texts = nrow(base_df),
            tokens = total_tokens,
            types = total_types,
            ttr = round(total_types / total_tokens, 4),
            sampled_ttr = round(corpus_sampled_ttr, 4),
            ttr_samples = corpus_ttr_samples,
            stringsAsFactors = FALSE
          )

        } else if (view_type == "meta") {
          result_list <- list()

          for (meta_group in unique(meta_info)) {
            group_docs <- which(meta_info == meta_group)
            group_tokens_list <- tokens[group_docs]
            group_tokens_combined <- unlist(as.list(group_tokens_list))

            group_total_tokens <- length(group_tokens_combined)
            group_total_types <- length(unique(group_tokens_combined))
            group_ttr <- group_total_types / group_total_tokens

            if (group_total_tokens >= 400) {
              group_result <- calculate_sampled_ttr(group_tokens_combined,
                                                    window_size = 400,
                                                    max_samples = 200)
              group_sampled_ttr <- group_result$ttr
              group_ttr_samples <- group_result$samples
            } else {
              group_sampled_ttr <- group_ttr
              group_ttr_samples <- 0
            }

            result_list[[meta_group]] <- data.frame(
              category = meta_group,
              texts = length(group_docs),
              tokens = group_total_tokens,
              types = group_total_types,
              ttr = round(group_ttr, 4),
              sampled_ttr = round(group_sampled_ttr, 4),
              ttr_samples = group_ttr_samples,
              stringsAsFactors = FALSE
            )
          }
          result_df <- do.call(rbind, result_list)
          rownames(result_df) <- NULL

        } else {
          result_df <- base_df %>%
            mutate(category = paste0(meta, " - ", document)) %>%
            select(category, tokens, types, ttr, sampled_ttr, ttr_samples) %>%
            mutate(texts = 1) %>%
            select(category, texts, tokens, types, ttr, sampled_ttr, ttr_samples)
        }

        incProgress(1, detail = "Complete!")
        result_df$sampled_ttr[is.nan(result_df$sampled_ttr)] <- 0
        return(result_df)
      })
    })

    # 2.3. Output: DT Rendering ----
    output$token_summary <- DT::renderDT({
      data <- summary_data()

      if (is.null(data) || nrow(data) == 0) {
        return(DT::datatable(
          data.frame(Message = "No summary data available. Please run tokenization first."),
          options = list(pageLength = 10, searching = FALSE, paging = FALSE, info = FALSE)
        ))
      }

      DT::datatable(
        data,
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          searching = TRUE,
          ordering = TRUE,
          columnDefs = list(
            list(className = "dt-right", targets = c(1, 2, 3, 4, 5, 6)),
            list(className = "dt-center", targets = 0)
          )
        ),
        colnames = c("Category", "Texts", "Tokens", "Types", "TTR", "Sampled TTR-400", "TTR Samples"),
        caption = htmltools::tags$caption(
          style = "caption-side: top; text-align: left; font-size: 12px; margin-bottom: 10px;",
          paste("Corpus summary statistics showing vocabulary richness measures.",
                "TTR = Type-Token Ratio. Sampled TTR uses random 400-word windows (max 200 samples) for length normalization.")
        )
      ) %>%
        DT::formatStyle(columns = c("ttr", "sampled_ttr"), backgroundColor = "#f8f9fa", fontWeight = "bold") %>%
        DT::formatStyle(columns = "tokens", backgroundColor = "#e3f2fd")
    })

    # 2.4. Output: Download Handler ----
    output$download_token_summary <- downloadHandler(
      filename = function() {
        view_type <- input$summary_view %||% "meta"
        paste0("corpus_summary_", view_type, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        data <- summary_data()
        if (is.null(data) || nrow(data) == 0) {
          write.csv(data.frame(message = "No summary data available"), file, row.names = FALSE)
          return()
        }

        view_type <- input$summary_view %||% "meta"
        header_info <- data.frame(
          analysis_type = "Corpus Summary Statistics",
          analysis_date = as.character(Sys.time()),
          summary_view = view_type,
          total_categories = nrow(data),
          notes = "TTR = Type-Token Ratio. Sampled TTR uses random 400-word windows (capped at 200 samples).",
          explanation_ttr = "Traditional TTR = unique words / total words",
          explanation_sampled_ttr = "Sampled TTR = average TTR from random 400-word windows (max 200 samples, length-normalized)",
          explanation_samples = "Number of 400-word windows sampled",
          methodology = "Random sampling approach: robust with fast computation.",
          literature_reference = "Based on Jarvis (2002), Zenker & Kyle (2021)",
          stringsAsFactors = FALSE
        )

        readr::write_csv(header_info, file)
        suppressMessages(readr::write_csv(data.frame(separator = "--- SUMMARY DATA STARTS BELOW ---"), file, append = TRUE))
        readr::write_csv(data, file, append = TRUE)
      }
    )

    return(list(summary_data = summary_data))
  })
}

# -----------------------------------------------------------------------------
# 3. EXTRA HELPER FUNCTIONS ----
# -----------------------------------------------------------------------------

format_number <- function(x) {
  ifelse(x >= 1000, paste0(round(x/1000, 1), "k"), as.character(x))
}

calculate_corpus_diversity <- function(tokens) {
  all_tokens <- unlist(as.list(tokens))
  total_tokens <- length(all_tokens)
  unique_tokens <- length(unique(all_tokens))

  list(
    total_tokens = total_tokens,
    unique_tokens = unique_tokens,
    ttr = unique_tokens / total_tokens,
    log_ttr = log(unique_tokens) / log(total_tokens)
  )
}

# -----------------------------------------------------------------------------
# 4. INITIALIZATION ----
# -----------------------------------------------------------------------------
cat("Summary utilities loaded successfully!\n")
cat("✓ Optimized TTR calculation using random sampling (capped at 200 samples)\n")