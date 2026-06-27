# =============================================================================
# FIXED FREQUENCY SERVER - Optimized Logic & Caching --------------------------
# =============================================================================

library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(data.table)
library(digest)
library(stringr)

# 1. STOPWORD HELPER ----------------------------------------------------------
get_stopwords_cached <- function(language) {
  if (exists("STOPWORDS_CACHE", envir = .GlobalEnv)) {
    cache <- get("STOPWORDS_CACHE", envir = .GlobalEnv)
    if (!is.null(cache[[language]])) return(cache[[language]])
  }
  return(quanteda::stopwords(language))
}

frequencyServer <- function(id, token_data, meta_filter_global, values, tagged_data = NULL) {
  moduleServer(id, function(input, output, session) {

    # 2. SOURCE SELECTION -----------------------------------------------------
    output$tagged_available <- reactive({
      !is.null(tagged_data) && !is.null(tagged_data()) && tagged_data()$available
    })
    outputOptions(output, "tagged_available", suspendWhenHidden = FALSE)
    
    active_tokens <- reactive({
      if (input$use_tagged_tokens %||% FALSE) {
        req(tagged_data(), tagged_data()$df)
        # Internal helper to bridge tagged DF back to quanteda-style tokens
        convert_tagged_to_tokens(tagged_data()$df, input$tag_column %||% "xpos")
      } else {
        token_data()
      }
    })

    # 3. OPTIMIZATION: INDEXED LOOKUPS ----------------------------------------
    frequency_indexes <- reactive({
      req(values$unified_freq_df)
      cat("Building optimized frequency indexes...\n")

      freq_dt <- as.data.table(values$unified_freq_df)
      freq_dt[, token_lower := tolower(as.character(token))]
      setkey(freq_dt, token_lower, tokenRank)
      freq_indexed <- freq_dt[, .SD[1], by = token_lower]

      # Return list of hash tables for O(1) lookups
      list(
        dt = freq_indexed,
        hash_tokenband = setNames(as.character(freq_indexed$tokenBand), freq_indexed$token_lower),
        hash_headband  = setNames(as.character(freq_indexed$headBand), freq_indexed$token_lower),
        hash_pos       = setNames(as.character(freq_indexed$PoS), freq_indexed$token_lower),
        hash_headword  = setNames(as.character(freq_indexed$headword), freq_indexed$token_lower),
        hash_tokenrank = setNames(freq_indexed$tokenRank, freq_indexed$token_lower),
        hash_tokenfreq = setNames(freq_indexed$tokenFreq, freq_indexed$token_lower)
      )
    })

    # 4. OPTIMIZATION: RESULT CACHING -----------------------------------------
    result_cache <- list()
    max_cache_size <- 50

    generate_cache_key <- function(...) {
      digest::digest(list(..., meta_filter = meta_filter_global()))
    }

    get_cached_result <- function(key) {
      if (key %in% names(result_cache)) {
        cat("Cache HIT!\n")
        return(result_cache[[key]])
      }
      return(NULL)
    }

    # 5. CORE FREQUENCY ENGINE ------------------------------------------------
    ngram_result <- eventReactive(input$run_ngram, {
      req(active_tokens())
      
      # Determine cache settings
      stopword_settings <- if (input$use_stopwords) {
        list(lang = input$stopword_language, custom = input$custom_stopwords)
      } else NULL

      cache_key <- generate_cache_key(
        n = input$ngram_n, view = input$ngram_view, lex = input$include_lexical_info,
        ranges = input$selected_ranges, sw = stopword_settings, 
        tagged = input$use_tagged_tokens, col = input$tag_column
      )

      cached <- get_cached_result(cache_key)
      if (!is.null(cached)) return(cached)

      withProgress(message = 'Running analysis...', value = 0, {
        toks <- active_tokens()
        
        # Stopword Filtering ----
        if (input$use_stopwords) {
          sw <- create_stopword_list(
            input$stopword_language, input$include_contractions, 
            input$custom_stopwords, input$custom_stopword_mode
          )
          toks <- quanteda::tokens_remove(toks, sw, case_insensitive = TRUE)
        }

        # N-Gram Generation ----
        if ((input$ngram_n %||% 1) > 1) {
          toks <- quanteda::tokens_ngrams(toks, n = input$ngram_n)
        }

        # DFM Construction ----
        dfm_obj <- quanteda::dfm(toks)
        view_type <- input$ngram_view %||% "meta"
        
        if (view_type == "meta") {
          dfm_obj <- quanteda::dfm_group(dfm_obj, groups = quanteda::docvars(toks, "meta"))
        }

        # Tabulation ----
        res <- quanteda.textstats::textstat_frequency(dfm_obj, groups = quanteda::docnames(dfm_obj)) %>%
          rename(token = feature, count = frequency, meta = group)

        # Lexical Enrichment ----
        if (input$include_lexical_info && (input$ngram_n %||% 1) == 1) {
          res <- fast_frequency_lookup(res, frequency_indexes(), input$freq_list_type, input$selected_ranges)
        }

        result_cache[[cache_key]] <<- res
        return(res)
      })
    })

    # 6. CHARTING & EXPORT ----------------------------------------------------
    output$ngram_result <- DT::renderDT({
      req(ngram_result())
      # Simplified view for UI performance
      DT::datatable(head(ngram_result(), 5000), options = list(scrollX = TRUE, pageLength = 25))
    })

    output$download_stopwords <- downloadHandler(
      filename = function() { paste0("stopwords_", Sys.Date(), ".txt") },
      content = function(file) {
        sw <- create_stopword_list(input$stopword_language, TRUE, input$custom_stopwords, "add")
        writeLines(sw, file)
      }
    )

    # 6.1. Download complete results as CSV ----
    output$download_ngram_csv <- downloadHandler(
      filename = function() {
        paste0("frequency_analysis_", Sys.Date(), ".csv")
      },
      content = function(file) {
        res <- ngram_result()
        if (is.null(res) || nrow(res) == 0) {
          readr::write_csv(data.frame(message = "No results to export"), file)
        } else {
          readr::write_csv(res, file)
        }
      }
    )

    # 6.2. Chart generation flag ----
    # Flips TRUE when the user clicks "Generate Charts" and there are results;
    # the conditionalPanels in the UI watch `charts_available`.
    charts_ready <- reactiveVal(FALSE)

    # Re-running the analysis invalidates any previously generated charts.
    observeEvent(ngram_result(), {
      charts_ready(FALSE)
    }, ignoreNULL = FALSE)

    observeEvent(input$generate_charts, {
      res <- ngram_result()
      if (is.null(res) || nrow(res) == 0) {
        showNotification("Run an analysis first - there are no results to chart.",
                         type = "warning")
        charts_ready(FALSE)
      } else {
        charts_ready(TRUE)
      }
    })

    output$charts_available <- reactive({
      isTRUE(charts_ready())
    })
    outputOptions(output, "charts_available", suspendWhenHidden = FALSE)

    # 6.3. Token frequency chart ----
    output$test_plot <- renderPlot({
      req(charts_ready())
      res <- ngram_result()
      req(!is.null(res), nrow(res) > 0)

      top_n <- input$chart_top_n %||% 15
      use_prop <- identical(input$chart_yaxis %||% "count", "proportion")
      has_meta <- "meta" %in% names(res) && dplyr::n_distinct(res$meta) > 1

      y_lab <- if (use_prop) "Proportion of group tokens" else "Frequency"

      if (has_meta) {
        # Per-group totals for normalisation (group = meta).
        group_totals <- res %>%
          group_by(meta) %>%
          summarise(group_total = sum(count), .groups = "drop")

        plot_df <- res %>%
          group_by(meta, token) %>%
          summarise(count = sum(count), .groups = "drop") %>%
          left_join(group_totals, by = "meta") %>%
          mutate(value = if (use_prop) count / group_total else count) %>%
          group_by(meta) %>%
          slice_max(value, n = top_n, with_ties = FALSE) %>%
          ungroup()

        # reorder-within: per-facet ordered label. Levels are built in
        # facet+value order so each panel sorts its own tokens.
        plot_df <- plot_df %>%
          arrange(meta, value) %>%
          mutate(token_ord = factor(paste(token, meta, sep = "___"),
                                    levels = paste(token, meta, sep = "___")))

        # The token (discrete) axis is ALWAYS free so each panel shows only its
        # own tokens - otherwise every facet draws all tokens with empty bars.
        # The value axis is controlled separately by the user toggle below.
        p <- ggplot(plot_df, aes(x = token_ord, y = value, fill = meta)) +
          geom_col(show.legend = FALSE) +
          coord_flip() +
          facet_wrap(~ meta, scales = "free") +
          scale_x_discrete(labels = function(x) sub("___.*$", "", x)) +
          scale_fill_brewer(palette = "Dark2") +
          labs(x = NULL, y = y_lab,
               title = paste("Top", top_n, "tokens per group",
                             if (use_prop) "(proportion)" else "(counts)")) +
          theme_minimal(base_size = 14)

        # Value-axis scaling: "Fixed" (or any proportion comparison) shares a
        # common value range across panels so bar heights are comparable;
        # "Free" lets each panel scale to its own max.
        want_fixed_value <- use_prop || identical(input$chart_facet_scales %||% "free", "fixed")
        if (want_fixed_value) {
          vmax <- max(plot_df$value, na.rm = TRUE)
          p <- p + expand_limits(y = c(0, vmax))
        }

        if (use_prop) p <- p + scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))
        p
      } else {
        total <- sum(res$count)
        plot_df <- res %>%
          group_by(token) %>%
          summarise(count = sum(count), .groups = "drop") %>%
          mutate(value = if (use_prop) count / total else count) %>%
          arrange(desc(value)) %>%
          head(top_n)

        p <- ggplot(plot_df, aes(x = reorder(token, value), y = value)) +
          geom_col(fill = "#2e7d32") +
          coord_flip() +
          labs(x = NULL, y = y_lab,
               title = paste("Top", nrow(plot_df),
                             if (use_prop) "by proportion" else "by frequency")) +
          theme_minimal(base_size = 14)

        if (use_prop) p <- p + scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))
        p
      }
    })

    # 6.4. Frequency-band distribution chart ----
    output$freq_band_plot <- renderPlot({
      req(charts_ready())
      res <- ngram_result()
      req(!is.null(res), nrow(res) > 0)

      if (!"tokenBand" %in% names(res)) {
        return(
          ggplot() +
            annotate("text", x = 1, y = 1,
                     label = "Enable 'Include Frequency Info' to see frequency bands",
                     size = 5) +
            theme_void()
        )
      }

      has_meta <- "meta" %in% names(res) && dplyr::n_distinct(res$meta) > 1
      band_order <- c("01k", "02k", "03k", "04k", "05k", "06k", "07k", "08k", "09k", "10k")

      # Keep only recognised bands, in canonical order.
      band_df <- res %>%
        filter(!is.na(tokenBand), tokenBand != "", tokenBand != "other") %>%
        filter(tokenBand %in% band_order)

      if (nrow(band_df) == 0) {
        return(
          ggplot() +
            annotate("text", x = 1, y = 1,
                     label = "No recognised frequency bands (01k-10k) in these results.",
                     size = 5) +
            theme_void()
        )
      }

      # Percentage per band within each group, plus cumulative coverage.
      if (has_meta) {
        band_df <- band_df %>%
          group_by(meta, band = tokenBand) %>%
          summarise(count = sum(count), .groups = "drop") %>%
          mutate(band = factor(band, levels = band_order)) %>%
          group_by(meta) %>%
          arrange(band, .by_group = TRUE) %>%
          mutate(total = sum(count),
                 percentage = (count / total) * 100,
                 cumulative = cumsum(percentage)) %>%
          ungroup()
      } else {
        band_df <- band_df %>%
          group_by(band = tokenBand) %>%
          summarise(count = sum(count), .groups = "drop") %>%
          mutate(band = factor(band, levels = band_order)) %>%
          arrange(band) %>%
          mutate(total = sum(count),
                 percentage = (count / total) * 100,
                 cumulative = cumsum(percentage))
      }

      if (has_meta) {
        ggplot(band_df, aes(x = band, group = meta)) +
          geom_col(aes(y = percentage, fill = meta),
                   position = "dodge", alpha = 0.75, width = 0.7) +
          geom_line(aes(y = cumulative, color = meta), linewidth = 1.1) +
          geom_point(aes(y = cumulative, color = meta), size = 2.3) +
          labs(title = "Frequency band distribution",
               subtitle = "Bars: % of tokens in each band  |  Lines: cumulative coverage",
               x = "Frequency band (Zipf-based)", y = "Percentage", fill = "Group", color = "Group") +
          scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(0, 100, 20)) +
          scale_fill_brewer(palette = "Dark2") +
          scale_color_brewer(palette = "Dark2") +
          theme_minimal(base_size = 14) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "bottom")
      } else {
        ggplot(band_df, aes(x = band, group = 1)) +
          geom_col(aes(y = percentage), fill = "#1565c0", alpha = 0.75, width = 0.7) +
          geom_line(aes(y = cumulative), color = "#c0392b", linewidth = 1.1) +
          geom_point(aes(y = cumulative), color = "#c0392b", size = 2.3) +
          labs(title = "Frequency band distribution",
               subtitle = "Bars: % of tokens in each band  |  Line: cumulative coverage",
               x = "Frequency band (Zipf-based)", y = "Percentage") +
          scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(0, 100, 20)) +
          theme_minimal(base_size = 14) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
    })
  })
}