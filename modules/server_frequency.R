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
  })
}