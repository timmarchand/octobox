# =============================================================================
# KWIC AND COLLOCATION MODULES
# =============================================================================

# KWIC Server Module ----------------------------------------------------------

# At the top of the function signature:
kwicServer <- function(id, token_data, meta_filter, tagged_data = NULL) {
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
        
        cat("Using TAGGED tokens for KWIC analysis\n")
        convert_tagged_to_tokens(tagged_data()$df, input$tag_column %||% "xpos")
      } else {
        cat("Using REGULAR tokens for KWIC analysis\n")
        token_data()
      }
    })
    
    # ... rest of your KWIC server code ...
    # Replace all instances of active_tokens() with active_tokens()
    
    # 1. KWIC Computation ----
    # 1. KWIC Computation ----
    # Flat token stream from the last concordance run (with boundary sentinels),
    # used by collocation to count each collocate token once even when node
    # windows overlap. Defined before result_data so it exists when set.
    kwic_flat <- reactiveVal(NULL)
    
    result_data <- eventReactive(input$run, {
      req(active_tokens())
      
      cat("\n=== KWIC FULL DIAGNOSTIC ===\n")
      
      if (is.null(active_tokens())) {
        cat("ERROR: active_tokens() is NULL\n")
        showNotification("No tokenized data available.", type = "error")
        return(tibble::tibble())
      }
      
      withProgress(message = 'Running KWIC analysis...', value = 0, {
        toks <- active_tokens()
        tokens_list <- as.list(toks)
        meta_all <- quanteda::docvars(toks, "meta")
        
        cat("Tokens loaded. Docs:", length(tokens_list), "\n")
        
        # Pull inputs
        index <- input$index
        n <- input$n
        separated <- input$separated
        
        cat("Search term (index):", index, "\n")
        cat("Context window (n):", n, "\n")
        cat("Separated:", separated, "\n")
        
        incProgress(0.3, detail = "Applying filters...")
        filter_meta <- meta_filter()
        keep <- if (is.null(filter_meta)) rep(TRUE, length(meta_all)) else meta_all %in% filter_meta
        
        tokens <- tokens_list[keep]
        meta <- meta_all[keep]
        
        cat("After filtering - docs:", length(tokens), "\n")
        
        if (length(tokens) == 0) {
          cat("ERROR: No documents after filtering\n")
          showNotification("No data available after filtering.", type = "error")
          return(tibble::tibble())
        }
        
        incProgress(0.6, detail = "Processing concordances...")
        
        # Define flat_tokens FIRST.
        # Insert a boundary sentinel between documents so concordance windows
        # can't bleed across document boundaries (which would over-count
        # collocates for a node near the start/end of a text).
        BOUNDARY <- "\u0001DOCBREAK\u0001"
        tokens_bounded <- lapply(tokens, function(t) c(as.character(t), BOUNDARY))
        flat_tokens <- unlist(tokens_bounded)
        # token_meta must align with flat_tokens (including the sentinel slots).
        token_meta <- rep(meta, lengths(tokens) + 1L)
        
        cat("Flat tokens created. Total:", length(flat_tokens), "\n")
        cat("First 20 tokens:", paste(head(flat_tokens, 20), collapse = " | "), "\n")
        
        # Smart pattern matching for tagged tokens
        is_tagged_search <- input$use_tagged_tokens %||% FALSE
        
        cat("Is tagged search:", is_tagged_search, "\n")
        
        if (is_tagged_search) {
          tag_col <- input$tag_column %||% "xpos"
          
          if (tag_col %in% c("xpos_only", "upos_only", "pos_only")) {
            # POS-only mode: tokens are bare tags (e.g. "NN", "NOUN").
            # The search term IS the whole token - match it directly, but
            # still allow regex if the user ticked it (e.g. "N.*").
            search_pattern <- index
            use_regex <- input$use_regex %||% FALSE
          } else if (startsWith(index, "_")) {
            search_pattern <- paste0("\\w+", index)
            use_regex <- TRUE
          } else if (grepl("_", index)) {
            search_pattern <- index
            use_regex <- FALSE
          } else {
            search_pattern <- paste0(index, "_\\w+")
            use_regex <- TRUE
          }
        } else {
          search_pattern <- index
          use_regex <- input$use_regex %||% FALSE
        }
        
        cat("Final search pattern:", search_pattern, "\n")
        cat("Use regex:", use_regex, "\n")
        
        incProgress(0.8, detail = "Generating results...")
        
        # Call quick_conc
        result <- quick_conc(
          flat_tokens, 
          index = search_pattern,
          n = n, 
          separated = separated,
          use_regex = use_regex
        )
        
        cat("Result from quick_conc - rows:", nrow(result), "\n")
        if (nrow(result) > 0) {
          cat("Result columns:", paste(names(result), collapse = ", "), "\n")
        }
        
        # Add meta information
        if (nrow(result) > 0) {
          result$meta <- token_meta[result$token_id]
          cat("Meta added successfully\n")
        }
        
        cat("=== END DIAGNOSTIC ===\n\n")
        
        incProgress(1, detail = "Complete!")
        kwic_flat(flat_tokens)   # expose flat stream for collocation dedup
        return(result)
      })
    })
    
    
    # 2. UI State Management ----
    output$has_results <- reactive({
      res <- tryCatch(result_data(), error = function(e) NULL)
      !is.null(res) && nrow(res) > 0
    })
    outputOptions(output, "has_results", suspendWhenHidden = FALSE)
    
    # 3. Dynamic Column Selection ----
    observe({
      df <- tryCatch(result_data(), error = function(e) NULL)
      req(df, nrow(df) > 0, input$separated)
      
      # Identify and sort position columns
      position_cols <- names(df)[grepl("^(left|right)\\d+$", names(df))]
      left_cols  <- sort(position_cols[grepl("^left", position_cols)], decreasing = TRUE)
      right_cols <- sort(position_cols[grepl("^right", position_cols)])
      
      final_choices <- c(left_cols, "match", right_cols)
      
      # Preserve the user's current column choice; only default to "match"
      # when their previous choice is no longer available.
      current <- isolate(input$count_column)
      selected <- if (!is.null(current) && current %in% final_choices) current else "match"
      
      updateSelectInput(session, "count_column",
                        choices = setNames(final_choices, final_choices),
                        selected = selected)
    })
    
    # Return Reactives ----
    return(list(
      result_data = result_data,
      separated   = reactive({ input$separated }),
      kwic_flat   = kwic_flat,
      has_results = reactive({
        res <- tryCatch(result_data(), error = function(e) NULL)
        !is.null(res) && nrow(res) > 0
      })
    ))
  })
}

# KWIC Results Server Module --------------------------------------------------
#' @param id Module ID
#' @param kwic_return The list of reactives returned by kwicServer
kwicResultsServer <- function(id, kwic_return) {
  moduleServer(id, function(input, output, session) {
    
    # Extract Reactives
    result_data <- kwic_return$result_data
    separated   <- kwic_return$separated
    kwic_counted_results <- reactiveVal(NULL)
    
    # Clear any applied position-counting result when a NEW search runs.
    # Otherwise the display (which prefers kwic_counted_results over
    # result_data) keeps showing the previous search's counted table and new
    # searches appear not to register.
    observeEvent(result_data(), {
      kwic_counted_results(NULL)
    }, ignoreNULL = FALSE)
    
    
    output$has_results <- reactive({
      data <- result_data()
      has_data <- !is.null(data) && nrow(data) > 0
      cat("has_results reactive - returning:", has_data, "\n")
      return(has_data)
    })
    outputOptions(output, "has_results", suspendWhenHidden = FALSE)
    
    # 1. Multi-Column Counting Logic ----
    observe({
      req(input$enable_counting)
      df <- result_data()
      req(df, nrow(df) > 0)
      
      position_cols <- names(df)[grepl("^(left|right)\\d+$", names(df))]
      other_cols <- intersect(c("match", "meta"), names(df))
      all_countable <- c(position_cols, other_cols)
      
      updateCheckboxGroupInput(session, "count_columns",
                               choices = setNames(all_countable, all_countable),
                               selected = NULL)
    })
    
    # 2. Selection Helpers ----
    observeEvent(input$select_all_positions, {
      req(result_data())
      cols <- names(result_data())
      updateCheckboxGroupInput(session, "count_columns", 
                               selected = cols[grepl("^(left|right)\\d+$|^match$", cols)])
    })
    
    observeEvent(input$clear_position_selection, {
      updateCheckboxGroupInput(session, "count_columns", selected = character(0))
    })
    
    # 3. Apply Counting Calculation ----
    observeEvent(input$apply_counting, {
      req(result_data(), input$count_columns)
      
      withProgress(message = 'Calculating frequencies...', value = 0, {
        df <- result_data()
        
        # Build combination string
        selected_data <- df[, input$count_columns, drop = FALSE]
        selected_data[] <- lapply(selected_data, function(x) {
          ifelse(is.na(x) | x == "" | x == " ", "(empty)", as.character(x))
        })
        
        combos <- apply(selected_data, 1, function(row) {
          paste(paste(names(selected_data), row, sep = ":"), collapse = " | ")
        })
        
        freq_table <- table(combos)
        
        # Sort and Filter
        df$count_by_sort <- as.numeric(freq_table[combos])
        df$combination_value <- combos
        
        # Apply sorting logic based on input$sort_method...
        # (Logic omitted for brevity, assuming standard sort implementation)
        
        kwic_counted_results(df)
        incProgress(1)
      })
    })
    
    # 4. Data Table Rendering ----
    output$result <- DT::renderDT({
      display_data <- kwic_counted_results() %||% result_data()
      req(display_data, nrow(display_data) > 0)
      
      # Formatting for UI
      display_df <- display_data %>% select(-any_of(c("case", "token_id")))
      
      # Highlight the match column and count column
      dt <- DT::datatable(display_df, options = list(scrollX = TRUE, pageLength = 25))
      
      if ("match" %in% names(display_df)) {
        dt <- dt %>% DT::formatStyle("match", backgroundColor = "#fff9c4", fontWeight = "bold")
      }
      return(dt)
    })
    
    # 5. Download Handler ----
    output$download_csv <- downloadHandler(
      filename = function() paste0("kwic_export_", Sys.Date(), ".csv"),
      content = function(file) {
        data_to_save <- kwic_counted_results() %||% result_data()
        readr::write_csv(data_to_save, file)
      }
    )
  })
}

# Collocation Server Module ---------------------------------------------------
#' @param id Module ID
#' @param kwic_results The list of reactives returned by kwicServer
#' @param token_data reactive returning the corpus quanteda tokens (for corpus
#'   frequencies and total size N).
#' @param meta_filter reactive returning the active meta filter (or NULL).
collocationServer <- function(id, kwic_results, token_data = NULL, meta_filter = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Dynamic UI Updates ----
    # Populate the available position choices when KWIC results change.
    # IMPORTANT: preserve the user's current selection - only fall back to the
    # left1/right1 default when nothing valid is currently selected. Otherwise
    # this observer re-fires and overwrites the user's choices every time.
    observe({
      req(kwic_results$has_results())
      df <- kwic_results$result_data()
      pos_cols <- names(df)[grepl("^(left|right)\\d+$", names(df))]
      
      current <- isolate(input$analysis_positions)
      still_valid <- intersect(current, pos_cols)
      selected <- if (length(still_valid) > 0) still_valid else intersect(c("left1", "right1"), pos_cols)
      
      updateCheckboxGroupInput(session, "analysis_positions",
                               choices = pos_cols, selected = selected)
    })
    
    # Position-selection shortcut buttons.
    pos_cols_current <- reactive({
      req(kwic_results$has_results())
      df <- kwic_results$result_data()
      names(df)[grepl("^(left|right)\\d+$", names(df))]
    })
    observeEvent(input$select_left, {
      cols <- pos_cols_current()
      updateCheckboxGroupInput(session, "analysis_positions",
                               selected = cols[grepl("^left", cols)])
    })
    observeEvent(input$select_right, {
      cols <- pos_cols_current()
      updateCheckboxGroupInput(session, "analysis_positions",
                               selected = cols[grepl("^right", cols)])
    })
    observeEvent(input$select_immediate, {
      cols <- pos_cols_current()
      updateCheckboxGroupInput(session, "analysis_positions",
                               selected = intersect(c("left1", "right1"), cols))
    })
    
    # Corpus frequency table + total size, computed once per token set.
    corpus_freq <- reactive({
      req(token_data)
      toks <- token_data()
      req(!is.null(toks))
      
      # Honour the active meta filter so collocation stats match the KWIC scope.
      if (!is.null(meta_filter)) {
        fm <- meta_filter()
        if (!is.null(fm) && length(fm) > 0) {
          meta_all <- quanteda::docvars(toks, "meta")
          toks <- toks[meta_all %in% fm]
        }
      }
      
      flat <- tolower(unlist(quanteda::as.list(toks)))
      list(
        freq = table(flat),
        N = length(flat)
      )
    })
    
    # 2. Statistical Analysis ----
    collocation_results <- eventReactive(input$run_analysis, {
      
      # Diagnose silent stops with a visible message instead of req() aborting.
      if (is.null(kwic_results$has_results()) || !isTRUE(kwic_results$has_results())) {
        showNotification("Run the Concordancer first - no KWIC results to analyse.",
                         type = "warning", duration = 6)
        return(NULL)
      }
      if (is.null(input$analysis_positions) || length(input$analysis_positions) == 0) {
        showNotification("Select at least one position (e.g. left1, right1) to analyse.",
                         type = "warning", duration = 6)
        return(NULL)
      }
      
      withProgress(message = 'Computing Collocations...', value = 0, {
        df <- kwic_results$result_data()
        positions <- input$analysis_positions
        positions <- positions[positions %in% names(df)]
        if (length(positions) == 0) {
          showNotification("Selected positions aren't present in the KWIC table - re-run the Concordancer with 'Separate Columns' on.",
                           type = "warning", duration = 8)
          return(NULL)
        }
        
        incProgress(0.2, detail = "Harvesting collocates...")
        
        cf <- corpus_freq()
        N <- cf$N
        corpus_counts <- cf$freq
        node_freq <- nrow(df)
        span <- length(positions)
        
        # Per-position counts: a named-vector count table for each position,
        # used both for O11 (summed) and for the position columns.
        pos_tabs <- lapply(positions, function(pos) {
          toks <- tolower(df[[pos]])
          toks <- toks[!is.na(toks) & toks != "" & toks != "_"]
          table(toks)
        })
        names(pos_tabs) <- positions
        
        # Full set of collocate types across all selected positions.
        words <- sort(unique(unlist(lapply(pos_tabs, names))))
        req(length(words) > 0)
        
        # O11 = total collocate count across the whole selected window.
        pos_counts <- sapply(positions, function(pos) {
          tb <- pos_tabs[[pos]]
          out <- as.numeric(tb[words]); out[is.na(out)] <- 0
          out
        })
        if (is.null(dim(pos_counts))) pos_counts <- matrix(pos_counts, nrow = length(words))
        colnames(pos_counts) <- positions
        # Raw per-position counts kept for the display columns (directionality).
        O11_raw <- rowSums(pos_counts)
        
        # Deduped O11 for the STATISTICS: count each collocate corpus token at
        # most once, even if it falls inside two overlapping node windows.
        O11 <- O11_raw  # fallback if flat stream unavailable or dedupe fails
        flat <- if (!is.null(kwic_results$kwic_flat)) kwic_results$kwic_flat() else NULL
        node_ids <- suppressWarnings(as.integer(df$token_id))
        node_ids <- node_ids[!is.na(node_ids)]
        O11_dedup <- tryCatch({
          if (is.null(flat) || length(node_ids) == 0) {
            NULL
          } else {
            offset_of <- function(p) {
              num <- as.integer(gsub("[^0-9]", "", p))
              if (grepl("^left", p)) -num else num
            }
            occ <- list()
            BND <- "\u0001DOCBREAK\u0001"
            for (p in positions) {
              off <- offset_of(p)
              if (is.na(off)) next
              targets <- node_ids + off
              valid <- targets >= 1 & targets <= length(flat)
              crosses <- vapply(node_ids, function(a) {
                b <- a + off
                if (b < 1 || b > length(flat)) return(TRUE)
                lo <- min(a, b); hi <- max(a, b)
                if (hi - lo <= 1) return(FALSE)
                any(flat[(lo + 1):(hi - 1)] == BND)
              }, logical(1))
              valid <- valid & !crosses
              abs_pos <- targets[valid]
              if (length(abs_pos) == 0) next
              toks_here <- tolower(flat[abs_pos])
              keep <- !is.na(toks_here) & toks_here != "" & toks_here != "_" &
                toks_here != BND
              abs_pos <- abs_pos[keep]; toks_here <- toks_here[keep]
              for (w in unique(toks_here)) {
                occ[[w]] <- c(occ[[w]], abs_pos[toks_here == w])
              }
            }
            vapply(words, function(w) {
              v <- occ[[w]]
              if (is.null(v)) 0L else length(unique(v))
            }, integer(1))
          }
        }, error = function(e) {
          message("Collocation dedupe failed, using raw window counts: ", e$message)
          NULL
        })
        if (!is.null(O11_dedup) && length(O11_dedup) == length(words)) {
          O11 <- O11_dedup
        }
        
        incProgress(0.5, detail = "Calculating window-level association measures...")
        
        # C1: collocate's total corpus frequency.
        C1 <- as.numeric(corpus_counts[words]); C1[is.na(C1)] <- 0
        
        # Consistent token model: everything below is in corpus tokens.
        #   R1  = node_freq * span   (number of window-token slots)
        #   E11 = R1 * C1 / N
        R1  <- node_freq * span
        E11 <- (R1 * C1) / N
        E11[E11 <= 0] <- NA_real_
        
        MI       <- log2(O11 / E11)
        t_score  <- (O11 - E11) / sqrt(O11)
        log_dice <- 14 + log2((2 * O11) / (R1 + C1))
        
        # Log-likelihood (G2) over a coherent 2x2 token table:
        #   row 1 = window slots (R1), row 2 = rest of corpus (N - R1)
        #   col 1 = collocate (C1), col 2 = everything else
        ll <- mapply(function(o11, c1) {
          if (is.na(c1) || c1 == 0) return(NA_real_)
          o12 <- R1 - o11
          o21 <- c1 - o11
          o22 <- N - R1 - o21
          if (o22 < 0) return(NA_real_)   # incoherent table -> report NA, no clamp
          obs <- c(o11, o12, o21, o22)
          r1 <- o11 + o12; r2 <- o21 + o22
          c1c <- o11 + o21; c2c <- o12 + o22
          exp <- c(r1 * c1c, r1 * c2c, r2 * c1c, r2 * c2c) / N
          terms <- mapply(function(o, e) if (o > 0 && e > 0) o * log(o / e) else 0, obs, exp)
          2 * sum(terms)
        }, O11, C1)
        
        # Assemble: stats first, then one count column per selected position.
        result <- data.frame(
          collocate      = words,
          freq_in_window = O11,
          corpus_freq    = C1,
          MI             = round(MI, 3),
          t_score        = round(t_score, 3),
          log_likelihood = round(ll, 3),
          logDice        = round(log_dice, 3),
          stringsAsFactors = FALSE
        )
        pos_df <- as.data.frame(pos_counts, stringsAsFactors = FALSE)
        names(pos_df) <- positions          # e.g. left1, right1
        result <- cbind(result, pos_df)
        
        # Minimum frequency filter (total co-occurrence count in window).
        min_freq <- input$min_freq %||% 1
        result <- result[result$freq_in_window >= min_freq, , drop = FALSE]
        req(nrow(result) > 0)
        
        # Sort by the chosen statistic (falls back to log-likelihood).
        sort_col <- switch(input$stat_measure %||% "Log-Likelihood",
                           "Log-Likelihood"     = "log_likelihood",
                           "Mutual Information"  = "MI",
                           "T-Score"             = "t_score",
                           "Raw Frequency"       = "freq_in_window",
                           "log_likelihood")
        result <- result[order(-result[[sort_col]]), , drop = FALSE]
        
        # Keep top N.
        top_n <- input$top_n %||% 20
        if (!is.na(top_n) && top_n > 0 && nrow(result) > top_n) {
          result <- result[seq_len(top_n), , drop = FALSE]
        }
        rownames(result) <- NULL
        
        incProgress(1, detail = "Done")
        result
      })
    })
    
    output$has_collocation_results <- reactive({
      res <- tryCatch(collocation_results(), error = function(e) NULL)
      !is.null(res) && nrow(res) > 0
    })
    outputOptions(output, "has_collocation_results", suspendWhenHidden = FALSE)
    
    # 3. Rendering ----
    output$collocation_table <- DT::renderDT({
      req(collocation_results())
      DT::datatable(collocation_results(), options = list(pageLength = 15)) %>%
        DT::formatStyle("log_likelihood", backgroundColor = DT::styleInterval(3.84, c("white", "#d4edda")))
    })
    
    # 4. Download ----
    output$download_collocations <- downloadHandler(
      filename = function() paste0("collocations_", Sys.Date(), ".csv"),
      content = function(file) {
        res <- collocation_results()
        if (is.null(res) || nrow(res) == 0) {
          readr::write_csv(data.frame(message = "No collocation results"), file)
        } else {
          readr::write_csv(res, file)
        }
      }
    )
  })
}