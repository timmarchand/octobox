# =============================================================================
# utils/data_utils.R - Data Processing & Extraction Engine ----
# =============================================================================

library(readr)
library(tools)
library(tibble)

# -----------------------------------------------------------------------------
# 1. TEXT PRE-PROCESSING ----
# -----------------------------------------------------------------------------

#' @description Handles contraction normalization and punctuation-aware spacing.
process_text_for_tokenization_optimized <- function(text, remove_punct = TRUE) {
  start_time <- Sys.time()
  text_processed <- text
  
  # A. Contraction Normalization (Vectorized)
  contraction_patterns <- c(
    "[\u2019\u2018\u0060\u00B4\u2032''`´′]" = "'",
    "([Ww]on)'t"    = "\\1 n't",
    "([Cc]an)'t"    = "\\1 n't",
    "([Ss]han)'t"   = "\\1 n't",
    "([A-Za-z]+)n't" = "\\1 n't",
    "([A-Za-z]+)'ll" = "\\1 'll",
    "([A-Za-z]+)'re" = "\\1 're",
    "([A-Za-z]+)'ve" = "\\1 've",
    "([A-Za-z]+)'d"  = "\\1 'd",
    "([A-Za-z]+)'m"  = "\\1 'm",
    "([A-Za-z]+)'s\\b" = "\\1 's"
  )
  
  for (pattern in names(contraction_patterns)) {
    text_processed <- gsub(pattern, contraction_patterns[pattern], text_processed, perl = TRUE)
  }
  
  # B. Punctuation Padding (If retaining)
  if (!remove_punct) {
    text_processed <- gsub("([.!?;:])([A-Za-z])", "\\1 \\2", text_processed, perl = TRUE)
    text_processed <- gsub("([A-Za-z])([.!?;:])", "\\1 \\2", text_processed, perl = TRUE)
    text_processed <- gsub('(["\'])([A-Za-z])', "\\1 \\2", text_processed, perl = TRUE)
    text_processed <- gsub('([A-Za-z])(["\'])', "\\1 \\2", text_processed, perl = TRUE)
  }
  
  cat("Text processed in:", round(as.numeric(Sys.time() - start_time), 3), "s\n")
  return(text_processed)
}

clean_text_input <- function(text) {
  if (is.null(text) || length(text) == 0) return(character(0))
  text <- trimws(text)
  return(text[nchar(text) > 0])
}

#' @description Splits a single pasted block into separate text units.
#' Paragraphs (separated by one or more blank lines) become individual texts;
#' if there are no blank-line breaks, each non-empty line becomes a text.
#' Returns the standard ingestion shape: list(type, content, metadata).
process_pasted_text <- function(pasted_text) {
  if (is.null(pasted_text) || length(pasted_text) == 0) {
    return(list(type = "paste", content = character(0), metadata = character(0)))
  }
  
  # Normalise line endings, then split on blank lines (paragraph boundaries).
  raw <- paste(pasted_text, collapse = "\n")
  raw <- gsub("\r\n?", "\n", raw)
  units <- strsplit(raw, "\n[[:space:]]*\n+", perl = TRUE)[[1]]
  
  # Fall back to line-by-line if there were no blank-line separators.
  if (length(units) <= 1) {
    units <- strsplit(raw, "\n", fixed = TRUE)[[1]]
  }
  
  # Collapse internal newlines within a unit to single spaces, then clean.
  units <- gsub("[[:space:]]*\n[[:space:]]*", " ", units, perl = TRUE)
  content <- clean_text_input(units)
  
  if (length(content) == 0) {
    return(list(type = "paste", content = character(0), metadata = character(0)))
  }
  
  list(
    type = "paste",
    content = content,
    metadata = rep("pasted", length(content))
  )
}

#' @description Build a stopword vector for filtering tokens.
#' @param language one of "en","es","fr","de","it","ja","zh".
#' @param include_contractions if TRUE, also add common contraction fragments
#'   ("n't","'s","'re",...) so split contractions are removed too.
#' @param custom_stopwords newline-separated extra words from the user.
#' @param mode "add" = base list + custom; "replace" = custom only.
#' @return character vector of lowercased stopwords.
create_stopword_list <- function(language = "en",
                                 include_contractions = TRUE,
                                 custom_stopwords = "",
                                 mode = "add") {
  
  # Parse custom words (one per line), trimmed and de-blanked.
  custom <- character(0)
  if (!is.null(custom_stopwords) && nzchar(trimws(custom_stopwords))) {
    custom <- trimws(strsplit(custom_stopwords, "\r?\n")[[1]])
    custom <- custom[nzchar(custom)]
  }
  
  # "replace" mode: ignore the base list entirely, use only custom words.
  if (identical(mode, "replace")) {
    return(unique(tolower(custom)))
  }
  
  # Base list from quanteda. snowball covers en/es/fr/de/it; ja/zh need the
  # stopwords-iso source, which may not be installed - fall back gracefully.
  base <- tryCatch({
    if (language %in% c("en", "es", "fr", "de", "it")) {
      quanteda::stopwords(language, source = "snowball")
    } else {
      quanteda::stopwords(language, source = "stopwords-iso")
    }
  }, error = function(e) {
    warning(sprintf("No stopword list for '%s' (%s); using English.", language, e$message))
    tryCatch(quanteda::stopwords("en", source = "snowball"),
             error = function(e2) character(0))
  })
  
  # Contraction fragments produced by the tokenizer's normalisation step.
  contractions <- character(0)
  if (isTRUE(include_contractions)) {
    contractions <- c("n't", "'s", "'re", "'ve", "'ll", "'d", "'m",
                      "na", "ta")  # gonna/gotta fragments
  }
  
  unique(tolower(c(base, contractions, custom)))
}

# -----------------------------------------------------------------------------
# 2. FILE INGESTION LOGIC ----
# -----------------------------------------------------------------------------

# In utils/data_utils.R (or wherever read_uploaded_file is defined):
read_uploaded_file <- function(file_info, skip_rows = 0) {  # ← Add skip_rows parameter
  tryCatch({
    file_ext <- tools::file_ext(file_info$name)
    
    if (file_ext == "csv") {
      content <- readr::read_csv(
        file_info$datapath,
        skip = skip_rows,  # ← Add this
        show_col_types = FALSE
      )
      return(list(type = "csv", content = content))
      
    } else if (file_ext == "txt") {
      content <- readLines(file_info$datapath)
      return(list(type = "txt", content = content, metadata = rep("unknown", length(content))))
      
    } else {
      return(NULL)
    }
  }, error = function(e) {
    showNotification(paste("Error reading file:", e$message), type = "error")
    return(NULL)
  })
}

read_corpus_files <- function(file_input, metadata_assignments = NULL) {
  if (is.null(file_input) || nrow(file_input) == 0) return(NULL)
  
  all_content  <- character(0)
  all_metadata <- character(0)
  
  for (i in 1:nrow(file_input)) {
    tryCatch({
      content <- paste(readLines(file_input$datapath[i], warn=F, encoding="UTF-8"), collapse=" ")
      meta <- if(!is.null(metadata_assignments)) metadata_assignments[i] else tools::file_path_sans_ext(file_input$name[i])
      
      all_content  <- c(all_content, trimws(content))
      all_metadata <- c(all_metadata, meta)
    }, error = function(e) next)
  }
  return(list(type="corpus", content=all_content, metadata=all_metadata))
}

# -----------------------------------------------------------------------------
# 3. KWIC EXTRACTION ENGINE (REVISED) ----
# -----------------------------------------------------------------------------

#' @description Custom KWIC extractor. Handles both separated and concatenated views.
quick_conc <- function(tokens, index, n = 5, separated = TRUE, use_regex = FALSE) {
  if (length(tokens) == 0) return(tibble::tibble())
  tokens  <- as.character(tokens)
  
  # Document-boundary sentinel (inserted by the caller between texts). Windows
  # must not cross it, and it must never match or appear in context.
  BOUNDARY <- "\u0001DOCBREAK\u0001"
  
  if (use_regex) {
    matches <- grep(index, tokens, ignore.case = TRUE, perl = TRUE)
  } else {
    # For exact match, use tolower on both sides
    matches <- which(tolower(tokens) == tolower(index))
  }
  # Never treat a boundary sentinel as a match.
  matches <- matches[tokens[matches] != BOUNDARY]
  
  if (length(matches) == 0) return(tibble::tibble())
  
  results <- list()
  for (i in seq_along(matches)) {
    m_pos <- matches[i]
    
    # Clamp the window so it can't cross a boundary sentinel on either side.
    left_limit  <- max(1, m_pos - n)
    right_limit <- min(length(tokens), m_pos + n)
    
    if (m_pos > 1L) {
      left_seg <- tokens[left_limit:(m_pos - 1L)]
      lb <- which(left_seg == BOUNDARY)
      if (length(lb) > 0) left_limit <- left_limit + max(lb)  # just after last boundary
    }
    if (m_pos < length(tokens)) {
      right_seg <- tokens[(m_pos + 1L):right_limit]
      rb <- which(right_seg == BOUNDARY)
      if (length(rb) > 0) right_limit <- m_pos + min(rb) - 1L  # just before first boundary
    }
    
    start <- left_limit
    end   <- right_limit
    
    if (separated) {
      row <- list(token_id = m_pos)
      # Left Context (guard against empty/reversed range at a boundary edge).
      left_t <- if (start <= (m_pos - 1L)) tokens[start:(m_pos - 1L)] else character(0)
      left_t <- left_t[left_t != BOUNDARY]
      if(length(left_t) > 0) {
        for(j in seq_along(left_t)) row[[paste0("left", length(left_t)-j+1)]] <- left_t[j]
      }
      row[["match"]] <- tokens[m_pos]
      # Right Context
      right_t <- if ((m_pos + 1L) <= end) tokens[(m_pos + 1L):end] else character(0)
      right_t <- right_t[right_t != BOUNDARY]
      if(length(right_t) > 0) {
        for(j in seq_along(right_t)) row[[paste0("right", j)]] <- right_t[j]
      }
      results[[i]] <- row
    } else {
      left_t  <- if (start <= (m_pos - 1L)) tokens[start:(m_pos - 1L)] else character(0)
      left_t  <- left_t[left_t != BOUNDARY]
      right_t <- if ((m_pos + 1L) <= end) tokens[(m_pos + 1L):end] else character(0)
      right_t <- right_t[right_t != BOUNDARY]
      results[[i]] <- list(
        token_id = m_pos,
        pre      = paste(left_t, collapse = " "),
        keyword  = tokens[m_pos],
        post     = paste(right_t, collapse = " ")
      )
    }
  }
  
  # Column Ordering Logic
  if (length(results) > 0) {
    if (separated) {
      all_cols   <- unique(unlist(lapply(results, names)))
      left_cols  <- sort(all_cols[grepl("^left", all_cols)], decreasing = TRUE)
      right_cols <- sort(all_cols[grepl("^right", all_cols)])
      ordered    <- c("token_id", left_cols, "match", right_cols)
      
      df <- do.call(rbind, lapply(results, function(r) {
        for(c in setdiff(ordered, names(r))) r[[c]] <- NA_character_
        as.data.frame(r[ordered], stringsAsFactors=F)
      }))
    } else {
      df <- do.call(rbind, lapply(results, as.data.frame, stringsAsFactors=F))
      df <- df[, c("token_id", "pre", "keyword", "post")]
    }
    return(tibble::as_tibble(df))
  }
  return(tibble::tibble())
}

# -----------------------------------------------------------------------------
# 4. UTILITIES & MEMORY ----
# -----------------------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

validate_uploaded_file <- function(file_input, max_size_mb = 50) {
  if (is.null(file_input)) return(list(valid=F, message="No file"))
  sz <- file.size(file_input$datapath) / (1024^2)
  if (sz > max_size_mb) return(list(valid=F, message="File too large"))
  return(list(valid=TRUE, message="OK"))
}

cleanup_large_objects <- function(threshold_mb = 100) {
  objs <- ls(envir = .GlobalEnv)
  for (o in objs) {
    if (grepl("^(temp_|cache_)", o)) {
      if (as.numeric(object.size(get(o))) / 1024^2 > threshold_mb) rm(list=o, envir=.GlobalEnv)
    }
  }
  gc()
}

cat("✓ Data utilities engine online.\n")