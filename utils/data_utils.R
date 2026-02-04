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
   if (use_regex) {
    matches <- grep(index, tokens, ignore.case = TRUE, perl = TRUE)
  } else {
    # For exact match, use tolower on both sides
    matches <- which(tolower(tokens) == tolower(index))
  }
  
  if (length(matches) == 0) return(tibble::tibble())

  results <- list()
  for (i in seq_along(matches)) {
    m_pos <- matches[i]
    start <- max(1, m_pos - n)
    end   <- min(length(tokens), m_pos + n)
    
    if (separated) {
      row <- list(token_id = m_pos)
      # Left Context
      left_t <- tokens[start:(m_pos-1)]
      if(length(left_t) > 0) {
        for(j in seq_along(left_t)) row[[paste0("left", length(left_t)-j+1)]] <- left_t[j]
      }
      row[["match"]] <- tokens[m_pos]
      # Right Context
      right_t <- tokens[(m_pos+1):end]
      if(length(right_t) > 0) {
        for(j in seq_along(right_t)) row[[paste0("right", j)]] <- right_t[j]
      }
      results[[i]] <- row
    } else {
      results[[i]] <- list(
        token_id = m_pos,
        pre      = paste(tokens[start:(m_pos-1)], collapse = " "),
        keyword  = tokens[m_pos],
        post     = paste(tokens[(m_pos+1):end], collapse = " ")
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