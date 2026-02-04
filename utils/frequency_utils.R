# =============================================================================
# utils/frequency_utils.R - Enhanced Frequency Lookup & Number Handling
# =============================================================================

library(data.table)
library(dplyr)
library(memoise)
library(digest)

# -----------------------------------------------------------------------------
# 1. CORE ENGINE: Cached Frequency Lookup ----
# -----------------------------------------------------------------------------

#' @description Performs high-speed join between tokens and freq database.
#' @note Caches results based on tokens_hash to prevent redundant joins.
get_frequency_info_cached <- memoise(function(tokens_hash, final_result, 
                                             freq_list_type, selected_ranges, 
                                             unified_freq_df, pos_examples_df = NULL) {
  
  if (is.null(unified_freq_df) || nrow(final_result) == 0) return(final_result)

  # 1.1. Prepare Lookup Table ----
  freq_lookup <- unified_freq_df %>%
    filter(!is.na(token), token != "", token != " ") %>%
    mutate(token = tolower(as.character(token))) %>%
    group_by(token) %>%
    slice_min(tokenRank, n = 1, with_ties = FALSE) %>%
    ungroup()

  # 1.2. Numeric Detection Logic ----
  final_result_clean <- final_result %>%
    mutate(
      token_lower = tolower(as.character(token)),
      is_numeric_token = grepl("^\\d+$", token_lower) | grepl("^\\d+\\.\\d+$", token_lower)
    )

  # 1.3. The Join & Mutation ----
  result <- final_result_clean %>%
    left_join(freq_lookup, by = c("token_lower" = "token")) %>%
    mutate(
      # Assign numbers to the 01k band and 'Num' PoS automatically
      tokenBand = case_when(
        is_numeric_token & (is.na(tokenBand) | tokenBand == "") ~ "01k",
        !is.na(tokenBand) ~ as.character(tokenBand),
        TRUE ~ NA_character_
      ),
      PoS = case_when(
        is_numeric_token & (is.na(PoS) | PoS == "") ~ "Num",
        !is.na(PoS) ~ as.character(PoS),
        TRUE ~ "Unclassified"
      ),
      # Set high-frequency stats for numbers (Rank 1, 1M freq)
      tokenRank = if_else(is_numeric_token & (is.na(tokenRank) | tokenRank == 0), 1L, as.integer(tokenRank %||% 999999L)),
      tokenFreq = if_else(is_numeric_token & (is.na(tokenFreq) | tokenFreq == 0), 1000000L, as.integer(tokenFreq %||% 0L))
    )

  # 1.4. Lexical Range Grouping ----
  result <- result %>%
    mutate(
      original_band = if_else(freq_list_type == "token", as.character(tokenBand), as.character(headBand)),
      grouped_band = case_when(
        is.null(selected_ranges) ~ "other",
        "all_bands" %in% selected_ranges ~ original_band,
        is_numeric_token & original_band == "01k" & "01k" %in% selected_ranges ~ "01k",
        original_band %in% selected_ranges ~ original_band,
        original_band %in% paste0(11:20, "k") & "11k-20k" %in% selected_ranges ~ "11k-20k",
        original_band %in% c(paste0(21:25, "k"), "30k", "35k", "40k", "45k", "50k") & "21k-50k" %in% selected_ranges ~ "21k-50k",
        TRUE ~ "other"
      ),
      tokenBand = grouped_band,
      headBand  = grouped_band
    ) %>%
    select(-token_lower, -original_band, -grouped_band, -is_numeric_token)

  # 1.5. PoS Examples Integration ----
  if (!is.null(pos_examples_df) && "PoS" %in% names(result)) {
    result <- result %>%
      left_join(pos_examples_df, by = "PoS") %>%
      mutate(category = category %||% "Unclassified", examples = examples %||% "")
  }

  return(result)
})

# -----------------------------------------------------------------------------
# 2. VALIDATION & TESTING ----
# -----------------------------------------------------------------------------

validate_wordfreq_structure <- function(df) {
  required <- c("token", "headword", "tokenRank", "headRank", "tokenBand", "headBand", "tokenFreq", "headFreq", "PoS")
  missing  <- setdiff(required, names(df))
  
  if (length(missing) > 0) {
    warning("Critical failure: wordFreq.csv is missing: ", paste(missing, collapse = ", "))
    return(FALSE)
  }
  return(TRUE)
}

test_number_detection <- function() {
  test_tokens <- c("the", "4", "10.5", "word", "0")
  data.frame(token = test_tokens) %>%
    mutate(is_numeric = grepl("^\\d+(\\.\\d+)?$", token))
}

# -----------------------------------------------------------------------------
# 3. BAND HELPERS & EXPORT ----
# -----------------------------------------------------------------------------

get_standard_band_order <- function() {
  c("01k", "02k", "03k", "04k", "05k", "06k", "07k", "08k", "09k", "10k", 
    "11k-20k", "21k-50k", "other")
}

write_enhanced_csv_with_metadata <- function(final_result, file_path, pos_examples_df = NULL, analysis_type = "Freq") {
  # Header metadata
  meta <- data.frame(type = analysis_type, date = as.character(Sys.time()), numeric_handling = "Numbers auto-assigned 01k/Num")
  readr::write_csv(meta, file_path)
  
  # Inject PoS Metadata if exists
  if (!is.null(pos_examples_df)) {
    readr::write_csv(data.frame(sep="--PoS Metadata--"), file_path, append=T)
    readr::write_csv(pos_examples_df, file_path, append=T)
  }
  
  readr::write_csv(data.frame(sep="--Results--"), file_path, append=T)
  readr::write_csv(final_result, file_path, append=T)
}

cat("✓ Frequency & Number Handling Utilities online.\n")