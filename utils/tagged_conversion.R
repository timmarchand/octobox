# =============================================================================
# utils/tagged_conversion.R - Tagged DF to Tokens -----------------------------
# =============================================================================

convert_tagged_to_tokens <- function(tagged_df, tag_column = "xpos") {
  
  # 1. Logic Branching: Text Construction -------------------------------------
  # This section builds the raw text strings before tokenization
  
  if (tag_column == "pos_only") {
    # Method: POS Tags Only ----
    tagged_text <- tagged_df %>%
      dplyr::group_by(doc_id) %>%
      dplyr::summarize(
        text = paste(xpos, collapse = " "),
        meta = dplyr::first(meta),
        .groups = "drop"
      )
    
  } else if (tag_column == "lemma") {
    # Method: Lemma + Tag (e.g., eat_VB) ----
    tagged_text <- tagged_df %>%
      dplyr::group_by(doc_id) %>%
      dplyr::summarize(
        text = paste0(lemma, "_", xpos, collapse = " "),
        meta = dplyr::first(meta),
        .groups = "drop"
      )
    
  } else {
    # Method: Word + Tag (e.g., eating_VBG) ----
    tag_col <- if (tag_column == "upos") "upos" else "xpos"
    
    tagged_text <- tagged_df %>%
      dplyr::group_by(doc_id) %>%
      dplyr::summarize(
        text = paste0(token, "_", .data[[tag_col]], collapse = " "),
        meta = dplyr::first(meta),
        .groups = "drop"
      )
  }
  
  # 2. Quanteda Integration ---------------------------------------------------
  # Converting the summarized text back into a token object
  
  # Create Corpus Object ----
  corp <- quanteda::corpus(tagged_text, text_field = "text")
  
  # Tokenize with fastestword ----
  # Note: fastestword preserves our underscore joins
  toks <- quanteda::tokens(corp, what = "fastestword")
  
  # Re-attach Metadata ----
  quanteda::docvars(toks, "meta") <- tagged_text$meta
  
  return(toks)
}