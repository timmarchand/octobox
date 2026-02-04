# =============================================================================
# modules/ui_tokenization.R - UI Components ----
# =============================================================================

tokenizationUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # 1. Header & Context -----------------------------------------------------
    div(
      title = "Convert your text into tokens (individual words) for analysis",
      p("Tokenization breaks your text into individual words and prepares it for analysis.", 
        style = "font-size: 13px; color: #6c757d; margin-bottom: 15px;")
    ),
    
    # 2. Information & Performance --------------------------------------------
    div(
      style = "background-color: #e3f2fd; border: 1px solid #bbdefb; padding: 10px; border-radius: 4px; margin-bottom: 15px;",
      HTML("<strong>💡 Performance Tip:</strong><br/>
            <small>
            • Contractions are processed automatically<br/>
            • Large corpora (>50k words) use optimized processing<br/>
            • Results are cached for repeated use
            </small>")
    ),
    
    # 3. Tokenization Configuration -------------------------------------------
    div(
      style = "margin-bottom: 15px; padding: 10px; border: 1px solid #f0f0f0; border-radius: 4px;",
      checkboxInput(ns("remove_punct"), "Remove Punctuation", value = TRUE),
      checkboxInput(ns("remove_numbers"), "Remove Numbers", value = FALSE),
      checkboxInput(ns("to_lower"), "Convert to Lowercase", value = TRUE)
    ),
    
    # 3.1. Conditional Warning (Punctuation) ----
    conditionalPanel(
      condition = sprintf("input['%s'] == false", ns("remove_punct")),
      div(
        style = "background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 8px; border-radius: 4px; margin-bottom: 15px; font-size: 12px;",
        HTML("<strong>📝 Note:</strong> Punctuation (., , ? !) will be retained and classified in the 01k frequency band.")
      )
    ),
    
    # 4. Action Execution -----------------------------------------------------
    div(
      style = "text-align: center; margin: 20px 0;",
      actionButton(
        ns("run_tokenize"), 
        "⚙️ Run Tokenization", 
        class = "btn-success btn-lg",
        style = "font-weight: bold; width: 100%; box-shadow: 0 2px 4px rgba(0,0,0,0.1);"
      )
    ),
    
    # 5. Post-Processing / Downloads ------------------------------------------
    
    # Only visible if 'tokenization_complete' is TRUE
    conditionalPanel(
      condition = sprintf("output['%s']", ns("tokenization_complete")),
      div(
        style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
        h6("📥 Download Tokenized Data", style = "margin-top: 0; color: #28a745;"),
        
        fluidRow(
          column(6,
                 div(title = "Download tokens with metadata (CSV)",
                     downloadButton(ns("download_tokens_csv"), "📊 CSV", 
                                    class = "btn-outline-success btn-block btn-sm"))
          ),
          column(6,
                 div(title = "Download raw tokens (TXT)",
                     downloadButton(ns("download_tokens_txt"), "📄 TXT", 
                                    class = "btn-outline-secondary btn-block btn-sm"))
          )
        ),
        
        # Format Legend
        div(
          style = "margin-top: 12px; padding: 8px; background-color: #e9ecef; border-radius: 3px; font-size: 11px; color: #495057;",
          HTML("<strong>CSV:</strong> token, doc_id, meta, position<br/>
                <strong>TXT:</strong> list of all tokens (one per line)")
        )
      )
    ),
    
    # 6. Status Output --------------------------------------------------------
    conditionalPanel(
      condition = sprintf("output['%s']", ns("tokenization_status")),
      div(
        style = "margin-top: 15px; border-left: 4px solid #28a745;",
        verbatimTextOutput(ns("tokenization_status"))
      )
    )
  )
}