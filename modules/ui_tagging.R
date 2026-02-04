# =============================================================================
# modules/ui_tagging.R - POS Tagging Module UI ----
# =============================================================================

taggingUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # 1. Header & Title ----
    h4("Part-of-Speech Tagging"),
    
    # 2. Information Panel ----
    div(
      style = "background-color: #e7f3ff; border: 1px solid #b3d9ff; padding: 12px; border-radius: 5px; margin-bottom: 15px;",
      HTML("<strong>ℹ️ About POS Tagging:</strong><br/>
            This tool annotates your corpus with part-of-speech tags using the UDPipe model.<br/>
            Each word is tagged with its grammatical category (noun, verb, adjective, etc.).<br/>
            <strong>Note:</strong> Tagging is resource-intensive. Limit corpus size to ~20,000 words.")
    ),
    
    # 3. Enable Tagging Control ----
    div(
      style = "margin-bottom: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px;",
      checkboxInput(ns("enable_tagging"),
                    "Enable POS Tagging (loads 15.6 MB language model)",
                    value = FALSE),
      
      # Warning for model load time
      conditionalPanel(
        condition = paste0("input['", ns("enable_tagging"), "'] == true"),
        div(
          style = "margin-top: 10px; padding: 10px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 4px;",
          HTML("<strong>⚠️ Model Loading:</strong> The first time you enable tagging, 
                it will take ~30 seconds to load the language model. Please be patient!")
        )
      )
    ),
    
    # 4. Tagging Configuration (Conditional) ----
    conditionalPanel(
      condition = paste0("input['", ns("enable_tagging"), "'] == true"),
      
      # Tag system selection (XPOS vs UPOS)
      div(
        style = "margin-bottom: 15px;",
        radioButtons(ns("tag_format"),
                     "Tag format:",
                     choices = list(
                       "XPOS (Penn Treebank style: NN, VB, JJ, etc.)" = "xpos",
                       "UPOS (Universal: NOUN, VERB, ADJ, etc.)" = "upos"
                     ),
                     selected = "xpos",
                     inline = FALSE)
      ),
      
      # Visual output format
      div(
        style = "margin-bottom: 15px;",
        radioButtons(ns("output_format"),
                     "Output format:",
                     choices = list(
                       "Inline (word_TAG word_TAG...)" = "inline",
                       "Vertical (one token per line)" = "vertical",
                       "Table (structured data)" = "table"
                     ),
                     selected = "inline",
                     inline = FALSE)
      ),
      
      # Execution Action
      div(
        style = "text-align: center; margin: 20px 0;",
        actionButton(ns("run_tagger"), 
                     "🏷️ Tag Corpus", 
                     class = "btn-primary btn-lg",
                     style = "width: 100%;")
      ),
      
      # 5. Results & Display Section ----
      conditionalPanel(
        condition = paste0("output['", ns("has_results"), "']"),
        
        hr(),
        
        # Summary statistics component
        uiOutput(ns("tagging_summary")),
        
        # Export option
        div(
          style = "text-align: center; margin: 15px 0;",
          downloadButton(ns("download_tagged"), 
                         "Download Tagged Corpus",
                         class = "btn-success",
                         style = "width: 100%;")
        ),
        
        # Preview Area
        h5("Tagged Output Preview", style = "margin-top: 20px;"),
        
        # Dynamic display based on output format
        conditionalPanel(
          condition = paste0("input['", ns("output_format"), "'] == 'table'"),
          DT::DTOutput(ns("tagged_table"))
        ),
        
        conditionalPanel(
          condition = paste0("input['", ns("output_format"), "'] != 'table'"),
          div(
            style = "max-height: 500px; overflow-y: auto; background-color: #f8f9fa; 
                     border: 1px solid #dee2e6; border-radius: 5px; padding: 15px;",
            verbatimTextOutput(ns("tagged_text"))
          )
        )
      )
    )
  )
}