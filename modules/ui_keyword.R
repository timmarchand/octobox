# =============================================================================
# KEYWORD ANALYSIS UI MODULE
# =============================================================================

#' Keyword Analysis UI
#' @description UI for Log-Odds Keyword Analysis with target/reference selection
#' @param id Module ID
keywordUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("Log-Odds Keyword Analysis"),
    p("Identify distinctive n-grams by comparing target group(s) against reference groups."),
    
    # 0. Tagged Data Options (The "Blue Box") - ONLY SHOWS AFTER TAGGING ----
    conditionalPanel(
      condition = paste0("output['", ns("tagged_available"), "']"),
      div(
        style = "background-color: #e7f3ff; border: 1px solid #b3d9ff; padding: 12px; border-radius: 5px; margin-bottom: 15px;",
        h5("🏷️ Tagged Corpus Available", style = "margin-top: 0; color: #0056b3;"),
        
        checkboxInput(ns("use_tagged_tokens"),
                      "Analyze tagged corpus for keywords",
                      value = FALSE),
        
        conditionalPanel(
          condition = paste0("input['", ns("use_tagged_tokens"), "'] == true"),
          
          radioButtons(ns("tag_column"),
                       "Tag analysis type:",
                       choices = list(
                         "Words with XPOS tags (word_NN, word_VB)" = "xpos",
                         "Words with UPOS tags (word_NOUN, word_VERB)" = "upos",
                         "POS tags only (NN, VB, JJ)" = "pos_only",
                         "Lemmas with tags (lemma_NN, lemma_VB)" = "lemma"
                       ),
                       selected = "xpos"),
          
          div(
            style = "font-size: 12px; color: #6c757d; margin-top: 5px;",
            HTML("<strong>💡 Use Case:</strong><br/>
                   • Find keywords that are specific <strong>nouns</strong> (NN, NNS)<br/>
                   • Identify distinctive <strong>verbs</strong> (VB, VBG, VBD)<br/>
                   • Compare <strong>adjective</strong> usage (JJ, JJR)<br/>
                   • Discover lemma-based keywords (base forms)")
          )
        )
      )
    ),
    
    # 1. Group Selection Section
    ## Target Group Selection
    div(
      style = "margin-bottom: 15px; padding: 12px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
      h6("🎯 Target Group Selection", style = "margin-top: 0; color: #495057;"),
      p("Select one or more target groups to analyze.", 
        style = "font-size: 13px; color: #6c757d; margin-bottom: 10px;"),
      
      checkboxGroupInput(ns("keyword_target"), 
                         "Select Target Group(s):", 
                         choices = NULL),
      
      div(
        style = "margin-top: 8px;",
        actionButton(ns("select_all_targets"), "Select All Available", 
                     class = "btn-outline-success btn-sm", style = "margin-right: 5px;"),
        actionButton(ns("clear_targets"), "Clear Selection", 
                     class = "btn-outline-secondary btn-sm")
      )
    ),
    
    ## Reference Group Selection
    div(
      style = "margin-bottom: 15px; padding: 12px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
      h6("📚 Reference Group Selection", style = "margin-top: 0; color: #495057;"),
      p("Select reference groups to represent the 'baseline' for comparison.", 
        style = "font-size: 13px; color: #6c757d; margin-bottom: 10px;"),
      
      div(
        style = "margin-bottom: 10px;",
        actionButton(ns("select_all_reference"), "Select All", class = "btn-sm"),
        actionButton(ns("deselect_all_reference"), "Deselect All", class = "btn-sm")
      ),
      
      checkboxGroupInput(ns("keyword_reference"), 
                         "Select Reference Group(s):", 
                         choices = NULL)
    ),
    
    # 2. Methodology Explainer (Collapsible)
    div(
      style = "margin: 15px 0;",
      tags$details(
        tags$summary(
          style = "cursor: pointer; font-weight: bold; color: #7b1fa2; padding: 10px; background-color: #f3e5f5; border-radius: 3px; border-left: 4px solid #7b1fa2;",
          "🎯 Keyword Analysis Explained"
        ),
        div(
          style = "padding: 15px; background-color: #f3e5f5; border-radius: 0 0 5px 5px; border-top: 1px solid #ce93d8; font-size: 13px; line-height: 1.4;",
          HTML("<strong>What is Keyword Analysis?</strong><br/>
                Keyword analysis identifies words statistically over-represented in targets vs references.<br/><br/>
                <strong>Interpreting Results:</strong><br/>
                • <strong>Positive log-odds:</strong> More frequent in target group(s)<br/>
                • <strong>Negative log-odds:</strong> More frequent in reference group(s)")
        )
      )
    ),
    
    # 3. Analysis Parameters Section
    hr(),
    h5("Analysis Parameters"),
    
    fluidRow(
      column(6,
             numericInput(ns("keyword_ngram_n"), "N for n-grams:", 
                          value = 1, min = 1, max = 5)
      ),
      column(6,
             numericInput(ns("keyword_top_n"), "Number of features per plot:", 
                          value = 15, min = 5, max = 30)
      )
    ),
    
    # 4. Stop Words Filtering
    div(
      style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background-color: #f8f9fa; margin-bottom: 15px;",
      h6("🚫 Stop Words Filtering", style = "margin-top: 0; color: #495057;"),
      checkboxInput(ns("use_stopwords"), "Remove stop words (function words)", value = FALSE),
      
      conditionalPanel(
        condition = paste0("input['", ns("use_stopwords"), "']"),
        
        div(style = "margin-bottom: 10px; padding: 8px; background-color: #e3f2fd; border-radius: 3px; font-size: 13px;",
            strong("Note:"), " Stop words are removed before n-gram generation."),
        
        fluidRow(
          column(6,
                 selectInput(ns("stopword_language"), "Language:", 
                             choices = c("English" = "en", "French" = "fr", "German" = "de"), 
                             selected = "en")
          ),
          column(6,
                 checkboxInput(ns("include_contractions"), "Include contractions", value = TRUE)
          )
        ),
        
        radioButtons(ns("custom_stopword_mode"), "Custom mode:", 
                     choices = c("Add" = "add", "Replace" = "replace"), inline = TRUE),
        
        textAreaInput(ns("custom_stopwords"), "Custom words (one per line):", rows = 3),
        
        downloadButton(ns("download_stopwords"), "📥 Download Stop Words", 
                       class = "btn-outline-secondary btn-sm")
      )
    ),
    
    # 5. Plotting & Execution
    div(
      style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; background-color: #f8f9fa; margin-bottom: 15px;",
      h6("📊 Plot Options", style = "margin-top: 0; color: #495057;"),
      radioButtons(ns("plot_type"), "What to plot:",
                   choices = list("Both directions" = "both", 
                                  "Target only" = "target_only", 
                                  "Reference only" = "reference_only"), 
                   selected = "both"),
      checkboxInput(ns("show_counts"), "Show frequency counts on plot", value = FALSE)
    ),
    
    ## Execution Buttons
    div(style = "text-align: center; margin: 20px 0;",
        actionButton(ns("run_keyword_analysis"), "🚀 Run Keyword Analysis", 
                     class = "btn-primary btn-lg")
    ),
    
    div(style = "text-align: center; margin: 15px 0;",
        downloadButton(ns("download_keyword_csv"), "📥 CSV Results", class = "btn-sm"),
        downloadButton(ns("download_keyword_plot"), "📊 Plot Image", class = "btn-sm", 
                       style = "margin-left: 10px;")
    ),
    
    # 6. Output Display
    hr(),
    h4("Analysis Results"),
    tableOutput(ns("keyword_summary")),
    
    h5("Keyword Comparison Plot"),
    plotOutput(ns("keyword_comparison_plot"), height = "600px"),
    
    h5("Detailed Keyword Scores"),
    DT::dataTableOutput(ns("keyword_scores_table"))
  ) # End tagList
} # End keywordUI function