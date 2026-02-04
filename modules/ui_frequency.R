# =============================================================================
# modules/ui_frequency.R - Frequency Analysis UI ------------------------------
# =============================================================================

frequencyUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("📈 N-gram Frequency Analysis"),
    
    # 1. Tagged Corpus Panel --------------------------------------------------
    # Provides options if a UDPipe tagged corpus is detected
    conditionalPanel(
      condition = paste0("output['", ns("tagged_available"), "']"),
      div(
        style = "background-color: #e7f3ff; border: 1px solid #b3d9ff; padding: 12px; border-radius: 5px; margin-bottom: 15px;",
        h5("🏷️ Tagged Corpus Available", style = "margin-top: 0; color: #0056b3;"),
        
        checkboxInput(ns("use_tagged_tokens"),
                      "Analyze tagged corpus",
                      value = FALSE),
        
        conditionalPanel(
          condition = paste0("input['", ns("use_tagged_tokens"), "'] == true"),
          
          radioButtons(ns("tag_column"),
                       "Analysis type:",
                       choices = list(
                         "Words with XPOS tags (word_NN, run_VB)" = "xpos",
                         "Words with UPOS tags (word_NOUN, run_VERB)" = "upos",
                         "POS tags only (NN, VB, JJ)" = "pos_only",
                         "Lemmas with tags (lemma_NN, lemma_VB)" = "lemma"
                       ),
                       selected = "xpos"),
          
          div(
            style = "font-size: 12px; color: #6c757d; margin-top: 5px;",
            HTML("<strong>💡 Examples:</strong><br/>
                  • <strong>Words with XPOS:</strong> the_DT, running_VBG, quickly_RB<br/>
                  • <strong>POS tags only:</strong> DT, VBG, RB (for tag frequency)<br/>
                  • <strong>Lemmas:</strong> run_VB, quick_RB (base forms)")
          )
        )
      )
    ),
    
    p("Generate frequency lists and analyze word/n-gram distributions in your corpus."),
    
    # 2. Main N-gram Configuration --------------------------------------------
    div(
      style = "margin-bottom: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
      h6("⚙️ N-gram Configuration", style = "margin-top: 0; color: #495057;"),
      
      fluidRow(
        column(3,
               div(title = "1 = individual words, 2 = word pairs, 3 = three-word phrases, etc.",
                   numericInput(ns("ngram_n"), "N-gram Size:", 
                                value = 1, min = 1, max = 5, step = 1)
               )
        ),
        column(3,
               div(title = "How to organize results: by text, group, or whole corpus",
                   selectInput(ns("ngram_view"), "Results View:",
                               choices = list(
                                 "By Text" = "text",
                                 "By Meta Group" = "meta", 
                                 "Whole Corpus" = "corpus"
                               ),
                               selected = "meta")
               )
        ),
        column(6,
               div(title = "Frequency bands from database (Unigrams only)",
                   br(),
                   # Show checkbox for n=1 ----
                   conditionalPanel(
                     condition = paste0("input['", ns("ngram_n"), "'] == 1"),
                     checkboxInput(ns("include_lexical_info"), "Include Frequency Info", TRUE)
                   ),
                   # Warning for n > 1 ----
                   conditionalPanel(
                     condition = paste0("input['", ns("ngram_n"), "'] > 1"),
                     div(
                       style = "background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 8px; border-radius: 4px; font-size: 12px;",
                       HTML("📝 <strong>Note:</strong> Frequency info is only available for individual words (n=1).")
                     )
                   )
               )
        )
      )
    ),
    
    # 3. Frequency Range Selection (Dynamic) ----------------------------------
    conditionalPanel(
      condition = paste0("input['", ns("ngram_n"), "'] == 1"),
      div(
        style = "margin-bottom: 20px; padding: 15px; background-color: #e8f5e8; border-radius: 5px; border: 1px solid #c8e6c8;",
        h6("📚 Frequency Analysis Options", style = "margin-top: 0; color: #2e7d32;"),
        
        selectInput(ns("freq_list_type"), "Analysis Type:",
                    choices = list(
                      "Token-based (Word Forms)" = "token",
                      "Headword-based (Lemmatized)" = "head"
                    ),
                    selected = "token"),
        
        # Lexical Range Checklist ----
        conditionalPanel(
          condition = paste0("input['", ns("include_lexical_info"), "']"),
          div(
            style = "margin-top: 15px; padding: 12px; background-color: #fff3cd; border-radius: 4px; border-left: 3px solid #ffc107;",
            h6("🔧 Lexical Range Selection", style = "margin-top: 0; color: #856404;"),
            checkboxGroupInput(ns("selected_ranges"), "Select Frequency Ranges:",
                               choices = c("01k", "02k", "03k", "04k", "05k", "06k", "07k", "08k", "09k", "10k"),
                               selected = c("01k", "02k", "03k", "04k", "05k", "06k", "07k", "08k", "09k", "10k"),
                               inline = TRUE),
            
            div(
              style = "margin-top: 10px;",
              actionButton(ns("select_default_ranges"), "📊 All", class = "btn-outline-success btn-sm"),
              actionButton(ns("select_core_ranges"), "🎯 Core", class = "btn-outline-info btn-sm"),
              actionButton(ns("clear_range_selection"), "🗑️ Clear", class = "btn-outline-danger btn-sm")
            )
          )
        )
      )
    ),
    
    # 4. Stop Words Panel -----------------------------------------------------
    div(
      style = "margin-bottom: 20px; padding: 15px; background-color: #fff3cd; border-radius: 5px; border: 1px solid #ffeaa7;",
      h6("🚫 Stop Words Configuration", style = "margin-top: 0; color: #856404;"),
      
      checkboxInput(ns("use_stopwords"), "Remove Stop Words", value = FALSE),
      
      conditionalPanel(
        condition = paste0("input['", ns("use_stopwords"), "']"),
        fluidRow(
          column(6, selectInput(ns("stopword_language"), "Language:", choices = c("en", "es", "fr", "de", "it", "ja", "zh"), selected = "en")),
          column(6, br(), checkboxInput(ns("include_contractions"), "Include Contractions", TRUE))
        ),
        textAreaInput(ns("custom_stopwords"), "Custom Words (one per line):", rows = 3),
        downloadButton(ns("download_stopwords"), "📥 Download List", class = "btn-outline-secondary btn-sm")
      )
    ),
    
    # 5. Run Button & Data Table ----------------------------------------------
    div(
      style = "text-align: center; margin: 30px 0;",
      actionButton(ns("run_ngram"), "🚀 Run N-gram Analysis", class = "btn-primary btn-lg")
    ),
    
    hr(),
    h5("📊 Analysis Results", style = "text-align: center;"),
    div(style = "text-align: center; margin-bottom: 10px;",
        downloadButton(ns("download_ngram_csv"), "📥 Download Complete CSV", class = "btn-outline-primary")),
    
    DT::dataTableOutput(ns("ngram_result")),
    
    # 6. Visualization Section ------------------------------------------------
    div(
      style = "margin-top: 30px;",
      h5("📊 Frequency Visualizations"),
      
      div(
        style = "margin-bottom: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
        fluidRow(
          column(4, numericInput(ns("chart_top_n"), "Top N in Charts:", value = 15, min = 5, max = 50)),
          column(4, br(), actionButton(ns("generate_charts"), "📊 Generate Charts", class = "btn-success")),
          column(4, br(), conditionalPanel(condition = paste0("output['", ns("charts_available"), "']"), 
                                           div(style = "color: green;", "✅ Ready")))
        )
      ),
      
      # Chart Display Tabs ----
      conditionalPanel(
        condition = paste0("output['", ns("charts_available"), "']"),
        tabsetPanel(
          tabPanel("Token Frequency", plotOutput(ns("test_plot"), height = "500px")),
          tabPanel("Frequency Bands", plotOutput(ns("freq_band_plot"), height = "500px"))
        )
      )
    )
  )
}