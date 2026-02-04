# =============================================================================
# FIXED PoS UI ----
# =============================================================================

posUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # 1. Header & Title ----
    h4("Part-of-Speech Analysis"),
    
    # 2. UDPipe Availability Panel ----
    # Displays specialized controls if a tagged corpus is detected
    conditionalPanel(
      condition = paste0("output['", ns("tagged_available"), "']"),
      div(
        style = "background-color: #e7f3ff; border: 1px solid #b3d9ff; padding: 12px; border-radius: 5px; margin-bottom: 15px;",
        h5("🏷️ UDPipe Tagged Corpus Available", style = "margin-top: 0; color: #0056b3;"),
        
        checkboxInput(ns("use_udpipe_pos"),
                      "Use UDPipe tagged data (more accurate)",
                      value = FALSE),
        
        # Nested panel for Tag System selection
        conditionalPanel(
          condition = paste0("input['", ns("use_udpipe_pos"), "'] == true"),
          
          radioButtons(ns("pos_tag_system"),
                       "Tag system:",
                       choices = list(
                         "XPOS (Penn Treebank: NN, VB, JJ, etc.)" = "xpos",
                         "UPOS (Universal: NOUN, VERB, ADJ, etc.)" = "upos"
                       ),
                       selected = "xpos",
                       inline = TRUE),
          
          div(
            style = "font-size: 12px; color: #6c757d; margin-top: 5px;",
            HTML("<strong>✅ Advantages:</strong><br/>
                  • More accurate POS tagging from UDPipe model<br/>
                  • Disambiguates words with multiple POS (e.g., 'run' as noun vs verb)<br/>
                  • Includes all grammatical categories<br/>
                  • Results match your tagged corpus exactly")
          )
        )
      )
    ),
    
    # 3. Configuration Panel ----
    # User inputs for plot appearance and data scaling
    div(
      style = "margin-bottom: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
      h6("⚙️ PoS Analysis Configuration", style = "margin-top: 0; color: #495057;"),
      
      fluidRow(
        column(4,
               div(title = "Number of top PoS tags to display in plots",
                   numericInput(ns("pos_top_n"), "Show Top N PoS Tags:", 
                                value = 8, min = 3, max = 15, step = 1)
               )
        ),
        column(4,
               div(title = "Choose how to compare PoS distributions across groups",
                   selectInput(ns("pos_comparison_type"), "Plot Type:",
                               choices = list(
                                 "Side-by-side Bars" = "grouped",
                                 "Separate Panels" = "faceted"
                               ),
                               selected = "grouped")
               )
        ),
        column(4,
               div(title = "Show proportions (percentages) instead of raw counts",
                   br(),
                   checkboxInput(ns("pos_proportional"), "Show Proportions", TRUE)
               )
        )
      )
    ),
    
    # 4. Information Panel ----
    div(
      style = "margin-bottom: 20px; padding: 15px; background-color: #e3f2fd; border-radius: 5px; border: 1px solid #bbdefb;",
      h6("ℹ️ About PoS Analysis", style = "margin-top: 0; color: #1976d2;"),
      div(style = "font-size: 13px; line-height: 1.4;",
          HTML("<strong>What this shows:</strong><br/>
                • <strong>Distribution:</strong> How grammatical categories are used across your corpus<br/>
                • <strong>Standardization:</strong> Different PoS tag systems are normalized to major categories<br/>
                • <strong>Comparison:</strong> See how different text groups use grammar differently<br/>
                • <strong>Coverage:</strong> Based on words found in the frequency database")
      )
    ),
    
    # 5. Chart Generation Controls ----
    div(
      style = "margin-bottom: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6; text-align: center;",
      h6("📊 Chart Generation", style = "margin-top: 0; color: #495057;"),
      
      actionButton(ns("run_and_generate"),
                   "🏷️ Analyze POS & Generate Charts",
                   class = "btn-primary btn-lg",
                   style = "width: 100%; margin: 20px 0;"),
      
      br(),
      
      # Status Indicator
      conditionalPanel(
        condition = paste0("output['", ns("pos_charts_available"), "']"),
        div(
          style = "margin-top: 10px; padding: 8px; background-color: #d4edda; border-radius: 3px; font-size: 12px;",
          HTML("✅ <strong>Charts Ready</strong> - View charts below")
        )
      ),
      
      # Guidance
      div(
        style = "margin-top: 10px; padding: 8px; background-color: #e3f2fd; border-radius: 3px; font-size: 12px;",
        HTML("<strong>💡 Chart Generation:</strong> Charts are generated on-demand to improve performance. Run tokenization first, then click 'Analyze POS'.")
      )
    ),
    
    # 6. Results & Visualizations ----
    h5("📊 PoS Distribution Results"),
    
    # Download Buttons (Conditional)
    conditionalPanel(
      condition = paste0("output['", ns("pos_charts_available"), "']"),
      div(
        style = "margin-bottom: 15px; text-align: center;",
        downloadButton(ns("download_pos_plot"), "📥 Download Plot", 
                       class = "btn-outline-primary btn-sm", style = "margin-right: 10px;"),
        downloadButton(ns("download_pos_csv"), "📥 Download Data", 
                       class = "btn-outline-secondary btn-sm")
      )
    ),
    
    # Plot Output Area
    conditionalPanel(
      condition = paste0("output['", ns("pos_charts_available"), "']"),
      plotOutput(ns("pos_plot"), height = "500px")
    ),
    
    # Placeholder Message
    conditionalPanel(
      condition = paste0("!output['", ns("pos_charts_available"), "']"),
      div(
        style = "text-align: center; padding: 40px; color: #6c757d; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
        h5("📊 Charts Not Generated Yet"),
        p("Run tokenization first, then click 'Analyze POS' to create visualizations.")
      )
    ),
    
    # 7. PoS Tag Guide (Expandable) ----
    div(
      style = "margin-top: 20px;",
      tags$details(
        tags$summary(
          style = "cursor: pointer; font-weight: bold; color: #7b1fa2; padding: 10px; background-color: #f3e5f5; border-radius: 3px; border-left: 4px solid #7b1fa2;",
          "📖 PoS Tag Guide"
        ),
        div(
          style = "padding: 15px; background-color: #f3e5f5; border-radius: 0 0 5px 5px; border-top: 1px solid #ce93d8; font-size: 13px; line-height: 1.4;",
          HTML("<strong>PoS Categories in Your Data:</strong><br/>
                 • <strong>NOUN:</strong> Person, place, thing (Noun, IrrN)<br/>
                 • <strong>VERB:</strong> Action or state (Verb)<br/>
                 • <strong>ADJ:</strong> Describes nouns (Adj)<br/>
                 • <strong>ADV:</strong> Describes verbs/adjectives (Adv)<br/>
                 • <strong>PREP:</strong> Relationships (Prep)<br/>
                 • <strong>DET:</strong> Specifies nouns (Detr)<br/>
                 • <strong>PRON:</strong> Replaces nouns (Pron)<br/>
                 • <strong>CONJ:</strong> Connectors (Conj)<br/>
                 • <strong>NUM:</strong> Quantities (Num)<br/>
                 • <strong>ART:</strong> Articles (ArtP)<br/>
                 • <strong>AUX:</strong> Auxiliary verbs (AuxV)<br/>
                 • <strong>PART:</strong> Particles/Negation (InfM, Neg, Exst)<br/>
                 • <strong>INTERJ:</strong> Interjections (Intj)<br/>
                 • <strong>ABBR:</strong> Abbreviations (Abbr)<br/>
                 • <strong>TIME:</strong> Time expressions (Time)<br/>
                 • <strong>OTHER:</strong> Irregular (Irr)")
        )
      )
    )
  )
}