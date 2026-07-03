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
          HTML("<p style='margin-top:0;'>Octobox uses three tag schemes. The <strong>simplified categories</strong> below apply to <em>untagged</em> analysis (from the built-in frequency database). When you analyse <em>tagged</em> data you choose <strong>UPOS</strong> (Universal) or <strong>XPOS</strong> (Penn Treebank) tags - both are listed further down.</p>

                 <strong>1. Simplified categories (untagged data):</strong><br/>
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
                 • <strong>OTHER:</strong> Irregular (Irr)<br/><br/>

                 <strong>2. UPOS - Universal POS tags (tagged data):</strong><br/>
                 • <strong>NOUN:</strong> Common noun (dog, idea)<br/>
                 • <strong>PROPN:</strong> Proper noun (London, Alice)<br/>
                 • <strong>VERB:</strong> Main verb (run, thought)<br/>
                 • <strong>AUX:</strong> Auxiliary (is, have, will)<br/>
                 • <strong>ADJ:</strong> Adjective (big, red)<br/>
                 • <strong>ADV:</strong> Adverb (quickly, very)<br/>
                 • <strong>PRON:</strong> Pronoun (she, it)<br/>
                 • <strong>DET:</strong> Determiner (the, this)<br/>
                 • <strong>ADP:</strong> Adposition/preposition (in, of)<br/>
                 • <strong>CCONJ:</strong> Coordinating conjunction (and, but)<br/>
                 • <strong>SCONJ:</strong> Subordinating conjunction (if, because)<br/>
                 • <strong>NUM:</strong> Numeral (one, 2020)<br/>
                 • <strong>PART:</strong> Particle (to, 's, not)<br/>
                 • <strong>INTJ:</strong> Interjection (oh, wow)<br/>
                 • <strong>PUNCT:</strong> Punctuation (. , ?)<br/>
                 • <strong>SYM:</strong> Symbol (%, $)<br/>
                 • <strong>X:</strong> Other/unclassified<br/><br/>

                 <strong>3. XPOS - Penn Treebank tags (tagged data):</strong><br/>
                 • <strong>NN / NNS:</strong> Noun, singular / plural<br/>
                 • <strong>NNP / NNPS:</strong> Proper noun, singular / plural<br/>
                 • <strong>VB:</strong> Verb, base form<br/>
                 • <strong>VBD / VBN:</strong> Verb, past tense / past participle<br/>
                 • <strong>VBG:</strong> Verb, gerund/present participle (-ing)<br/>
                 • <strong>VBP / VBZ:</strong> Verb, non-3rd / 3rd person present<br/>
                 • <strong>JJ / JJR / JJS:</strong> Adjective / comparative / superlative<br/>
                 • <strong>RB / RBR / RBS:</strong> Adverb / comparative / superlative<br/>
                 • <strong>PRP / PRP$:</strong> Personal / possessive pronoun<br/>
                 • <strong>DT:</strong> Determiner (the, a)<br/>
                 • <strong>IN:</strong> Preposition/subordinating conjunction<br/>
                 • <strong>CC:</strong> Coordinating conjunction (and, or)<br/>
                 • <strong>CD:</strong> Cardinal number<br/>
                 • <strong>MD:</strong> Modal (can, will)<br/>
                 • <strong>TO:</strong> 'to'<br/>
                 • <strong>UH:</strong> Interjection<br/>
                 • <strong>WDT / WP / WRB:</strong> Wh-determiner / pronoun / adverb<br/>
                 <span style='color:#7b1fa2;'>Tip: XPOS is finer-grained (tense, number, degree); UPOS is coarser and cross-linguistic.</span>")
        )
      )
    )
  )
}