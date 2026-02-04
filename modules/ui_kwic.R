# =============================================================================
# modules/ui_kwic.R - Concordancer & Collocation UI ----
# =============================================================================

# -----------------------------------------------------------------------------
# 1. KWIC Search Controls UI
# -----------------------------------------------------------------------------
kwicUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h4("KWIC (Key Word in Context)"),
    
    # Tagged Corpus Section ----
    conditionalPanel(
      condition = paste0("output['", ns("tagged_available"), "']"),
      div(
        style = "background-color: #e7f3ff; border: 1px solid #b3d9ff; padding: 12px; border-radius: 5px; margin-bottom: 15px;",
        h5("🏷️ Tagged Corpus Available", style = "margin-top: 0; color: #0056b3;"),
        
        checkboxInput(ns("use_tagged_tokens"),
                     "Analyze tagged corpus for KWIC",
                     value = FALSE),
        
        conditionalPanel(
          condition = paste0("input['", ns("use_tagged_tokens"), "'] == true"),
          
          radioButtons(ns("tag_column"),
                      "Tag type:",
                      choices = list(
                        "XPOS tags (word_NN)" = "xpos",
                        "UPOS tags (word_NOUN)" = "upos",
                        "POS only (NN)" = "pos_only"
                      ),
                      selected = "xpos"),
          
          div(
            style = "font-size: 12px; color: #6c757d; margin-top: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 3px;",
            HTML("<strong>💡 KWIC search tips for tagged tokens:</strong><br/>
                 • <strong>Word in context:</strong> <code>bank</code> finds bank_NN, bank_VB, etc.<br/>
                 • <strong>Specific form:</strong> <code>bank_NN</code> finds only noun usage<br/>
                 • <strong>POS in context:</strong> <code>_VBD</code> shows all past tense verbs<br/>
                 • <strong>With regex:</strong> <code>_VB.*</code> shows all verb forms in context<br/>
                 • <strong>Useful for:</strong> Disambiguating homographs, studying grammatical patterns")
          )
        )
      )
    ),
    
    # ... rest of your existing KWIC UI ...
    
    # 1.1. Regex Input & Documentation ----
    div(
      style = "margin-bottom: 15px;",
      textInput(ns("index"), "Search Pattern (Regex):", value = "example"),
      
      # Collapsible Regex Guide
      tags$details(
        style = "margin-top: 10px; border: 1px solid #bbdefb; border-radius: 5px; overflow: hidden;",
        tags$summary(
          style = "cursor: pointer; font-weight: bold; color: #1976d2; padding: 10px; background-color: #e3f2fd;",
          "🔤 Regular Expression Guide (Click to Expand)"
        ),
        div(
          style = "padding: 15px; background-color: #f5faff; font-size: 13px; line-height: 1.5;",
          HTML("<strong>Basic Patterns:</strong><br/>
                • <code>\\bcat\\b</code> → Whole word 'cat' only<br/>
                • <code>^cat</code> → Starts with 'cat' (caterpillar)<br/>
                • <code>cat$</code> → Ends with 'cat' (tomcat)<br/><br/>
                <strong>Advanced:</strong><br/>
                • <code>climat.*</code> → climate, climatic, climatology<br/>
                • <code>colou?r</code> → color and colour<br/>
                • <code>(big|large)</code> → logical OR match")
        )
      )
    ),
    
    # 1.2. Context Settings ----
    fluidRow(
      column(6, numericInput(ns("n"), "Context Window (n)", value = 5, min = 1, max = 20)),
      column(6, style = "margin-top: 25px;", 
             checkboxInput(ns("separated"), "Separate Columns", TRUE))
    ),
    
    div(style = "text-align: center; margin-top: 20px;",
        actionButton(ns("run"), "🔍 Run Concordancer", 
                     class = "btn-success btn-lg", style = "width: 100%;"))
  )
}

# -----------------------------------------------------------------------------
# 2. KWIC Results & Dynamic Counting UI
# -----------------------------------------------------------------------------
kwicResultsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    conditionalPanel(
      condition = sprintf("output['%s']", ns("has_results")),
      
      # 2.1. Dynamic Counting Panel ----
      div(
        style = "margin-bottom: 15px; padding: 15px; border: 1px solid #ddd; border-radius: 5px; background-color: #f8f9fa;",
        checkboxInput(ns("enable_counting"), "📊 Enable Dynamic Position Counting", value = FALSE),
        
        conditionalPanel(
          condition = sprintf("input['%s']", ns("enable_counting")),
          hr(),
          h6("🔢 Multi-Column Frequency Analysis"),
          fluidRow(
            column(6,
                   checkboxGroupInput(ns("count_columns"), "Count by Position(s):", choices = NULL),
                   div(class = "btn-group",
                       actionButton(ns("select_all_positions"), "📍 All", class = "btn-xs"),
                       actionButton(ns("select_immediate_positions"), "🎯 ±1", class = "btn-xs"),
                       actionButton(ns("clear_position_selection"), "🗑️ Clear", class = "btn-xs"))
            ),
            column(6,
                   selectInput(ns("sort_method"), "Sort Method:",
                               choices = list("Frequency (Desc)" = "desc_count", "Alpha" = "alpha_asc")),
                   numericInput(ns("max_combinations"), "Show Top N:", value = 50, step = 10),
                   actionButton(ns("apply_counting"), "Apply Analysis", class = "btn-primary btn-sm btn-block")
            )
          )
        )
      ),
      
      # 2.2. Table Output ----
      downloadButton(ns("download_csv"), "📥 Download CSV", class = "btn-outline-primary btn-sm", style="margin-bottom:10px;"),
      DT::dataTableOutput(ns("result"))
    )
  )
}

# -----------------------------------------------------------------------------
# 3. Collocation Analysis UI
# -----------------------------------------------------------------------------
collocationUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      style = "margin: 15px 0; padding: 20px; border: 1px solid #dee2e6; border-radius: 8px; background-color: #ffffff;",
      h4("🔗 Collocation Analysis", style = "color: #2c3e50;"),
      p("Analyze statistical attraction between your search term and neighbors.", style="color: #7f8c8d;"),
      
      # 3.1. Position Selection Matrix ----
      fluidRow(
        column(8, checkboxGroupInput(ns("analysis_positions"), "Positions to Analyze:", choices = NULL, inline = TRUE)),
        column(4, verticalLayout(
          actionButton(ns("select_left"), "⬅️ All Left", class = "btn-sm btn-block"),
          actionButton(ns("select_right"), "➡️ All Right", class = "btn-sm btn-block"),
          actionButton(ns("select_immediate"), "🎯 Immediate ±1", class = "btn-sm btn-block")
        ))
      ),
      
      # 3.2. Stats Configuration ----
      hr(),
      fluidRow(
        column(4, numericInput(ns("min_freq"), "Min Freq:", value = 3)),
        column(4, numericInput(ns("top_n"), "Show Top:", value = 20)),
        column(4, selectInput(ns("stat_measure"), "Statistic:", 
                              choices = c("Log-Likelihood", "Mutual Information", "T-Score", "Raw Frequency")))
      ),
      
      actionButton(ns("run_analysis"), "🔗 Run Analysis", class = "btn-success btn-block", style="margin-top:15px;")
    ),
    
    # 3.3. Results Table ----
    conditionalPanel(
      condition = sprintf("output['%s']", ns("has_collocation_results")),
      h5("📈 Collocation Results"),
      downloadButton(ns("download_collocations"), "📥 Download Analysis", class = "btn-outline-primary btn-sm"),
      DT::dataTableOutput(ns("collocation_table"))
    )
  )
}