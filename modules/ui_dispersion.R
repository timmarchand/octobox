# =============================================================================
# modules/ui_dispersion.R - Dispersion Analysis UI ----------------------------
# =============================================================================

dispersionUI <- function(id) {
  ns <- NS(id)
tagList(
  h3("Dispersion Analysis", style = "color: #495057;"),
  
  # Tagged Corpus Section ----
  conditionalPanel(
    condition = paste0("output['", ns("tagged_available"), "']"),
    div(
      style = "background-color: #e7f3ff; border: 1px solid #b3d9ff; padding: 12px; border-radius: 5px; margin-bottom: 15px;",
      h5("🏷️ Tagged Corpus Available", style = "margin-top: 0; color: #0056b3;"),
      
      checkboxInput(ns("use_tagged_tokens"),
                   "Analyze tagged corpus for dispersion",
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
          HTML("<strong>💡 Search tips for tagged tokens:</strong><br/>
               • <strong>Any form:</strong> <code>run</code> finds run_VB, run_VBG, run_NN, etc.<br/>
               • <strong>Exact form:</strong> <code>run_VB</code> finds only base verb<br/>
               • <strong>POS category:</strong> <code>_NN</code> finds all singular nouns<br/>
               • <strong>With regex:</strong> <code>_VB.*</code> finds all verb forms (VB, VBD, VBG, etc.)<br/>
               • <strong>With regex:</strong> <code>_NN.*</code> finds all noun forms (NN, NNS, NNP, etc.)")
        )
      )
    )
  ),
  
  # 1. Educational Guide (Collapsible) --------------------------------------
  div(
    style = "margin: 15px 0;",
    tags$details(
      tags$summary(
        style = "cursor: pointer; font-weight: bold; color: #7b1fa2; padding: 10px; background-color: #f3e5f5; border-radius: 3px; border-left: 4px solid #7b1fa2;",
        "📊 Dispersion Measures Explained"
      ),
      div(
        style = "padding: 15px; background-color: #f3e5f5; border-radius: 0 0 5px 5px; border-top: 1px solid #ce93d8; font-size: 13px; line-height: 1.6;",
        HTML("<strong>Gries's DP (Deviation of Proportions):</strong> 0 (even) to 1 (clumped).<br/>
              <strong>Range:</strong> Percentage of texts containing the word.")
      )
    )
  ),
  
  # 2. Search & Regex Input -------------------------------------------------
  div(
    style = "margin-bottom: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
    h5("Search Term", style = "margin-top: 0; color: #495057;"),
    textInput(ns("search_term"),
              "Enter word or phrase to analyze:",
              value = "",
              placeholder = "e.g., corpus, language, analysis"),
    checkboxInput(ns("use_regex"),
                  "Use regular expression (regex)",
                  value = FALSE),
    div(
      style = "font-size: 12px; color: #6c757d; margin-top: 5px;",
      HTML("💡 <strong>Regex examples:</strong> <code>\\w+ing\\b</code> (words ending in 'ing') | <code>_VB.*</code> (all verb forms when using tagged tokens)")
    )
  ),

    # 3. Calculation Parameters -----------------------------------------------
    div(
      style = "margin-bottom: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6;",
      h5("Analysis Options", style = "margin-top: 0; color: #495057;"),

      fluidRow(
        column(6,
          radioButtons(ns("dispersion_measure"),
                      "Dispersion measure:",
                      choices = list("Gries's DP" = "dp", "Range (%)" = "range", "Both" = "both"),
                      selected = "both")
        ),
        column(6,
          radioButtons(ns("dispersion_level"),
                      "Calculate across:",
                      choices = list("Individual texts" = "text", "Meta groups" = "meta"),
                      selected = "text")
        )
      ),

      # Faceting Option ----
      conditionalPanel(
        condition = paste0("input['", ns("dispersion_level"), "'] == 'text'"),
        checkboxInput(ns("facet_by_meta"),
                      "Show separate plots by meta group",
                      value = FALSE)
      )
    ),

    # Action Button ----
    div(
      style = "text-align: center; margin: 20px 0;",
      actionButton(ns("run_dispersion"),
                  "📊 Calculate Dispersion",
                  class = "btn-primary btn-lg")
    ),

    # 4. Results & Downloads --------------------------------------------------
    conditionalPanel(
      condition = paste0("output['", ns("has_results"), "']"),

      hr(),
      h4("Results", style = "margin-top: 20px;"),

      # Summary Stats Table ----
      div(
        style = "margin-bottom: 20px; padding: 15px; background-color: #e8f5e8; border-radius: 5px; border-left: 4px solid #4caf50;",
        uiOutput(ns("dispersion_summary"))
      ),

      DT::dataTableOutput(ns("dispersion_table")),

      div(
        style = "margin-top: 15px; text-align: center;",
        downloadButton(ns("download_dispersion_csv"), "📥 Download Results", class = "btn-outline-primary")
      ),

      # 5. Visualization Engine -----------------------------------------------
      h5("Dispersion Visualizations", style = "margin-top: 30px;"),

      selectInput(ns("viz_type"),
                 "Visualization type:",
                 choices = list(
                   "Dispersion Plot (Barcode)" = "barcode",
                   "Frequency Bar Chart" = "bar",
                   "Relative Frequency Line" = "line",
                   "Heatmap (Meta groups only)" = "heatmap"
                 ),
                 selected = "barcode"),

      # Barcode Granularity Settings ----
      conditionalPanel(
        condition = paste0("input['", ns("viz_type"), "'] == 'barcode'"),
        conditionalPanel(
          condition = paste0("input['", ns("dispersion_level"), "'] == 'text'"),
          div(
            style = "margin-top: 10px; margin-bottom: 10px;",
            radioButtons(ns("barcode_granularity"),
                        "Show barcode by:",
                        choices = list(
                          "Whole corpus" = "corpus",
                          "Meta group" = "meta",
                          "Individual text" = "text"
                        ),
                        selected = "corpus"),
            
            conditionalPanel(
              condition = paste0("input['", ns("barcode_granularity"), "'] == 'corpus'"),
              checkboxInput(ns("barcode_color_by_meta"), "Color-code by meta group", value = TRUE)
            )
          )
        )
      ),

      # Plot Display Area ----
      conditionalPanel(condition = paste0("input['", ns("viz_type"), "'] == 'bar'"), plotOutput(ns("dispersion_plot"), height = "400px")),
      conditionalPanel(condition = paste0("input['", ns("viz_type"), "'] == 'barcode'"), uiOutput(ns("barcode_plot_ui"))),
      conditionalPanel(condition = paste0("input['", ns("viz_type"), "'] == 'line'"), plotOutput(ns("dispersion_line"), height = "400px")),
      conditionalPanel(condition = paste0("input['", ns("viz_type"), "'] == 'heatmap'"), plotOutput(ns("dispersion_heatmap"), height = "400px")),

      div(
        style = "margin-top: 15px; text-align: center;",
        downloadButton(ns("download_dispersion_plot"), "📊 Download Plot", class = "btn-outline-secondary")
      )
    )
  )
}