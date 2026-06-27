# =============================================================================
# modules/ui_data_input.R - Full Text Version
# =============================================================================

dataInputUI <- function(id) {
  ns <- NS(id)

  tagList(
    div(title = "Upload or paste your text data for corpus analysis",
      h4("Data Input")
    ),

    # Performance Tip Box
    div(
      style = "background-color: #fff3cd; border: 1px solid #ffeaa7; padding: 10px; border-radius: 4px; margin-bottom: 15px; font-size: 0.9em;",
      HTML("<strong>Inputting Tips:</strong><br/>
            • Upload a <strong>CSV file</strong> (the default) for texts with metadata columns<br/>
            • After uploading, <strong>choose your text column</strong> before tokenizing - this step is required<br/>
            • Use <strong>Paste Text</strong> for quick demos, or <strong>Multiple Files</strong> for large corpora")
    ),

    radioButtons(ns("input_type"), "Select Input Method:",
                 choices = c("Upload Single File (CSV)" = "file",
                             "Paste Text" = "paste",
                             "Upload Multiple Files (Corpus)" = "corpus"),
                 selected = "file",
                 inline = FALSE),

    hr(),

    # 1. Single File Upload Panel ----
    conditionalPanel(
      condition = paste0("input['", ns("input_type"), "'] == 'file'"),
      div(title = "Upload a .csv file or .txt file",
        fileInput(ns("upload_csv"), "Upload CSV File", 
                  accept = c(".csv", "text/csv", "text/comma-separated-values")),
        
        # Skip rows option
        numericInput(ns("skip_rows"), 
                     "Skip first N rows:", 
                     value = 0, 
                     min = 0, 
                     max = 100,
                     step = 1),
        
        helpText("Use this to skip header or information rows in your CSV file.")
      )
    ),  # ← CLOSE conditionalPanel for file upload

    # 2. Corpus (Multiple Files) Upload Panel ----
    conditionalPanel(
      condition = paste0("input['", ns("input_type"), "'] == 'corpus'"),
      fileInput(ns("upload_corpus"), "Upload Corpus Files:", 
                multiple = TRUE, accept = c(".txt", ".csv"), width = "100%"),

      div(
        style = "margin-top: 10px; padding: 15px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px;",
        h5("📁 Meta Grouping Strategy", style = "margin-top: 0; font-size: 15px;"),

        radioButtons(ns("meta_assignment_method"), "How to assign meta groups:",
                    choices = list("Manual assignment" = "manual", "Extract from filename" = "delimiter"),
                    selected = "manual"),

        conditionalPanel(
          condition = paste0("input['", ns("meta_assignment_method"), "'] == 'delimiter'"),
          fluidRow(
            column(6, textInput(ns("meta_delimiter"), "Delimiter:", value = "_")),
            column(6, selectInput(ns("meta_position"), "Use part:", 
                                  choices = list("Before" = "before", "After" = "after")))
          ),
          verbatimTextOutput(ns("meta_preview")),
          actionButton(ns("apply_delimiter_meta"), "Apply Delimiter", class = "btn-success btn-sm")
        )
      ),
      uiOutput(ns("corpus_metadata_container"))
    ),

    # 3. Paste Text Panel ----
    conditionalPanel(
      condition = paste0("input['", ns("input_type"), "'] == 'paste'"),
      div(title = "Each line is treated as a separate text unit.",
        textAreaInput(ns("pasted_text"), "Input Text Sample",
                      rows = 15,
                      width = "100%",
                      value = "This sample text demonstrates various linguistic features for corpus analysis. Notice the frequency distribution of function words like 'the,' 'and,' 'of' compared to content words.

Corpus linguistics is the empirical study of language based on large, structured collections of texts, enabling quantitative and qualitative analysis of real-world language use.

All the world's a stage, and all the men and women merely players. They have their exits and their entrances, and one man in his time plays many parts.

It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness.

There are only 10 types of people in the world: those who understand binary and those who don't.

How do you get 4 elephants into a Mini Cooper? Two in the front and two in the back. How do you know if 4 elephants have gone to church? There's a Mini Cooper parked outside.",
                      placeholder = "Type or paste your corpus text here...")
      )
    ),

    # 4. CSV Column Selection ----
    uiOutput(ns("column_selection_ui"))
  )
}