# =============================================================================
# COMPLETE app.R File ----
# =============================================================================

# 1. Libraries & Dependencies ----
library(shiny)
library(DT)
library(quanteda)
library(quanteda.textstats)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(tidyr)
library(tibble)
library(data.table)
library(tidyselect)
library(purrr)
library(memoise)  # For caching expensive operations
library(cachem)   # For memory caching
library(digest)   # For hashing

# 2. Global Configurations ----
options(
  shiny.maxRequestSize = 100*1024^2,  # 100MB upload limit
  shiny.sanitize.errors = TRUE,
  repos = c(CRAN = "https://cran.rstudio.com/")
)

# 3. Static Data & Caching ----

#### Cache Initialization ####
STATIC_DATA_CACHE <- cachem::cache_mem(max_size = 300 * 1024^2)  # 300MB cache
STOPWORDS_CACHE <- list()

#### Stopword Caching Function ####
get_stopwords_cached <- function(language) {
  if (is.null(STOPWORDS_CACHE[[language]])) {
    cat("Caching stopwords for language:", language, "\n")
    STOPWORDS_CACHE[[language]] <<- quanteda::stopwords(language)
  }
  return(STOPWORDS_CACHE[[language]])
}

cat("Stopword caching function loaded\n")

# 4. Frequency Data Pre-loading ----
cat("Loading and optimizing frequency data at startup...\n")

UNIFIED_FREQ_DF <- tryCatch({
  if (file.exists("wordFreq.csv")) {
    cat("Loading wordFreq.csv...\n")
    freq_data <- data.table::fread("wordFreq.csv", encoding = "UTF-8")
    freq_data <- as.data.frame(freq_data)
    
    expected_cols <- c("token", "headword", "tokenRank", "headRank",
                       "tokenBand", "headBand", "tokenFreq", "headFreq", "PoS")

    missing_cols <- setdiff(expected_cols, names(freq_data))
    if (length(missing_cols) > 0) {
      cat("Warning: Missing expected columns:", paste(missing_cols, collapse = ", "), "\n")
    }

    freq_data <- freq_data %>%
      mutate(
        token = tolower(as.character(token)),
        headword = tolower(as.character(headword)),
        tokenRank = as.integer(tokenRank),
        headRank = as.integer(headRank),
        tokenBand = as.factor(tokenBand),
        headBand = as.factor(headBand),
        tokenFreq = as.integer(tokenFreq),
        headFreq = as.integer(headFreq),
        PoS = as.factor(PoS)
      )
    cat("Loaded and optimized", nrow(freq_data), "frequency entries\n")
    freq_data
  } else {
    cat("Error: wordFreq.csv not found.\n")
    NULL
  }
}, error = function(e) {
  cat("Error loading frequency data:", e$message, "\n")
  NULL
})

#### PoS Examples ####
POS_EXAMPLES_DF <- tryCatch({
  if (file.exists("PoS_examples.csv")) {
    cat("Loading PoS_examples.csv...\n")
    pos_data <- readr::read_csv("PoS_examples.csv", show_col_types = FALSE) %>%
      mutate(
        PoS = as.character(PoS),
        category = as.character(category),
        examples = as.character(examples)
      )
    cat("Loaded", nrow(pos_data), "PoS entries\n")
    pos_data
  } else {
    cat("PoS_examples.csv not found\n")
    NULL
  }
}, error = function(e) {
  cat("Error loading PoS examples:", e$message, "\n")
  NULL
})

#### Spelling Pairs ####
SPELLING_PAIRS <- tryCatch({
  if (file.exists("US_UK_spelling.csv")) {
    cat("Loading US_UK_spelling.csv...\n")
    spelling_data <- readr::read_csv("US_UK_spelling.csv", show_col_types = FALSE) %>%
      mutate(US = tolower(US), UK = tolower(UK)) %>%
      filter(!is.na(US), !is.na(UK), US != "", UK != "")
    cat("Loaded", nrow(spelling_data), "spelling pairs\n")
    spelling_data
  } else {
    cat("US_UK_spelling.csv not found\n")
    NULL
  }
}, error = function(e) {
  cat("Error loading spelling pairs:", e$message, "\n")
  NULL
})

# 5. External Module Sourcing ----

#### UI Modules ####
source("modules/ui_data_input.R")
source("modules/ui_tokenization.R")
source("modules/ui_kwic.R")
source("modules/ui_frequency.R")
source("modules/ui_keyword.R")
source("modules/ui_pos.R")
source("modules/ui_dispersion.R")
source("modules/ui_tagging.R")
source("modules/ui_references.R")

#### Server Modules ####
source("modules/server_data_input.R")
source("modules/server_tokenization.R")
source("modules/server_kwic.R")
source("modules/server_frequency.R")
source("modules/server_keyword.R")
source("modules/server_tagging.R")
source("modules/server_visualization.R")
source("modules/server_pos.R")
source("modules/server_dispersion.R")

#### Utility Scripts ####
source("utils/data_utils.R")
source("utils/frequency_utils.R")
source("utils/plot_utils.R")
source("utils/summary_utils.R")
source("utils/tagged_conversion.R") 

# 6. UI DEFINITION ----
ui <- fluidPage(
  title = "Corpus Analysis Tool",

  # CSS Styles
  tags$head(
    tags$style(HTML("
      .performance-indicator { background-color: #e3f2fd; border-left: 4px solid #1976d2; padding: 8px 12px; margin: 5px 0; font-size: 12px; border-radius: 3px; }
      .memory-warning { background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 8px 12px; margin: 5px 0; font-size: 12px; border-radius: 3px; }
      .feature-info { background-color: #e8f5e8; border-left: 4px solid #4caf50; padding: 8px 12px; margin: 5px 0; font-size: 12px; border-radius: 3px; }
    "))
  ),

  sidebarLayout(
    
    # Sidebar Panel ----
    sidebarPanel(
      width = 3,

      # Logo/Branding
      div(
        style = "text-align: center; margin-bottom: 20px; padding: 15px; background: linear-gradient(135deg, #1976d2 0%, #42a5f5 100%); border-radius: 8px; color: white; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
        h3("OCTOBOX", style = "margin: 0; font-weight: 700; font-size: 24px; letter-spacing: 2px;"),
        h5("Online Corpus Toolbox", style = "margin: 5px 0 0 0; font-weight: 400; font-size: 14px; letter-spacing: 0.5px;")
      ),

      # Features Indicator
      conditionalPanel(
        condition = "output.data_features_available",
        div(class = "feature-info", HTML("✓ <strong>Features Active:</strong> Frequency database active"))
      ),

      # Data Input
      div(
        style = "border-bottom: 1px solid #dee2e6; padding-bottom: 15px; margin-bottom: 15px;",
        h5("📁 Data Input", style = "color: #495057; margin-top: 0;"),
        dataInputUI("data_input")
      ),

      # Data Preview
      conditionalPanel(
        condition = "output.show_preview",
        div(
          style = "border-bottom: 1px solid #dee2e6; padding-bottom: 15px; margin-bottom: 15px;",
          h5("📋 Data Preview", style = "color: #495057; margin-top: 0;"),
          div(style = "max-height: 200px; overflow-y: auto; font-size: 12px;", tableOutput("file_preview"))
        )
      ),

      # Meta Filter
      div(
        style = "border-bottom: 1px solid #dee2e6; padding-bottom: 15px; margin-bottom: 15px;",
        h5("🏷️ Meta Filter", style = "color: #495057;"),
        conditionalPanel(
          condition = "output.meta_groups_count > 20",
          div(class = "memory-warning", HTML("⚠️ <strong>Performance:</strong> >20 groups detected."))
        ),
        p("Filter your corpus by document categories:", style = "font-size: 13px; color: #6c757d; margin-bottom: 10px;"),
        div(
          style = "margin-bottom: 12px;",
          actionButton("select_all_meta", "Select All", class = "btn-outline-primary btn-sm", style = "margin-right: 5px;"),
          actionButton("deselect_all_meta", "Deselect All", class = "btn-outline-secondary btn-sm")
        ),
        checkboxGroupInput("meta_filter_global", "Filter by Meta Group:", choices = NULL)
      ),

      # Tokenization Sidebar
      div(
        h5("⚙️ Tokenization", style = "color: #495057;"),
        p("Process your text into tokens:", style = "font-size: 13px; color: #6c757d; margin-bottom: 10px;"),
        tokenizationUI("tokenization")
      )
    ),

    # Main Panel Tabs ----
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        type = "tabs",

        # Tab 1: Summary
        tabPanel("📊 Corpus Summary",
                 div(style = "padding: 20px;",
                     h3("Corpus Overview", style = "color: #495057;"),
                     p("Basic statistics and Type-Token Ratios (TTR).", style = "color: #6c757d; margin-bottom: 20px;"),
                     conditionalPanel(
                       condition = "output.corpus_size_large",
                       div(class = "performance-indicator", HTML("📊 <strong>Large Corpus Detected:</strong> optimized"))
                     ),
                     div(style = "margin-bottom: 20px;",
                       fluidRow(
                         column(6, radioButtons("token_summary-summary_view", "Summary View:", choices = c("Per Text" = "text", "By Meta Group" = "meta", "Whole Corpus" = "corpus"), inline = TRUE)),
                         column(6, br(), downloadButton("token_summary-download_token_summary", "📥 Download Summary", class = "btn-outline-primary"))
                       )
                     ),
                     div(style = "margin: 20px 0;",
                       tags$details(
                         tags$summary(style = "cursor: pointer; font-weight: bold; color: #2e7d32; padding: 10px; background-color: #e8f5e8; border-radius: 3px;", "📈 Statistical Measures Explained"),
                         div(style = "padding: 15px; background-color: #e8f5e8; border-radius: 0 0 5px 5px; border-top: 1px solid #c8e6c8;",
                           tags$ul(
                             tags$li(HTML("<strong>TTR:</strong> Vocabulary richness.")),
                             tags$li(HTML("<strong>Sample TTR-400:</strong> Normalized richness."))
                           )
                         )
                       )
                     ),
                     DT::dataTableOutput("token_summary-token_summary")
                 )
        ),

        # Tab 2: Frequency
        tabPanel("📈 Frequency Analysis",
                 div(style = "padding: 20px;",
                     conditionalPanel(condition = "output.data_features_available", div(class = "feature-info", HTML("✓ <strong>Database Active</strong>"))),
                     conditionalPanel(condition = "!output.data_features_available", div(class = "memory-warning", HTML("⚠️ <strong>Basic Mode</strong>"))),
                     frequencyUI("frequency")
                 )
        ),

        # Tab 3: PoS
        tabPanel("📝 Part-of-Speech",
                 div(style = "padding: 20px;",
                     h3("Part-of-Speech Distribution"),
                     p("Analyze grammatical categories.", style = "color: #6c757d; margin-bottom: 20px;"),
                     posUI("pos_analysis")
                 )
        ),

        # Tab 4: Dispersion
        tabPanel("📍 Dispersion", div(style = "padding: 20px;", dispersionUI("dispersion"))),

        # Tab 5: Keyword
        tabPanel("🎯 Keyword Analysis", div(style = "padding: 20px;", keywordUI("keyword"))),

        # Tab 6: KWIC
        tabPanel("🔍 KWIC & Collocation",
                 fluidPage(
                   fluidRow(column(12, div(style = "background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 8px; padding: 20px; margin-bottom: 20px;", kwicUI("kwic")))),
                   fluidRow(column(12, div(style = "background: #ffffff; border: 1px solid #dee2e6; border-radius: 8px; padding: 25px; margin-bottom: 20px;", h4("📄 KWIC Results"), kwicResultsUI("kwic_results")))),
                   fluidRow(column(12, div(style = "background: #ffffff; border: 1px solid #dee2e6; border-radius: 8px; padding: 25px; margin-bottom: 20px;", collocationUI("collocation"))))
                 )
        ),
# Tab 7: Tagging
tabPanel("POS Tagging", icon = icon("tags"), taggingUI("tagging")),

# Tab 8: References
tabPanel("References", 
         icon = icon("book"),
         referencesUI("references"))
      )  # Close tabsetPanel
    )    # Close mainPanel
  )      # Close sidebarLayout
)        # Close fluidPage

# 7. SERVER DEFINITION ----
server <- function(input, output, session) {

  # Performance Monitoring ----
  performance_metrics <- reactiveValues(
    cache_hits = 0,
    cache_misses = 0,
    optimizations_applied = 0
  )

  # State Management & Shared Values ----
  shared_values <- reactiveValues(
    has_tagged_tokens = FALSE,
    tagged_tokens = NULL,
    tagged_df = NULL
  )
  
  values <- reactiveValues(
    token_data = NULL,
    unified_freq_df = UNIFIED_FREQ_DF,
    pos_examples_df = POS_EXAMPLES_DF,
    spelling_pairs = SPELLING_PAIRS,
    current_data_signature = NULL,
    processing_large_corpus = FALSE
  )

  # Data Diagnostics Observer ----
  observe({
    if (is.null(values$unified_freq_df)) {
      cat("ERROR: unified_freq_df is NULL. Attempting reload...\n")
      freq_data <- NULL
      if (file.exists("wordFreq.csv")) {
        freq_data <- tryCatch({
          data <- readr::read_csv("wordFreq.csv", show_col_types = FALSE)
          data %>% mutate(across(c(token, headword, PoS, tokenBand, headBand), as.character),
                          across(c(tokenRank, headRank, tokenFreq, headFreq), as.integer))
        }, error = function(e) { NULL })
      }
      if (!is.null(freq_data)) values$unified_freq_df <- freq_data
    }
  })

  # UI Status Indicators ----
  output$data_features_available <- reactive({
    !is.null(values$unified_freq_df) && !is.null(values$pos_examples_df)
  })
  outputOptions(output, "data_features_available", suspendWhenHidden = FALSE)

  output$meta_groups_count <- reactive({
    data <- safe_text_and_meta()
    if (is.null(data)) return(0)
    length(unique(data$meta))
  })
  outputOptions(output, "meta_groups_count", suspendWhenHidden = FALSE)

  output$corpus_size_large <- reactive({
    data <- safe_text_and_meta()
    if (is.null(data)) return(FALSE)
    length(data$text) > 1000 || sum(nchar(data$text)) > 100000
  })
  outputOptions(output, "corpus_size_large", suspendWhenHidden = FALSE)

  # Data Input Processing ----
  data_input_return <- dataInputServer("data_input")

safe_text_and_meta <- reactive({
    tryCatch({
      data <- data_input_return$selected_text_and_meta()
      if (is.null(data) || is.null(data$text)) return(NULL)
      
      # FIX: Filter both vectors together using the same index
      valid_idx <- !is.na(data$text) & !is.na(data$meta)
      text_clean <- data$text[valid_idx]
      meta_clean <- data$meta[valid_idx]
      
      list(text = text_clean, meta = meta_clean)
    }, error = function(e) NULL)
  })

  # Sidebar Previews & Filters ----
  output$show_preview <- reactive({ !is.null(data_input_return$uploaded_data()) })
  outputOptions(output, "show_preview", suspendWhenHidden = FALSE)

  output$file_preview <- renderTable({
    req(data_input_return$uploaded_data())
    data <- data_input_return$uploaded_data()
    if (data$type %in% c("txt", "paste")) {
      head(data.frame(Line = seq_along(data$content), Text = substr(data$content, 1, 100)), 5)
    } else if (data$type == "corpus") {
      head(data.frame(File = sapply(data$file_info, function(x) x$filename), Lines = sapply(data$file_info, function(x) length(x$content))), 10)
    } else head(data$content, 5)
  })

  observe({
    data <- safe_text_and_meta()
    req(data$meta)
    choices <- unique(data$meta[data$meta != ""])
    updateCheckboxGroupInput(session, "meta_filter_global", choices = choices, selected = choices)
  })

  observeEvent(input$select_all_meta, {
    choices <- unique(safe_text_and_meta()$meta)
    updateCheckboxGroupInput(session, "meta_filter_global", selected = choices)
  })
  observeEvent(input$deselect_all_meta, { updateCheckboxGroupInput(session, "meta_filter_global", selected = character(0)) })

  safe_meta_filter <- reactive({ input$meta_filter_global })

  # Module Server Calls ----
  tokenization_return <- tokenizationServer("tokenization", safe_text_and_meta, safe_meta_filter, values)
  tokenSummaryServer("token_summary", tokenization_return$token_data, safe_meta_filter)
  
# KWIC Module
kwic_return <- kwicServer(
  "kwic",
  tokenization_return$token_data,
  safe_meta_filter,
  tagged_data = reactive({
    list(
      available = shared_values$has_tagged_tokens,
      df = shared_values$tagged_df
    )
  })
)
  kwicResultsServer("kwic_results", kwic_return)
  collocationServer("collocation", kwic_return)
  
  frequencyServer("frequency", tokenization_return$token_data, safe_meta_filter, values, 
                  tagged_data = reactive({ list(available = shared_values$has_tagged_tokens, df = shared_values$tagged_df) }))

  posServer("pos_analysis", tokenization_return$token_data, safe_meta_filter, values,
            tagged_data = reactive({ list(available = shared_values$has_tagged_tokens, df = shared_values$tagged_df) }))

# Keyword Module
keywordServer(
  "keyword",
  tokenization_return$token_data,
  safe_text_and_meta,
  safe_meta_filter,
  tagged_data = reactive({  # ← ADD THIS
    list(
      available = shared_values$has_tagged_tokens,
      df = shared_values$tagged_df
    )
  })
)
# Dispersion Module
dispersionServer(
  "dispersion",
  tokenization_return$token_data,
  safe_meta_filter,
  tagged_data = reactive({  # ← ADD THIS
    list(
      available = shared_values$has_tagged_tokens,
      df = shared_values$tagged_df
    )
  })
)

taggingServer("tagging", tokenization_return$token_data, tokenization_return$meta_filter, shared_values = shared_values) 

  # Housekeeping & Cleanup ----
  observe({
    invalidateLater(30000)
    if (values$processing_large_corpus) gc()
  })

  session$onSessionEnded(function() { gc() })
}

# 8. Startup Sequence & Execution ----
cat("=== CORPUS ANALYSIS TOOL ===\n")
# ... (Startup messages preserved)
shinyApp(ui = ui, server = server)
