# =============================================================================
# modules/server_data_input.R ----
# =============================================================================

dataInputServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    # 1. Reactive Values & State ----
    # Store corpus metadata assignments
    corpus_metadata <- reactiveVal(NULL)

    # 2. Data Ingestion Logic ----
    # Process uploaded data
    uploaded_data <- reactive({
      if (input$input_type == "paste") {
        req(input$pasted_text)
        return(process_pasted_text(input$pasted_text))
      }

     if (input$input_type == "file") {
  req(input$upload_csv)  # ← Changed from input$upload
  result <- read_uploaded_file(input$upload_csv, skip_rows = input$skip_rows %||% 0)  # ← Add skip_rows
        if (!is.null(result)) {
          cat("File uploaded successfully. Type:", result$type, "\n")
          if (result$type == "csv") {
            cat("CSV columns:", paste(names(result$content), collapse = ", "), "\n")
          }
        } else {
          cat("File upload failed or returned NULL\n")
        }
        return(result)
      }

      if (input$input_type == "corpus") {
        req(input$upload_corpus)
        metadata_assignments <- corpus_metadata()
        return(read_corpus_files(input$upload_corpus, metadata_assignments))
      }

      return(NULL)
    })

    # 3. Dynamic UI Rendering: Metadata Container ----
    output$corpus_metadata_container <- renderUI({
      cat("Checking corpus metadata container conditions...\n")
      cat("Input type:", input$input_type, "\n")

      # Only show for corpus uploads
      if (input$input_type != "corpus") {
        cat("Not corpus input, returning NULL\n")
        return(NULL)
      }

      # Check if files are uploaded
      if (is.null(input$upload_corpus)) {
        cat("No corpus files uploaded yet\n")
        return(NULL)
      }

      if (nrow(input$upload_corpus) == 0) {
        cat("Corpus upload is empty\n")
        return(NULL)
      }

      cat("Showing corpus metadata UI for", nrow(input$upload_corpus), "files\n")

      files <- input$upload_corpus

      # Create the complete metadata assignment UI
      tagList(
        br(),
        div(
          style = "border: 1px solid #ddd; padding: 15px; border-radius: 5px; background-color: #f9f9f9;",
          h6("📝 Metadata Assignment", style = "color: #333; margin-top: 0;"),
          p("Assign category labels to your files. You can:",
            style = "margin-bottom: 10px; font-size: 14px;"),
          tags$ul(
            tags$li("Use the same label for multiple files (e.g., 'BBC' for all BBC articles)"),
            tags$li("Create different categories (e.g., 'BBC', 'WaPo', 'JTimes')"),
            tags$li("Use descriptive names (e.g., 'Academic', 'News', 'Fiction')")
          ),

          # Quick assignment tools
          div(
            style = "margin-bottom: 15px; padding: 10px; background-color: #e8f4f8; border-radius: 3px;",
            h6("🚀 Quick Assignment Tools:", style = "margin-top: 0; color: #2c5aa0;"),

            fluidRow(
              column(4,
                textInput(session$ns("bulk_meta_label"), "Bulk Label:",
                          placeholder = "e.g., BBC, News, Academic")
              ),
              column(4,
                numericInput(session$ns("bulk_start"), "From file #:",
                           value = 1, min = 1, step = 1)
              ),
              column(4,
                numericInput(session$ns("bulk_end"), "To file #:",
                           value = 1, min = 1, step = 1)
              )
            ),

            div(style = "text-align: center; margin-top: 10px;",
              actionButton(session$ns("apply_bulk"), "Apply to Range",
                          class = "btn-info btn-sm"),
              actionButton(session$ns("apply_all"), "Apply to All",
                          class = "btn-warning btn-sm")
            )
          ),

          # Individual file assignment
          h6("📁 Individual File Assignments:", style = "color: #333; margin-bottom: 10px;"),
          div(
            style = "max-height: 300px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; border-radius: 3px;",

            # Create input fields for each file
            lapply(1:nrow(files), function(i) {
              filename_base <- tools::file_path_sans_ext(files$name[i])

              div(
                style = "margin-bottom: 8px; padding: 8px; border: 1px solid #e0e0e0; border-radius: 3px;",
                fluidRow(
                  column(1,
                    div(style = "text-align: center; font-weight: bold; color: #666; padding-top: 5px;",
                        paste0("#", i))
                  ),
                  column(5,
                    div(style = "font-size: 13px; color: #333; padding-top: 5px; word-break: break-all;",
                        files$name[i])
                  ),
                  column(6,
                    textInput(
                      session$ns(paste0("meta_", i)),
                      label = NULL,
                      value = filename_base,
                      placeholder = "Enter category/group name"
                    )
                  )
                )
              )
            })
          ),

          br(),
          div(style = "text-align: center;",
            actionButton(session$ns("update_metadata"), "✅ Confirm Metadata Assignments",
                        class = "btn-success btn-sm"),
            br(), br(),
            actionButton(session$ns("reset_metadata"), "🔄 Reset to Filenames",
                        class = "btn-outline-secondary btn-sm")
          )
        )
      )
    })

    # 4. Metadata Assignment Observers ----
    
    # Bulk Apply Range Logic
    observeEvent(input$apply_bulk, {
      req(input$upload_corpus, input$bulk_meta_label, input$bulk_start, input$bulk_end)

      if (nchar(trimws(input$bulk_meta_label)) == 0) {
        showNotification("Please enter a label for bulk assignment", type = "warning")
        return()
      }

      files <- input$upload_corpus
      start_idx <- max(1, min(input$bulk_start, nrow(files)))
      end_idx <- max(start_idx, min(input$bulk_end, nrow(files)))

      for (i in start_idx:end_idx) {
        updateTextInput(session, paste0("meta_", i), value = trimws(input$bulk_meta_label))
      }

      showNotification(
        paste("Applied label '", trimws(input$bulk_meta_label), "' to files", start_idx, "to", end_idx),
        type = "message"
      )
    })

    # Apply All Logic
    observeEvent(input$apply_all, {
      req(input$upload_corpus, input$bulk_meta_label)

      if (nchar(trimws(input$bulk_meta_label)) == 0) {
        showNotification("Please enter a label for bulk assignment", type = "warning")
        return()
      }

      files <- input$upload_corpus
      for (i in 1:nrow(files)) {
        updateTextInput(session, paste0("meta_", i), value = trimws(input$bulk_meta_label))
      }

      showNotification(
        paste("Applied label '", trimws(input$bulk_meta_label), "' to all", nrow(files), "files"),
        type = "message"
      )
    })

    # Reset Logic
    observeEvent(input$reset_metadata, {
      req(input$upload_corpus)
      files <- input$upload_corpus
      for (i in 1:nrow(files)) {
        filename_base <- tools::file_path_sans_ext(files$name[i])
        updateTextInput(session, paste0("meta_", i), value = filename_base)
      }
      showNotification("Reset all labels to original filenames", type = "message")
    })

    # Confirm Assignments Logic
    observeEvent(input$update_metadata, {
      req(input$upload_corpus)
      files <- input$upload_corpus
      assignments <- character(nrow(files))

      for (i in 1:nrow(files)) {
        meta_value <- input[[paste0("meta_", i)]]
        if (!is.null(meta_value) && nchar(trimws(meta_value)) > 0) {
          assignments[i] <- trimws(meta_value)
        } else {
          assignments[i] <- tools::file_path_sans_ext(files$name[i])
        }
      }

      corpus_metadata(assignments)
      assignment_summary <- table(assignments)
      summary_text <- paste(
        "Metadata confirmed!",
        paste(paste(names(assignment_summary), "(", assignment_summary, "files)"), collapse = ", ")
      )
      showNotification(summary_text, type = "message", duration = 5)
    })

    # 5. CSV Column Selection Logic ----
    
    # Render CSV Mapping UI
    output$column_selection_ui <- renderUI({
      if (input$input_type != "file") return(NULL)
      if (is.null(uploaded_data())) return(NULL)
      if (uploaded_data()$type != "csv") return(NULL)

      columns <- names(uploaded_data()$content)
      df <- uploaded_data()$content

      column_info <- sapply(columns, function(col) {
        length(unique(df[[col]]))
      })

      high_cardinality_cols <- names(column_info)[column_info > 20]

      tagList(
        h5("Column Selection"),
        p("Choose which columns contain your text and metadata:"),

        selectInput(session$ns("text_column"),
                    "Choose Text Column:",
                    choices = columns,
                    selected = columns[1]),

        div(
          conditionalPanel(
            condition = paste0("input['", session$ns("text_column"), "'] != null"),
            {
              selectInput(session$ns("meta_column"),
                          "Choose Metadata Column(s):",
                          choices = NULL,
                          multiple = TRUE,
                          selected = NULL)
            }
          )
        ),

        if (length(high_cardinality_cols) > 0) {
          div(
            style = "margin-top: 10px; padding: 10px; background-color: #fff3cd; border: 1px solid #ffeaa7; border-radius: 4px;",
            HTML(paste0(
              "<strong>⚠️ Performance Warning:</strong><br/>",
              "The following columns have more than 20 unique values and may slow down the app:<br/>",
              "<strong>", paste(high_cardinality_cols, " (", column_info[high_cardinality_cols], " values)", collapse = ", "), "</strong><br/>",
              "Consider using columns with fewer categories for better performance."
            ))
          )
        } else {
          div(
            style = "margin-top: 10px; padding: 8px; background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 4px;",
            HTML("<strong>✅ Good:</strong> All columns have ≤20 unique values.")
          )
        }
      )
    })

    # Observer: Sync Meta Choices with Text Column Selection
    observe({
      if (input$input_type != "file") return()
      if (is.null(uploaded_data())) return()
      if (uploaded_data()$type != "csv") return()
      if (is.null(input$text_column)) return()

      all_columns <- names(uploaded_data()$content)
      df <- uploaded_data()$content
      available_meta_columns <- setdiff(all_columns, input$text_column)

      if (length(available_meta_columns) > 0) {
        meta_column_info <- sapply(available_meta_columns, function(col) {
          length(unique(df[[col]]))
        })

        meta_choices <- setNames(
          available_meta_columns,
          paste0(available_meta_columns, " (", meta_column_info, " unique values)")
        )

        updateSelectInput(session, "meta_column", choices = meta_choices, selected = NULL)
        cat("Updated meta column choices. Text column:", input$text_column, "\n")
      } else {
        updateSelectInput(session, "meta_column", choices = NULL, selected = NULL)
        showNotification("All columns used for text. No metadata columns available.", type = "warning")
      }
    })

    # 6. Final Data Assembly ----
    
    # Compile selected text and metadata into final object
    selected_text_and_meta <- reactive({
      req(uploaded_data())

      if (uploaded_data()$type %in% c("paste", "txt", "corpus")) {
        return(list(
          text = uploaded_data()$content,
          meta = uploaded_data()$metadata
        ))
      } else if (uploaded_data()$type == "csv") {
        req(input$text_column)
        df <- uploaded_data()$content
        text_data <- as.character(df[[input$text_column]])

        meta_data <- if (!is.null(input$meta_column) && length(input$meta_column) > 0) {
          apply(df[, input$meta_column, drop = FALSE], 1, paste, collapse = " | ")
        } else {
          rep("unknown", length(text_data))
        }

        return(list(text = text_data, meta = meta_data))
      }
    })

    # 7. Filename Delimiter Logic ----
    
    # Meta assignment preview (Regex/Delimiter)
    output$meta_preview <- renderText({
      req(input$upload_corpus, input$meta_assignment_method == "delimiter")

      files <- input$upload_corpus
      filenames <- files$name
      delimiter <- input$meta_delimiter %||% "_"
      position <- input$meta_position %||% "before"

      meta_groups <- sapply(filenames, function(filename) {
        name_no_ext <- tools::file_path_sans_ext(filename)
        if (grepl(delimiter, name_no_ext, fixed = TRUE)) {
          parts <- strsplit(name_no_ext, delimiter, fixed = TRUE)[[1]]
          if (position == "before") return(parts[1]) else return(parts[length(parts)])
        } else {
          return(name_no_ext)
        }
      })

      preview_lines <- paste0(filenames, " → ", meta_groups)
      preview_text <- paste(head(preview_lines, 10), collapse = "\n")
      if (length(filenames) > 10) preview_text <- paste0(preview_text, "\n... (", length(filenames) - 10, " more)")
      return(preview_text)
    })

    # Apply Delimiter Logic to inputs
    observeEvent(input$apply_delimiter_meta, {
      req(input$upload_corpus, input$meta_assignment_method == "delimiter")

      files <- input$upload_corpus
      delimiter <- input$meta_delimiter %||% "_"
      position <- input$meta_position %||% "before"

      for (i in 1:nrow(files)) {
        filename <- files$name[i]
        name_no_ext <- tools::file_path_sans_ext(filename)
        if (grepl(delimiter, name_no_ext, fixed = TRUE)) {
          parts <- strsplit(name_no_ext, delimiter, fixed = TRUE)[[1]]
          meta_value <- if (position == "before") parts[1] else parts[length(parts)]
        } else {
          meta_value <- name_no_ext
        }
        updateTextInput(session, paste0("meta_", i), value = meta_value)
      }

      Sys.sleep(0.5)
      showNotification("Delimiter assignment applied successfully.", type = "message")
    })

    # 8. Module Returns ----
    # Return reactive values (KEEP AT THE VERY END)
    return(list(
      uploaded_data = uploaded_data,
      selected_text_and_meta = selected_text_and_meta
    ))
  })
}