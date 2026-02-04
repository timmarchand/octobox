# =============================================================================
# COMPLETE PoS SERVER - UDPipe & Pattern-Based Integration ----
# =============================================================================

posServer <- function(id, token_data, meta_filter, values, tagged_data = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Tagged Data Availability Check ----
    output$tagged_available <- reactive({
      !is.null(tagged_data) && !is.null(tagged_data()) && tagged_data()$available
    })
    outputOptions(output, "tagged_available", suspendWhenHidden = FALSE)
    
    # 2. Reactive Storage ----
    pos_charts_generated <- reactiveVal(FALSE)
    pos_stored_data <- reactiveVal(NULL)
    
    pos_data <- reactive({
      pos_stored_data()
    })
    
    # 3. Combined POS Analysis Engine ----
    observeEvent(input$run_and_generate, {
      
      use_udpipe <- input$use_udpipe_pos %||% FALSE
      
      withProgress(message = 'Analyzing POS distribution...', value = 0, {
        
         pos_current_data <- NULL
        
        if (use_udpipe) {
          # 3a. UDPipe-Based Analysis ----
          req(tagged_data())
          req(tagged_data()$df)
          
          cat("Using UDPipe tagged data for POS analysis\n")
          
          incProgress(0.2, detail = "Loading tagged data...")
          
          df <- tagged_data()$df
          
          # Apply meta filter
          filter_meta <- meta_filter()
          if (!is.null(filter_meta) && length(filter_meta) > 0) {
            df <- df[df$meta %in% filter_meta, ]
          }
          
          if (nrow(df) == 0) {
            showNotification("No data after filtering", type = "error")
            return()
          }
          
          incProgress(0.4, detail = "Aggregating POS tags...")
          
          # Choose tag system
          tag_col <- if (input$pos_tag_system %||% "xpos" == "upos") "upos" else "xpos"
          
    # Aggregate by POS tag and meta
pos_summary <- df %>%
  dplyr::group_by(meta, !!rlang::sym(tag_col)) %>%
  dplyr::summarize(count = dplyr::n(), .groups = "drop")

# Check that meta column exists
if (!"meta" %in% names(pos_summary)) {
  cat("ERROR: meta column missing after aggregation!\n")
  showNotification("Error: Meta information lost during analysis", type = "error")
  return()
}

# Rename tag column to standardized_pos
names(pos_summary)[names(pos_summary) == tag_col] <- "standardized_pos"

# Add percentages
pos_summary <- pos_summary %>%
  dplyr::group_by(meta) %>%
  dplyr::mutate(
    percentage = round(count / sum(count) * 100, 2)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(meta, desc(count))

# Verify meta column still exists
cat("UDPipe analysis - columns:", paste(names(pos_summary), collapse = ", "), "\n")
cat("UDPipe analysis - meta values:", paste(unique(pos_summary$meta), collapse = ", "), "\n")

      pos_current_data <- pos_summary 

        } 
         
         
         
         else {
          # 3b. Pattern-Based Analysis ----
          cat("Using pattern-based POS analysis\n")
          
          incProgress(0.2, detail = "Validating data...")
          
          # Check token data
          token_data_obj <- token_data()
          if (is.null(token_data_obj)) {
            showNotification("No token data - run tokenization first", type = "error")
            return()
          }
          
          # Check frequency data
          if (is.null(values$unified_freq_df)) {
            showNotification("No frequency database available for PoS analysis", type = "error")
            return()
          }
          
          incProgress(0.3, detail = "Processing tokens...")
          
          toks <- token_data_obj
          meta_all <- quanteda::docvars(toks, "meta")
          filter_meta <- meta_filter()
          
          # Apply meta filter
          if (!is.null(filter_meta)) {
            keep <- meta_all %in% filter_meta
            if (sum(keep) == 0) {
              showNotification("No documents after filtering", type = "error")
              return()
            }
            toks <- toks[keep]
            meta_all <- meta_all[keep]
          }
          
          # Convert tokens to data frame
          tokens_list <- as.list(toks)
          token_df_list <- list()
          
          for (i in seq_along(tokens_list)) {
            if (length(tokens_list[[i]]) > 0) {
              token_df_list[[i]] <- data.frame(
                token = tolower(tokens_list[[i]]),
                meta = meta_all[i],
                stringsAsFactors = FALSE
              )
            }
          }
          
          all_tokens_df <- dplyr::bind_rows(token_df_list)
          
          if (nrow(all_tokens_df) == 0) {
            showNotification("No tokens to analyze", type = "error")
            return()
          }
          
          incProgress(0.5, detail = "Joining with frequency data...")
          
          # Join with frequency data
          freq_clean <- values$unified_freq_df %>%
            dplyr::mutate(token = tolower(as.character(token))) %>%
            dplyr::group_by(token) %>%
            dplyr::slice_min(tokenRank, n = 1, with_ties = FALSE) %>%
            dplyr::ungroup() %>%
            dplyr::select(token, any_of(c("PoS", "DomPoS", "coca_PoS")))
          
          pos_data_joined <- all_tokens_df %>%
            dplyr::left_join(freq_clean, by = "token")
          
          incProgress(0.7, detail = "Standardizing POS tags...")
          
          # FIXED: Build raw_pos outside mutate to avoid column reference errors
          available_pos_cols <- intersect(names(pos_data_joined), c("PoS", "coca_PoS", "DomPoS"))
          
          if (length(available_pos_cols) > 0) {
            pos_data_joined$raw_pos <- NA_character_
            
            if ("PoS" %in% names(pos_data_joined)) {
              pos_data_joined$raw_pos <- ifelse(!is.na(pos_data_joined$PoS), 
                                                 as.character(pos_data_joined$PoS), 
                                                 pos_data_joined$raw_pos)
            }
            if ("coca_PoS" %in% names(pos_data_joined)) {
              pos_data_joined$raw_pos <- ifelse(is.na(pos_data_joined$raw_pos) & !is.na(pos_data_joined$coca_PoS), 
                                                 as.character(pos_data_joined$coca_PoS), 
                                                 pos_data_joined$raw_pos)
            }
            if ("DomPoS" %in% names(pos_data_joined)) {
              pos_data_joined$raw_pos <- ifelse(is.na(pos_data_joined$raw_pos) & !is.na(pos_data_joined$DomPoS), 
                                                 as.character(pos_data_joined$DomPoS), 
                                                 pos_data_joined$raw_pos)
            }
          } else {
            pos_data_joined$raw_pos <- NA_character_
          }
          
          # Now standardize using the raw_pos column
          pos_data_joined <- pos_data_joined %>%
            dplyr::mutate(
              standardized_pos = dplyr::case_when(
                is.na(raw_pos) ~ "OTHER",
                raw_pos == "Noun" ~ "NOUN",
                raw_pos == "Verb" ~ "VERB",
                raw_pos == "Adj" ~ "ADJ",
                raw_pos == "Adv" ~ "ADV",
                raw_pos == "Prep" ~ "PREP",
                raw_pos == "Conj" ~ "CONJ",
                raw_pos == "Detr" ~ "DET",
                raw_pos == "Pron" ~ "PRON",
                raw_pos == "Num" ~ "NUM",
                raw_pos == "Intj" ~ "INTERJ",
                raw_pos == "ArtP" ~ "ART",
                raw_pos == "AuxV" ~ "AUX",
                raw_pos == "InfM" ~ "PART",
                raw_pos == "Neg" ~ "PART",
                raw_pos == "Exst" ~ "PART",
                raw_pos == "Abbr" ~ "ABBR",
                raw_pos == "IrrN" ~ "NOUN",
                raw_pos == "Irr" ~ "OTHER",
                raw_pos == "Time" ~ "TIME",
                TRUE ~ "OTHER"
              )
            )
          
          # Filter and summarize
          pos_summary <- pos_data_joined %>%
            dplyr::filter(!is.na(standardized_pos)) %>%
            dplyr::group_by(meta, standardized_pos) %>%
            dplyr::summarise(count = n(), .groups = "drop")
          
          pos_current_data <- pos_summary
          
          cat("Pattern-based POS analysis complete:", nrow(pos_summary), "rows\n")
        
            
          pos_current_data <- pos_summary  
          }
        
        incProgress(0.9, detail = "Generating charts...")
        
        # Validate we have data
        if (is.null(pos_current_data) || nrow(pos_current_data) == 0) {
          showNotification("No POS data to display", type = "warning")
          return()
        }
        
        # Store results
        pos_stored_data(pos_current_data)
        pos_charts_generated(TRUE)
        
        incProgress(1, detail = "Complete!")
        
        showNotification(
          paste("POS analysis complete!",
                nrow(pos_current_data), "POS categories found"),
          type = "message",
          duration = 4
        )
      })
    })
    
    # 4. Chart Availability Indicator ----
    output$pos_charts_available <- reactive({
      pos_charts_generated() && !is.null(pos_data()) && nrow(pos_data()) > 0
    })
    outputOptions(output, "pos_charts_available", suspendWhenHidden = FALSE)
    
  # 5. Main POS Plot ----
output$pos_plot <- renderPlot({
  req(pos_charts_generated())
  req(pos_data())
  
  data <- pos_data()
  
  if (is.null(data) || nrow(data) == 0) {
    return(ggplot2::ggplot() +
             ggplot2::annotate("text", x = 1, y = 1,
                              label = "No PoS data available",
                              size = 6) +
             ggplot2::theme_void())
  }
  
  # Validate inputs with defaults
  top_n <- if(is.null(input$pos_top_n) || is.na(input$pos_top_n)) 8 else input$pos_top_n
  plot_type <- if(is.null(input$pos_comparison_type)) "grouped" else input$pos_comparison_type
  show_percentages <- if(is.null(input$pos_proportional)) TRUE else input$pos_proportional
  
  # Get top PoS tags
  top_pos <- data %>%
    dplyr::group_by(standardized_pos) %>%
    dplyr::summarise(total_count = sum(count), .groups = "drop") %>%
    dplyr::arrange(desc(total_count)) %>%
    dplyr::slice_head(n = top_n) %>%
    dplyr::pull(standardized_pos)
  
  plot_data <- data %>%
    dplyr::filter(standardized_pos %in% top_pos)
  
  # Check if we're using XPOS (Penn Treebank) tags
  is_penn_treebank <- any(grepl("^[A-Z]{2,4}$", plot_data$standardized_pos))
  
  # Calculate proportions if requested
  if (show_percentages) {
    plot_data <- plot_data %>%
      dplyr::group_by(meta) %>%
      dplyr::mutate(
        total_tokens = sum(count),
        percentage = (count / total_tokens) * 100
      ) %>%
      dplyr::ungroup()
    
    y_var <- "percentage"
    y_label <- "Percentage (%)"
    
    # ADD TAG SYSTEM TO TITLE
    tag_system_label <- if(is_penn_treebank) " (XPOS Tags)" else " (UPOS Tags)"
    plot_title <- paste0("Part-of-Speech Distribution (Proportional)", tag_system_label)
    
  } else {
    y_var <- "count"
    y_label <- "Count"
    
    # ADD TAG SYSTEM TO TITLE
    tag_system_label <- if(is_penn_treebank) " (XPOS Tags)" else " (UPOS Tags)"
    plot_title <- paste0("Part-of-Speech Distribution (Absolute)", tag_system_label)
  }
  
  # Create ordered factor - DON'T filter out XPOS tags
  pos_order <- c("NOUN", "VERB", "ADJ", "ADV", "PREP", "DET", "PRON",
                 "AUX", "CONJ", "NUM", "PART", "MODAL", "ART", "INTERJ", "OTHER")
  
  if (is_penn_treebank) {
    # Using XPOS - don't filter, just order by frequency
    plot_data <- plot_data %>%
      dplyr::arrange(desc(count)) %>%
      dplyr::mutate(
        standardized_pos = factor(standardized_pos,
                                 levels = unique(standardized_pos[order(-count)]))
      )
  } else {
    # Using UPOS or pattern-based - filter to standard categories
    plot_data <- plot_data %>%
      dplyr::filter(standardized_pos %in% pos_order) %>%
      dplyr::mutate(
        standardized_pos = factor(standardized_pos,
                                 levels = intersect(pos_order, unique(standardized_pos)))
      )
  }
  
  # Create plot based on selected type
  if (plot_type == "faceted") {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = standardized_pos,
                                                 y = .data[[y_var]],
                                                 fill = standardized_pos)) +
      ggplot2::geom_col(alpha = 0.8, width = 0.7) +
      ggplot2::coord_flip() +
      ggplot2::facet_wrap(~meta, scales = "free_x") +
      ggplot2::labs(
        title = plot_title,
        x = "Part-of-Speech Tag",
        y = y_label,
        caption = paste("Showing top", length(top_pos), "PoS tags per group")
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "none",
        strip.text = ggplot2::element_text(face = "bold", size = 10),
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        axis.text.y = ggplot2::element_text(size = 10),
        axis.text.x = ggplot2::element_text(size = 9)
      ) +
      ggplot2::scale_fill_viridis_d(option = "viridis", alpha = 0.8)
    
  } else {
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = standardized_pos,
                                                 y = .data[[y_var]],
                                                 fill = meta)) +
      ggplot2::geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = plot_title,
        x = "Part-of-Speech Tag",
        y = y_label,
        fill = "Category",
        caption = paste("Comparing top", length(top_pos), "PoS tags across groups")
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.title = ggplot2::element_text(face = "bold"),
        axis.text.y = ggplot2::element_text(size = 10),
        axis.text.x = ggplot2::element_text(size = 9)
      ) +
      ggplot2::scale_fill_viridis_d(option = "viridis", alpha = 0.8)
  }
  
  return(p)
})
    
    # 6. Download Handlers ----
    # 6a. Plot Download ----
    output$download_pos_plot <- downloadHandler(
      filename = function() {
        comparison_type <- input$pos_comparison_type %||% "grouped"
        type_name <- if(comparison_type == "grouped") "sidebyside" else "panels"
        paste0("pos_distribution_", type_name, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(pos_data())
        
        # ... YOUR EXISTING DOWNLOAD PLOT CODE ...
        # (Keep everything from your current download_pos_plot handler)
      }
    )
    
    # 6b. CSV Download ----
    output$download_pos_csv <- downloadHandler(
      filename = function() {
        paste0("pos_distribution_", Sys.Date(), ".csv")
      },
      content = function(file) {
        # ... YOUR EXISTING CSV DOWNLOAD CODE ...
        # (Keep everything from your current download_pos_csv handler)
      }
    )
    
    # 7. Return Values ----
    return(list(
      pos_data = pos_data,
      charts_generated = reactive({ pos_charts_generated() })
    ))
    
  })  # Close moduleServer
}  # Close posServer