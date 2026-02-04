# =============================================================================
# modules/server_visualization.R - FREQUENCY BANDS ----
# =============================================================================

#' Frequency Band Chart Server
#' @description Handles rendering of frequency distribution plots and data exports
visualizationServer <- function(id, ngram_result, freq_list_type, use_lemma_bands) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Main Plot Rendering --------------------------------------------------
    
    output$freq_band_plot <- renderPlot({
      df <- ngram_result()
      req(nrow(df) > 0)
      
      # 1.1. Column Mapping & Validation ----
      # Maps UI selection to internal dataframe columns (tokenBand vs headBand)
      freq_type_val <- freq_list_type()
      band_col      <- if(freq_type_val == "token") "tokenBand" else "headBand"
      
      # Check for data availability
      if (!band_col %in% names(df) || all(is.na(df[[band_col]]))) {
        return(ggplot2::ggplot() + 
                 ggplot2::annotate("text", x = 1, y = 1, 
                                   label = "Frequency band data not available.\nUse unigrams (n=1) and enable 'Include Frequency Info'.", 
                                   size = 5) +
                 ggplot2::theme_void())
      }
      
      # 1.2. Data Processing ----
      # Filtering 'other' and preparing band order (wordFreq_lite 01k-10k)
      band_order     <- c("01k", "02k", "03k", "04k", "05k", "06k", "07k", "08k", "09k", "10k")
      
      plot_data <- df %>%
        filter(!is.na(.data[[band_col]]), 
               .data[[band_col]] != "", 
               .data[[band_col]] != "other") %>%
        group_by(meta, band = .data[[band_col]]) %>%
        summarise(count = sum(count), .groups = "drop") %>%
        filter(band %in% band_order) %>%
        mutate(band = factor(band, levels = band_order))
      
      if (nrow(plot_data) == 0) {
        return(ggplot2::ggplot() + 
                 ggplot2::annotate("text", x = 1, y = 1, 
                                   label = "No recognized frequency bands found (01k-10k).", 
                                   size = 5) +
                 ggplot2::theme_void())
      }
      
      # 1.3. Statistical Calculations ----
      plot_data <- plot_data %>%
        group_by(meta) %>%
        mutate(total = sum(count),
               percentage = (count / total) * 100) %>%
        arrange(meta, band) %>%
        mutate(cumulative = cumsum(percentage)) %>%
        ungroup()
      
      # 1.4. Plot Generation ----
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = band, group = meta)) +
        # Bars: Density per band
        ggplot2::geom_col(ggplot2::aes(y = percentage, fill = meta), 
                          position = "dodge", alpha = 0.7, width = 0.7) +
        # Lines: Cumulative coverage
        ggplot2::geom_line(ggplot2::aes(y = cumulative, color = meta), size = 1.2) +
        ggplot2::geom_point(ggplot2::aes(y = cumulative, color = meta), size = 2.5) +
        # Formatting
        ggplot2::labs(
          title = paste("Frequency Band Distribution -", stringr::str_to_title(freq_type_val)),
          subtitle = "Bars: band percentage | Lines: cumulative coverage",
          x = "Frequency Band (Zipf-based)", y = "Percentage (%)"
        ) +
        ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(0, 100, 20)) +
        ggplot2::scale_fill_viridis_d(option = "viridis") +
        ggplot2::scale_color_viridis_d(option = "viridis") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                       legend.position = "bottom")
      
      return(p)
    })
    
    # 2. Export Handlers ------------------------------------------------------
    
    ## 2.1. Plot Download ----
    output$download_freq_plot <- downloadHandler(
      filename = function() paste0("freq_band_", freq_list_type(), "_", Sys.Date(), ".png"),
      content = function(file) {
        # Note: In a production app, consider moving plot generation 
        # to a reactive object to avoid code duplication here.
        ggplot2::ggsave(file, width = 12, height = 8, dpi = 300, bg = "white")
      }
    )

    ## 2.2. CSV Data Download ----
    output$download_freq_data <- downloadHandler(
      filename = function() paste0("freq_band_data_", Sys.Date(), ".csv"),
      content = function(file) {
        df <- ngram_result()
        req(nrow(df) > 0)
        
        # Re-run summary logic for CSV
        band_col <- if(freq_list_type() == "token") "tokenBand" else "headBand"
        
        df_summary <- df %>%
          filter(!is.na(.data[[band_col]]), .data[[band_col]] != "other") %>%
          group_by(meta, band = .data[[band_col]]) %>%
          summarise(count = sum(count, na.rm = TRUE), .groups = 'drop') %>%
          group_by(meta) %>%
          mutate(percent = count / sum(count) * 100) %>%
          ungroup()
        
        readr::write_csv(df_summary, file)
      }
    )
  })
}