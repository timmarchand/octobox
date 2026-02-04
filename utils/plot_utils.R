# =============================================================================
# PLOTTING UTILITIES - Fixed keyword plot functions
# =============================================================================

# Create simple keyword comparison plot (fallback)
create_simple_keyword_plot <- function(results, top_n) {
  # Get top features by absolute log-odds
  top_features <- results$logodds %>%
    slice_max(abs_log_odds, n = top_n) %>%
    mutate(
      direction = ifelse(log_odds > 0, "Target", "Reference")
    )
  
  ggplot(top_features, aes(x = reorder(feature, log_odds), y = log_odds, fill = direction)) +
    geom_col(alpha = 0.8) +
    scale_fill_manual(values = c("Target" = "#0033cc", "Reference" = "#d30b0d"),
                      name = "Favors") +
    coord_flip() +
    labs(title = paste("Top", nrow(top_features), "Keywords by Log-Odds"),
         subtitle = paste("Target:", paste(results$target, collapse = " + "), "vs Reference:", paste(results$reference, collapse = " + ")),
         x = "Feature",
         y = "Log-Odds Score") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5)
}

# Create keyword comparison plot with proper error handling
create_enhanced_keyword_plot <- function(results, top_n, plot_type = "both", show_counts = FALSE) {
  
  # Validate inputs
  if (is.null(results) || is.null(results$logodds) || nrow(results$logodds) == 0) {
    return(ggplot() + 
           annotate("text", x = 1, y = 1, 
                   label = "No keyword results available", 
                   size = 6) +
           theme_void())
  }
  
  logodds_data <- results$logodds
  
  # Select features based on plot type with better error handling
  tryCatch({
    if (plot_type == "both") {
      # Get top n from each direction
      target_features <- logodds_data %>%
        filter(log_odds > 0) %>%
        slice_max(log_odds, n = top_n)
      
      reference_features <- logodds_data %>%
        filter(log_odds < 0) %>%
        slice_min(log_odds, n = top_n)
      
      plot_features <- bind_rows(target_features, reference_features)
      
      if (nrow(plot_features) == 0) {
        # Fallback to overall top if no features in both directions
        plot_features <- logodds_data %>%
          slice_max(abs_log_odds, n = top_n)
      }
      
    } else if (plot_type == "target_only") {
      plot_features <- logodds_data %>%
        filter(log_odds > 0) %>%
        slice_max(log_odds, n = top_n)
      
    } else if (plot_type == "reference_only") {
      plot_features <- logodds_data %>%
        filter(log_odds < 0) %>%
        slice_min(log_odds, n = top_n)
      
    } else {  # overall_top
      plot_features <- logodds_data %>%
        slice_max(abs_log_odds, n = top_n)
    }
    
    if (nrow(plot_features) == 0) {
      return(ggplot() + 
             annotate("text", x = 1, y = 1, 
                     label = "No features available for selected plot type", 
                     size = 6) +
             theme_void())
    }
    
    # Add direction column for consistent coloring
    plot_features <- plot_features %>%
      mutate(
        direction = ifelse(log_odds > 0, "Target", "Reference")
      )
    
    # Add frequency counts if requested and available
    if (show_counts && !is.null(results$meta_freq)) {
      tryCatch({
        # Get frequency data
        target_freq <- results$meta_freq %>%
          filter(meta %in% results$target) %>%
          group_by(feature) %>%
          summarise(target_count = sum(count, na.rm = TRUE), .groups = "drop")
        
        reference_freq <- results$meta_freq %>%
          filter(meta %in% results$reference) %>%
          group_by(feature) %>%
          summarise(reference_count = sum(count, na.rm = TRUE), .groups = "drop")
        
        plot_features <- plot_features %>%
          left_join(target_freq, by = "feature") %>%
          left_join(reference_freq, by = "feature") %>%
          mutate(
            target_count = replace_na(target_count, 0),
            reference_count = replace_na(reference_count, 0),
            count_label = ifelse(log_odds > 0, 
                               paste0("(", target_count, ")"),
                               paste0("(", reference_count, ")")),
            feature_with_count = paste(feature, count_label)
          )
        
        x_var <- "feature_with_count"
        x_label <- "Feature (Count)"
      }, error = function(e) {
        # If frequency data fails, use basic feature names
        x_var <- "feature"
        x_label <- "Feature"
      })
    } else {
      x_var <- "feature"
      x_label <- "Feature"
    }
    
    # Create plot title based on type
    plot_title <- switch(plot_type,
      "both" = paste("Top", top_n, "Features Favoring Each Side"),
      "target_only" = paste("Top", top_n, "Features Favoring", paste(results$target, collapse = " + ")),
      "reference_only" = paste("Top", top_n, "Features Favoring Reference"),
      "overall_top" = paste("Top", top_n, "Features by Log-Odds Magnitude")
    )
    
    # Create the plot with fixed color mapping
    p <- ggplot(plot_features, aes(x = reorder(.data[[x_var]], log_odds), 
                                  y = log_odds, 
                                  fill = direction)) +
      geom_col(alpha = 0.8) +
      scale_fill_manual(values = c("Target" = "#0033cc", "Reference" = "#d30b0d"),
                        name = "Favors") +
      coord_flip() +
      labs(title = plot_title,
           subtitle = paste("Comparing", paste(results$target, collapse = " + "), 
                           "vs", paste(results$reference, collapse = ", ")),
           x = x_label,
           y = "Log-Odds Score") +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray50"),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 11)
      ) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5, color = "gray40")
    
    return(p)
    
  }, error = function(e) {
    cat("Plot creation error:", e$message, "\n")
    # Return simple fallback plot
    return(create_simple_keyword_plot(results, top_n))
  })
}

# Rest of the plotting utilities remain the same...
# [Include all other functions from the original plot_utils.R file]