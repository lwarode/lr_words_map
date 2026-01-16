#' Create interactive scatter plot directly with plotly (no ggplotly)
#' Used for main app to avoid blank space/resizing issues
create_plotly_scatter_plot <- function(data,
                                       selected_word = NULL,
                                       show_reference_lines = TRUE,
                                       top_n_global = 50,
                                       top_n_quadrant = 10,
                                       highlighted_words = NULL) {
  word_data <- data$word_associations
  global_stats <- data$global_stats

  # Get global top words by frequency
  global_top_words <- word_data %>%
    arrange(desc(frequency)) %>%
    slice_head(n = top_n_global) %>%
    pull(word)

  # Get quadrant-based top words
  quadrant_top_words <- c()
  if (top_n_quadrant > 0) {
    in_left <- word_data %>% filter(lr_semantic == "left", lr_position_mean < global_stats$lr_position_mean) %>% arrange(desc(frequency)) %>% slice_head(n = top_n_quadrant) %>% pull(word)
    out_left <- word_data %>% filter(lr_semantic == "left", lr_position_mean >= global_stats$lr_position_mean) %>% arrange(desc(frequency)) %>% slice_head(n = top_n_quadrant) %>% pull(word)
    in_right <- word_data %>% filter(lr_semantic == "right", lr_position_mean >= global_stats$lr_position_mean) %>% arrange(desc(frequency)) %>% slice_head(n = top_n_quadrant) %>% pull(word)
    out_right <- word_data %>% filter(lr_semantic == "right", lr_position_mean < global_stats$lr_position_mean) %>% arrange(desc(frequency)) %>% slice_head(n = top_n_quadrant) %>% pull(word)
    quadrant_top_words <- c(in_left, out_left, in_right, out_right)
  }
  all_top_words <- unique(c(global_top_words, quadrant_top_words))
  if (!is.null(highlighted_words) && length(highlighted_words) > 0) {
    all_top_words <- unique(c(all_top_words, highlighted_words))
  }
  word_data <- word_data %>% filter(word %in% all_top_words)

  # Alpha for highlight
  word_data <- word_data %>% mutate(
    is_highlighted = if (!is.null(highlighted_words)) word %in% highlighted_words else TRUE,
    point_alpha = ifelse(is_highlighted, 0.75, 0.15)
  )

  # Tooltip - use HTML encoding to preserve German umlauts
  word_data <- word_data %>% mutate(
    tooltip = paste0(
      "<b>", htmltools::htmlEscape(word_en), " (", htmltools::htmlEscape(word), ")</b><br>",
      "Self-Placement: ", round(lr_position_mean, 2), "<br>",
      "Semantic Score: ", round(semantic_score, 2), "<br>",
      "Frequency: ", frequency
    )
  )

  # Main scatter - source must match what's used in event_data()
  p <- plot_ly(
    data = word_data,
    x = ~lr_position_mean,
    y = ~semantic_score,
    type = 'scatter',
    mode = 'markers',
    color = ~lr_semantic,
    colors = SEMANTIC_COLORS,
    marker = list(
      size = ~scales::rescale(frequency, to = c(8, 24)),
      opacity = ~point_alpha,
      line = list(width = 0)
    ),
    text = ~tooltip,
    hoverinfo = 'text',
    customdata = ~word,
    showlegend = FALSE,
    width = NULL,
    source = "scatter_plot"
  )

  # Reference lines
  if (show_reference_lines) {
    p <- p %>%
      add_lines(x = c(2, 8), y = c(0, 0), line = list(dash = 'dash', color = 'grey'), inherit = FALSE, showlegend = FALSE) %>%
      add_lines(x = rep(global_stats$lr_position_mean, 2), y = c(-1.1, 1.1), line = list(dash = 'dash', color = 'grey'), inherit = FALSE, showlegend = FALSE)
  }

  # Highlight selected word with a black ring
  if (!is.null(selected_word) && selected_word %in% word_data$word) {
    sel <- word_data %>% filter(word == selected_word)
    p <- p %>%
      add_trace(
        data = sel,
        x = ~lr_position_mean,
        y = ~semantic_score,
        type = 'scatter',
        mode = 'markers',
        marker = list(
          size = 32,
          color = 'rgba(0,0,0,0)',
          line = list(width = 3, color = '#000'),
          opacity = 0.6
        ),
        hoverinfo = 'none',
        showlegend = FALSE,
        inherit = FALSE
      )
  }

  # Layout
  p <- p %>% layout(
    xaxis = list(
      title = "Left-Right Self-Placement (Mean)",
      range = c(2, 8),
      zeroline = FALSE,
      fixedrange = TRUE
    ),
    yaxis = list(
      title = "Left-Right Semantic Association Score (Left: -1, Right: +1)",
      range = c(-1.1, 1.1),
      zeroline = FALSE,
      fixedrange = TRUE
    ),
    margin = list(l = 60, r = 20, t = 20, b = 60, pad = 0),
    height = NULL,
    width = NULL,
    dragmode = 'zoom',
    hovermode = 'closest',
    font = list(family = "Arial, Helvetica, sans-serif", size = 12)
  )

  p <- p %>% config(
    displayModeBar = TRUE,
    modeBarButtonsToRemove = c("lasso2d", "select2d"),
    displaylogo = FALSE,
    scrollZoom = FALSE,
    responsive = FALSE
  ) %>%
    event_register("plotly_click")

  p
}

# ==============================================================================
# Plotting Utilities for Left-Right Associations App
# ==============================================================================
# Based on code_paper.R - clean academic style
# Colors: Red (left) and Blue (right) only
# Transparency: out-ideology more transparent, in-ideology less transparent
# Font: sans (system sans-serif)
# ==============================================================================

library(ggplot2)
library(ggrepel)
library(plotly)
library(dplyr)

# ------------------------------------------------------------------------------
# Color Definitions (Red = Left, Blue = Right ONLY)
# ------------------------------------------------------------------------------

# Association colors - primary scheme
ASSOCIATION_COLORS <- c(
  "left" = "#E30019",   # Red for left
  "right" = "#4285F4"   # Blue for right
)

# Semantic colors for the scatter plot
SEMANTIC_COLORS <- c(
  "left" = "#E30019",   # Red for semantically left
  "right" = "#4285F4",  # Blue for semantically right
  "neutral" = "#757575" # Grey for neutral
)

# ------------------------------------------------------------------------------
# Theme Academia (clean sans-serif theme)
# ------------------------------------------------------------------------------

theme_academia <- function(base_size = 12) {
  
  # Use sans-serif (cross-platform compatible)
  font <- "sans"
  
  theme_bw(base_size = base_size) %+replace%
    theme(
      text = element_text(family = font),
      legend.text = element_text(size = 9),
      legend.title = element_blank(),
      legend.box.background = element_rect(colour = "black", fill = "white", linetype = "solid"),
      
      # Grid lines - more transparent
      panel.grid.major = element_line(color = "grey80", linewidth = 0.15),
      panel.grid.minor = element_line(color = "grey90", linewidth = 0.1),
      
      # Panel border/edge
      panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.5),
      
      # Faceting
      strip.background = element_blank(),
      strip.text = element_text(
        color = "black",
        size = rel(1),
        vjust = 1
      ),
      
      # Axis
      axis.title = element_text(size = rel(0.95)),
      axis.text = element_text(size = rel(0.9)),
      
      # Plot
      plot.title = element_text(size = rel(1.1), face = "bold", hjust = 0),
      plot.subtitle = element_text(size = rel(0.9), color = "grey40"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

# ------------------------------------------------------------------------------
# Main Scatter Plot (Figure 5/6 from paper style)
# Colors: Red for left-semantic, Blue for right-semantic
# Transparency: in-ideology = 0.75, out-ideology = 0.35
# ------------------------------------------------------------------------------

create_scatter_plot <- function(data,
                                selected_word = NULL,
                                show_labels = TRUE,
                                show_reference_lines = TRUE,
                                top_n_global = 50,
                                top_n_quadrant = 10,
                                highlighted_words = NULL,
                                aspect_ratio = 1) {

  word_data <- data$word_associations
  global_stats <- data$global_stats
  
  # Get global top words by frequency
  global_top_words <- word_data %>%
    arrange(desc(frequency)) %>%
    slice_head(n = top_n_global) %>%
    pull(word)
  
  # Get quadrant-based top words (like Figure 6)
  quadrant_top_words <- c()
  if (top_n_quadrant > 0) {
    # In-ideology left (left position, left semantic)
    in_left <- word_data %>%
      filter(lr_semantic == "left", lr_position_mean < global_stats$lr_position_mean) %>%
      arrange(desc(frequency)) %>%
      slice_head(n = top_n_quadrant) %>%
      pull(word)
    
    # Out-ideology left (right position, left semantic)
    out_left <- word_data %>%
      filter(lr_semantic == "left", lr_position_mean >= global_stats$lr_position_mean) %>%
      arrange(desc(frequency)) %>%
      slice_head(n = top_n_quadrant) %>%
      pull(word)
    
    # In-ideology right (right position, right semantic)
    in_right <- word_data %>%
      filter(lr_semantic == "right", lr_position_mean >= global_stats$lr_position_mean) %>%
      arrange(desc(frequency)) %>%
      slice_head(n = top_n_quadrant) %>%
      pull(word)
    
    # Out-ideology right (left position, right semantic)
    out_right <- word_data %>%
      filter(lr_semantic == "right", lr_position_mean < global_stats$lr_position_mean) %>%
      arrange(desc(frequency)) %>%
      slice_head(n = top_n_quadrant) %>%
      pull(word)
    
    quadrant_top_words <- c(in_left, out_left, in_right, out_right)
  }
  
  # Combine and deduplicate
  all_top_words <- unique(c(global_top_words, quadrant_top_words))
  
  # Always include selected/highlighted word(s) even if not in top N
  if (!is.null(highlighted_words) && length(highlighted_words) > 0) {
    all_top_words <- unique(c(all_top_words, highlighted_words))
  }
  
  # Filter to selected words
  word_data <- word_data %>% filter(word %in% all_top_words)
  
  # Set alpha - uniform opacity for all points
  word_data <- word_data %>%
    mutate(
      # Determine in-ideology vs out-ideology (for quadrant info)
      is_in_ideology = (lr_semantic == "left" & lr_position_mean < global_stats$lr_position_mean) |
                       (lr_semantic == "right" & lr_position_mean >= global_stats$lr_position_mean),
      # Uniform alpha for all points
      point_alpha = 0.75,
      # Reduce alpha for non-highlighted words if any are highlighted
      is_highlighted = if (!is.null(highlighted_words)) word %in% highlighted_words else TRUE,
      point_alpha = ifelse(is_highlighted, point_alpha, 0.15)
    )

  # Create label column
  word_data <- word_data %>%
    mutate(
      label_text = paste0(word, " ('", word_en, "')")
    )

  # Base plot
  p <- ggplot(word_data, aes(
    x = lr_position_mean,
    y = semantic_score,
    label = label_text,
    text = tooltip_text,
    customdata = word
  )) +
    theme_academia() +
    labs(
      x = "Left-Right Self-Placement (Mean)",
      y = "Left-Right Semantic Association Score (Left: -1, Right: +1)"
    )

  # Add single reference lines (grey) - y=0 and x=global mean

  if (show_reference_lines) {
    p <- p +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
      geom_vline(xintercept = global_stats$lr_position_mean, 
                 linetype = "dashed", colour = "grey50", linewidth = 0.5)
  }

  # Add points colored by semantic association (red/blue only)
  p <- p +
    geom_point(
      aes(color = lr_semantic, alpha = point_alpha, size = frequency),
      show.legend = FALSE
    ) +
    scale_color_manual(
      values = SEMANTIC_COLORS,
      na.value = "#757575"
    ) +
    scale_alpha_identity() +
    scale_size_continuous(range = c(2, 8), guide = "none")

  # Add labels with ggrepel
  # NOTE: geom_text_repel is NOT supported in plotly interactive mode
  # Labels only appear in the downloaded PNG, not the interactive plot
  if (show_labels) {
    p <- p +
      geom_text_repel(
        aes(alpha = ifelse(is_highlighted, 1, 0.3)),
        family = "sans",
        size = 3.2,
        max.overlaps = 40,
        max.time = 3,
        show.legend = FALSE,
        segment.color = "grey50",
        segment.size = 0.3
      )
  }

  # Set axis limits - no expansion/margin at edges
  p <- p +
    scale_x_continuous(breaks = seq(2, 8, 1), limits = c(2, 8), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(-1, 1, 0.25), limits = c(-1.1, 1.1), expand = c(0.02, 0))

  # Highlight selected word with thinner, more transparent ring
  if (!is.null(selected_word)) {
    selected_data <- word_data %>% filter(word == selected_word)
    if (nrow(selected_data) > 0) {
      p <- p +
        geom_point(
          data = selected_data,
          color = "#000000",
          size = 7,
          shape = 1,
          stroke = 1.2,
          alpha = 0.6
        )
    }
  }
  
  # Set aspect ratio for downloads, or use coord_cartesian for interactive
  if (!is.null(aspect_ratio)) {
    p <- p + coord_fixed(ratio = aspect_ratio * (6 / 2.2), xlim = c(2, 8), ylim = c(-1.1, 1.1))
  } else {
    # For interactive (plotly), use coord_cartesian with fixed limits
    # This prevents the plot from resizing when highlighting changes
    p <- p + coord_cartesian(xlim = c(2, 8), ylim = c(-1.1, 1.1), expand = FALSE)
  }

  return(p)
}

# ------------------------------------------------------------------------------
# Convert ggplot to plotly with click events
# ------------------------------------------------------------------------------

create_interactive_scatter <- function(ggplot_obj) {

  p <- ggplotly(ggplot_obj, tooltip = "text", source = "scatter_plot") %>%
    layout(
      dragmode = "zoom",
      clickmode = "event",
      hovermode = "closest",
      showlegend = FALSE,
      autosize = TRUE,
      font = list(family = "Arial, Helvetica, sans-serif", size = 12),
      xaxis = list(
        range = c(2, 8),
        autorange = FALSE,
        fixedrange = FALSE,
        tickfont = list(family = "Arial, Helvetica, sans-serif", size = 11),
        titlefont = list(family = "Arial, Helvetica, sans-serif", size = 12)
      ),
      yaxis = list(
        range = c(-1.1, 1.1),
        autorange = FALSE,
        fixedrange = FALSE,
        tickfont = list(family = "Arial, Helvetica, sans-serif", size = 11),
        titlefont = list(family = "Arial, Helvetica, sans-serif", size = 12)
      ),
      hoverlabel = list(
        bgcolor = "white",
        font = list(family = "Arial, Helvetica, sans-serif", size = 12)
      )
    ) %>%
    config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d"),
      displaylogo = FALSE,
      # Mobile-friendly: scroll to zoom disabled, responsive sizing
      scrollZoom = FALSE,
      responsive = TRUE
    ) %>%
    event_register("plotly_click") %>%
    htmlwidgets::onRender("
      function(el, x) {
        // Force sans-serif on all SVG text elements
        var texts = el.querySelectorAll('text');
        texts.forEach(function(t) {
          t.style.fontFamily = 'Arial, Helvetica, sans-serif';
        });
        // Also observe for future changes
        var observer = new MutationObserver(function(mutations) {
          var texts = el.querySelectorAll('text');
          texts.forEach(function(t) {
            t.style.fontFamily = 'Arial, Helvetica, sans-serif';
          });
        });
        observer.observe(el, {childList: true, subtree: true});

        // Mobile touch improvements: increase point size on touch devices
        if ('ontouchstart' in window || navigator.maxTouchPoints > 0) {
          // Set larger marker size for touch
          var traces = el._fullData;
          if (traces) {
            traces.forEach(function(trace, i) {
              if (trace.marker && trace.marker.size) {
                // Increase size by 50% for touch
                var currentSize = Array.isArray(trace.marker.size) ? trace.marker.size : [trace.marker.size];
                var newSize = currentSize.map(function(s) { return Math.max(s * 1.3, 10); });
                Plotly.restyle(el, {'marker.size': [newSize]}, [i]);
              }
            });
          }
        }

        // Trigger resize after render to fill container properly
        setTimeout(function() {
          Plotly.Plots.resize(el);
        }, 100);
      }
    ")

  return(p)
}

# ------------------------------------------------------------------------------
# Distribution Plot (Figure 7 style from paper)
# Uses actual individual-level data from df_exp_left and df_exp_right
# Shows DENSITY plot (not histogram bars)
# Only show distributions based on lr_semantic:
# - left/right terms: show ONLY that distribution
# - neutral terms: show BOTH distributions
# ------------------------------------------------------------------------------

create_distribution_plot <- function(word_info, data) {

  if (is.null(word_info) || nrow(word_info) == 0) {
    return(create_empty_plot("Select a word to view distribution"))
  }
  
  info <- word_info
  lr_semantic <- info$lr_semantic
  the_word <- info$word
  
  # Title format: ENGLISH (German) - no quotes
  title_term <- paste0(info$word_en, " (", info$word, ")")
  
  # Determine which distributions to show based on lr_semantic
  show_left <- lr_semantic %in% c("left", "neutral")
  show_right <- lr_semantic %in% c("right", "neutral")
  
  # Get actual individual-level data
  df_exp_left <- data$df_exp_left
  df_exp_right <- data$df_exp_right
  
  # Build individual-level data for density plot
  indiv_data <- data.frame()
  left_mean <- NA
  right_mean <- NA
  
  # Get left distribution data
  if (show_left && !is.null(df_exp_left)) {
    left_individuals <- df_exp_left %>%
      filter(left_words == the_word) %>%
      filter(!is.na(lr_self))
    
    if (nrow(left_individuals) > 0) {
      left_mean <- mean(left_individuals$lr_self, na.rm = TRUE)
      indiv_data <- rbind(indiv_data, data.frame(
        lr_self = left_individuals$lr_self,
        association = "Left"
      ))
    }
  }
  
  # Get right distribution data  
  if (show_right && !is.null(df_exp_right)) {
    right_individuals <- df_exp_right %>%
      filter(right_words == the_word) %>%
      filter(!is.na(lr_self))
    
    if (nrow(right_individuals) > 0) {
      right_mean <- mean(right_individuals$lr_self, na.rm = TRUE)
      indiv_data <- rbind(indiv_data, data.frame(
        lr_self = right_individuals$lr_self,
        association = "Right"
      ))
    }
  }
  
  if (nrow(indiv_data) == 0) {
    return(create_empty_plot("No distribution data available"))
  }
  
  # Create density plot (Figure 7 style)
  p <- ggplot(indiv_data, aes(x = lr_self, fill = association)) +
    geom_density(alpha = 0.35, color = NA) +
    scale_fill_manual(
      name = "Association\nwith:",
      values = c("Left" = "#E30019", "Right" = "#4285F4")
    ) +
    scale_x_continuous(breaks = seq(1, 11, 1), limits = c(1, 11)) +
    theme_academia() +
    theme(
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      legend.title = element_text(size = 9),
      plot.title = element_text(size = 14, margin = margin(b = 15)),
      legend.position = "right"
    ) +
    labs(
      title = title_term,
      x = "Left-Right Self-Placement",
      y = NULL
    )
  
  # Get max density for positioning annotations
  # Build the plot to get the density values
  plot_build <- ggplot_build(p)
  max_density <- max(plot_build$data[[1]]$y, na.rm = TRUE)
  
  # Add mean lines and annotations for ALL distributions
  if (show_left && !is.na(left_mean)) {
    p <- p + 
      geom_vline(xintercept = left_mean, linetype = 2, color = "#E30019", linewidth = 0.8) +
      annotate(
        geom = "text",
        family = "sans",
        x = left_mean,
        y = max_density * 0.05,
        label = paste0("bar(x) == ", round(left_mean, 2)),
        parse = TRUE,
        color = "#E30019",
        size = 3.5,
        hjust = ifelse(!is.na(right_mean) && left_mean < right_mean, 1.1, -0.1)
      )
  }
  
  if (show_right && !is.na(right_mean)) {
    p <- p + 
      geom_vline(xintercept = right_mean, linetype = 2, color = "#4285F4", linewidth = 0.8) +
      annotate(
        geom = "text",
        family = "sans",
        x = right_mean,
        y = max_density * 0.05,
        label = paste0("bar(x) == ", round(right_mean, 2)),
        parse = TRUE,
        color = "#4285F4",
        size = 3.5,
        hjust = ifelse(!is.na(left_mean) && right_mean > left_mean, -0.1, 1.1)
      )
  }
  
  # Add distance segment for neutral terms (both distributions)
  if (lr_semantic == "neutral" && !is.na(left_mean) && !is.na(right_mean)) {
    p <- p +
      annotate(
        geom = "segment",
        x = left_mean,
        xend = right_mean,
        y = max_density * 0.85,
        yend = max_density * 0.85,
        color = "black",
        linetype = 1,
        linewidth = 0.35
      ) +
      annotate(
        geom = "text",
        family = "sans",
        x = (left_mean + right_mean) / 2,
        y = max_density * 0.95,
        label = paste0("Dist.: ", round(abs(left_mean - right_mean), 2)),
        color = "black",
        size = 3.5
      )
  }

  return(p)
}

# ------------------------------------------------------------------------------
# Empty Plot Placeholder
# ------------------------------------------------------------------------------

create_empty_plot <- function(message = "Select a word to view details") {

  ggplot() +
    annotate(
      "text",
      x = 0.5, y = 0.5,
      label = message,
      size = 5,
      color = "#9E9E9E",
      fontface = "italic",
      family = "sans"
    ) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "#F8F9FA", color = NA),
      plot.background = element_rect(fill = "#F8F9FA", color = NA)
    ) +
    xlim(0, 1) +
    ylim(0, 1)
}

# ------------------------------------------------------------------------------
# Save Plot Function (for download buttons)
# ------------------------------------------------------------------------------

save_plot_png <- function(plot, filename, width = 9, height = 9, dpi = 300) {
  ggsave(
    plot = plot,
    filename = filename,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
}

# ------------------------------------------------------------------------------
# Quadrant Diagram (Framework visualization inspired by Figure 1)
# Shows the 2x2 quadrant structure with highlighted active quadrant
# ------------------------------------------------------------------------------

create_quadrant_diagram <- function(word_info = NULL, global_mean = 4.9) {
  
  # Define quadrant data
  # Y-axis: Semantic Association (Right at top, Left at bottom)
  # X-axis: Political Position (Left on left, Right on right)
  quadrants <- data.frame(
    xmin = c(0, 1, 0, 1),
    xmax = c(1, 2, 1, 2),
    ymin = c(1, 1, 0, 0),
    ymax = c(2, 2, 1, 1),
    quadrant = c("out_right", "in_right", "in_left", "out_left"),
    label = c("Out-Ideology\nRight", "In-Ideology\nRight", "In-Ideology\nLeft", "Out-Ideology\nLeft"),
    ideology_type = c("out", "in", "in", "out"),
    stringsAsFactors = FALSE
  )
  
  # Determine which quadrant to highlight (if word selected)
  highlight_quadrant <- NULL
  highlight_both <- FALSE  # Flag for positionally ambiguous cases
  is_neutral <- FALSE
  no_selection <- TRUE
  is_ambiguous <- FALSE
  
  # Threshold for positional ambiguity (within ±0.5 of global mean)
  ambiguity_threshold <- 0.5
  
  if (!is.null(word_info) && is.data.frame(word_info) && nrow(word_info) > 0) {
    no_selection <- FALSE
    lr_semantic <- as.character(word_info$lr_semantic[1])
    
    if (is.na(lr_semantic) || lr_semantic == "neutral") {
      is_neutral <- TRUE
    } else {
      lr_pos <- word_info$lr_position_mean[1]
      
      # Check if positionally ambiguous (close to global mean)
      position_distance <- abs(lr_pos - global_mean)
      is_ambiguous <- position_distance < ambiguity_threshold
      
      # Determine position side (for primary quadrant)
      is_left_position <- lr_pos < global_mean
      
      if (is_ambiguous) {
        # Highlight both quadrants for this semantic side
        highlight_both <- TRUE
        if (lr_semantic == "left") {
          highlight_quadrant <- c("in_left", "out_left")
        } else if (lr_semantic == "right") {
          highlight_quadrant <- c("in_right", "out_right")
        }
      } else {
        # Clear position - single quadrant
        if (lr_semantic == "left" && is_left_position) {
          highlight_quadrant <- "in_left"
        } else if (lr_semantic == "left" && !is_left_position) {
          highlight_quadrant <- "out_left"
        } else if (lr_semantic == "right" && !is_left_position) {
          highlight_quadrant <- "in_right"
        } else if (lr_semantic == "right" && is_left_position) {
          highlight_quadrant <- "out_right"
        }
      }
    }
  }
  
  # Set fill colors based on highlight (avoiding case_when size issues)
  quadrants <- quadrants %>%
    mutate(
      fill_color = if (no_selection) {
        "#E8EAED"  # All grey if no selection
      } else if (is_neutral) {
        "#F0F0F0"  # Light/faded grey for neutral (more opaque looking)
      } else if (highlight_both && !is.null(highlight_quadrant)) {
        # Both quadrants highlighted with lighter shade for ambiguous cases
        ifelse(quadrant %in% highlight_quadrant,
               ifelse(grepl("_left", quadrant), "#F28B82", "#8AB4F8"),  # Lighter red/blue
               "#F5F5F5")
      } else if (!is.null(highlight_quadrant)) {
        ifelse(quadrant %in% highlight_quadrant,
               ifelse(grepl("_left", quadrant), "#E30019", "#4285F4"),
               "#F5F5F5")
      } else {
        "#F5F5F5"  # Light grey fallback
      },
      text_color = if (no_selection) {
        "#757575"
      } else if (is_neutral) {
        "#BDBDBD"  # Faded text for neutral
      } else if (highlight_both && !is.null(highlight_quadrant)) {
        ifelse(quadrant %in% highlight_quadrant, "white", "#9E9E9E")
      } else if (!is.null(highlight_quadrant)) {
        ifelse(quadrant %in% highlight_quadrant, "white", "#9E9E9E")
      } else {
        "#9E9E9E"
      },
      label_x = (xmin + xmax) / 2,
      label_y = (ymin + ymax) / 2
    )
  
  # Base plot
  p <- ggplot(quadrants) +
    # Quadrant rectangles
    geom_rect(
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill_color),
      color = "white",
      linewidth = 2
    ) +
    scale_fill_identity() +
    # Quadrant labels - smaller text to fit inside boxes
    geom_text(
      aes(x = label_x, y = label_y, label = label, color = text_color),
      size = 3.2,
      fontface = "bold",
      family = "sans",
      lineheight = 0.85
    ) +
    scale_color_identity() +
    # Axis labels using annotate (Y-axis: Right at top, Left at bottom)
    # Position labels closer to boxes - minimal padding
    annotate("text", x = 0.5, y = -0.12, label = "Left", size = 2.8, family = "sans", color = "#5F6368") +
    annotate("text", x = 1.5, y = -0.12, label = "Right", size = 2.8, family = "sans", color = "#5F6368") +
    annotate("text", x = 1, y = -0.30, label = "Political Position", size = 3, family = "sans", fontface = "bold", color = "#5F6368") +
    annotate("text", x = -0.12, y = 0.5, label = "Left", size = 2.8, family = "sans", color = "#5F6368", angle = 90) +
    annotate("text", x = -0.12, y = 1.5, label = "Right", size = 2.8, family = "sans", color = "#5F6368", angle = 90) +
    annotate("text", x = -0.30, y = 1, label = "Semantic\nAssociation", size = 3, family = "sans", fontface = "bold", color = "#5F6368", angle = 90, lineheight = 0.85) +
    # Reference lines (center cross) using annotate
    annotate("segment", x = 0, xend = 2, y = 1, yend = 1, color = "white", linewidth = 1.5) +
    annotate("segment", x = 1, xend = 1, y = 0, yend = 2, color = "white", linewidth = 1.5) +
    # In-ideology (+) and Out-ideology (-) indicators in quadrant corners
    # In-ideology: bottom-left (left) and top-right (right) = positive alignment
    annotate("text", x = 0.12, y = 0.12, label = "+", size = 4, family = "sans", fontface = "bold", color = "#666666") +
    annotate("text", x = 1.88, y = 1.88, label = "+", size = 4, family = "sans", fontface = "bold", color = "#666666") +
    # Out-ideology: top-left (right semantic, left position) and bottom-right (left semantic, right position) = negative framing
    annotate("text", x = 0.12, y = 1.88, label = "\u2212", size = 4, family = "sans", fontface = "bold", color = "#666666") +
    annotate("text", x = 1.88, y = 0.12, label = "\u2212", size = 4, family = "sans", fontface = "bold", color = "#666666") +
    # Theme and coordinates - tight bounds, shifted slightly left
    coord_cartesian(xlim = c(-0.45, 2.15), ylim = c(-0.45, 2.05), clip = "off") +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(2, 15, 2, 2)  # More right margin to push content left
    )
  
  # Add "NEUTRAL" indicator if neutral word (bold and prominent)
  if (is_neutral && !no_selection) {
    p <- p +
      annotate("label", x = 1, y = 1, label = "NEUTRAL", size = 6, 
               family = "sans", fontface = "bold", color = "#424242",
               fill = "white", label.size = 0, label.padding = unit(0.4, "lines"))
  }
  
  # Add warning for positionally ambiguous cases
  if (is_ambiguous && !no_selection) {
    p <- p +
      annotate("text", x = 1, y = -0.50,
               label = "Position near center \u2013 ambiguous",
               size = 3.2, family = "sans", fontface = "italic", color = "#B45309")
  }
  
  return(p)
}
