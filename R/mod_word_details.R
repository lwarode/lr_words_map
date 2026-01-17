# ==============================================================================
# Word Details Module for Left-Right Associations App
# ==============================================================================
# Shows word info with language toggle (checkboxes) and distribution plot
# No separate dropdown - uses selected_word from scatter plot module
# ==============================================================================

# Helper: conditionally add spinner
add_spinner <- function(ui, ...) {
  if (requireNamespace("shinycssloaders", quietly = TRUE)) {
    shinycssloaders::withSpinner(ui, ...)
  } else {
    ui
  }
}

# ------------------------------------------------------------------------------
# Module UI
# ------------------------------------------------------------------------------

wordDetailsUI <- function(id) {
  ns <- NS(id)

  div(
    class = "word-details-container",

    # Word header section - using simple div instead of card to avoid blue border
    div(
      style = "background: #FFFFFF; border-radius: 0.5rem; padding: 1rem; margin-bottom: 1rem; box-shadow: 0 1px 3px rgba(0,0,0,0.08);",
      # Word title (reactive)
      uiOutput(ns("word_header")),
      # Quick stats with frequencies
      uiOutput(ns("word_stats"))
    ),

    # Distribution plot section - also simple div
    div(
      style = "background: #FFFFFF; border-radius: 0.5rem; padding: 0; margin-bottom: 1rem; box-shadow: 0 1px 3px rgba(0,0,0,0.08);",
      div(
        style = "padding: 0.875rem 1rem; border-bottom: 1px solid #E8EAED; display: flex; justify-content: space-between; align-items: center;",
        tags$h6("Position Distribution", style = "margin: 0; font-weight: 500;"),
        downloadButton(
          ns("download_dist"),
          "Download Figure",
          class = "btn btn-sm btn-outline-secondary"
        )
      ),
      div(
        style = "padding: 0.5rem;",
        add_spinner(plotOutput(ns("distribution_plot"), height = "320px"), type = 6, color = "#5F6368", size = 0.5)
      )
    ),
    
    # Quadrant diagram section
    div(
      style = "background: #FFFFFF; border-radius: 0.5rem; padding: 0; margin-bottom: 1rem; box-shadow: 0 1px 3px rgba(0,0,0,0.08);",
      div(
        style = "padding: 0.875rem 1rem; border-bottom: 1px solid #E8EAED;",
        tags$h6("Framework", style = "margin: 0; font-weight: 500;")
      ),
      div(
        class = "text-center",
        style = "padding: 0.75rem;",
        div(
          style = "display: inline-block; width: 100%; max-width: 400px;",
          plotOutput(ns("quadrant_diagram"), height = "320px", width = "100%")
        )
      ),
      div(
        style = "padding: 0.75rem 1rem; border-top: 1px solid #E8EAED;",
        tags$small(
          class = "text-muted",
          HTML(
            "<strong>In-Ideology:</strong> Words used by people to describe their own ideological side<br>",
            "<strong>Out-Ideology:</strong> Words used by people to describe the opposing ideological side<br>",
            "<strong>Neutral:</strong> Words with no clear semantic left-right association"
          )
        )
      )
    )
  )
}

# ------------------------------------------------------------------------------
# Module Server
# ------------------------------------------------------------------------------

wordDetailsServer <- function(id, data, selected_word) {
  moduleServer(id, function(input, output, session) {
    font_scale_for <- function(output_id) {
      1
    }

    # Reactive: Get word info (handles NULL and empty string)
    word_info <- reactive({
      sw <- selected_word()
      if (is.null(sw) || sw == "") return(NULL)
      req(data())

      data()$word_associations %>%
        filter(word == sw) %>%
        slice(1)
    })

    # Render word header - always show ENGLISH (GERMAN) format
    output$word_header <- renderUI({
      sw <- selected_word()
      if (is.null(sw) || sw == "" || is.null(word_info()) || nrow(word_info()) == 0) {
        return(
          div(
            class = "text-center py-4",
            tags$i(class = "bi bi-hand-index-thumb fs-1 text-muted"),
            tags$p(class = "text-muted mt-2 mb-0", "Click on a word in the plot or use the dropdown")
          )
        )
      }

      info <- word_info()

      # Always show both languages: ENGLISH (GERMAN)
      word_display <- tags$h4(
        info$word_en,
        tags$small(class = "text-muted ms-2", paste0("(", info$word, ")"))
      )

      # Semantic badge color (red for left, blue for right, grey for neutral)
      badge_color <- switch(
        as.character(info$lr_semantic),
        "left" = "background-color: #E30019; color: white;",
        "right" = "background-color: #4285F4; color: white;",
        "background-color: #757575; color: white;"
      )
      
      badge_text <- switch(
        as.character(info$lr_semantic),
        "left" = "Left",
        "right" = "Right",
        "Neutral"
      )

      tagList(
        div(
          class = "d-flex align-items-center justify-content-between",
          word_display,
          span(
            class = "badge",
            style = badge_color,
            badge_text
          )
        )
      )
    })

    # Render word stats with colored frequencies
    output$word_stats <- renderUI({
      sw <- selected_word()
      if (is.null(sw) || sw == "" || is.null(word_info()) || nrow(word_info()) == 0) {
        return(NULL)
      }

      info <- word_info()

      div(
        class = "mt-3",
        # Row 1: Position and Semantic Score
        div(
          class = "row text-center mb-2",
          div(
            class = "col-6",
            tags$div(class = "stat-value h5 mb-0", style = "color: #333333;", round(info$lr_position_mean, 2)),
            tags$div(class = "stat-label small text-muted", "POSITION (MEAN)")
          ),
          div(
            class = "col-6",
            tags$div(class = "stat-value h5 mb-0", style = "color: #333333;", round(info$semantic_score, 3)),
            tags$div(class = "stat-label small text-muted", "SEMANTIC SCORE")
          )
        ),
        # Row 2: Frequencies with colors
        div(
          class = "row text-center",
          div(
            class = "col-4",
            tags$div(
              class = "stat-value h5 mb-0",
              style = "color: #E30019;",  # Red for left
              ifelse(is.na(info$left_freq), 0, info$left_freq)
            ),
            tags$div(class = "stat-label small text-muted", "LEFT")
          ),
          div(
            class = "col-4",
            tags$div(
              class = "stat-value h5 mb-0",
              style = "color: #4285F4;",  # Blue for right
              ifelse(is.na(info$right_freq), 0, info$right_freq)
            ),
            tags$div(class = "stat-label small text-muted", "RIGHT")
          ),
          div(
            class = "col-4",
            tags$div(
              class = "stat-value h5 mb-0",
              style = "color: #333333;",  # Black for total
              info$frequency
            ),
            tags$div(class = "stat-label small text-muted", "TOTAL")
          )
        )
      )
    })

    # Render distribution plot
    output$distribution_plot <- renderPlot({
      # Disable showtext for UI rendering to avoid DPI issues on shinyapps.io
      # Downloads still use showtext with correct DPI
      if (requireNamespace("showtext", quietly = TRUE)) {
        showtext_was_on <- showtext::showtext_auto()
        showtext::showtext_auto(enable = FALSE)
        on.exit(showtext::showtext_auto(enable = showtext_was_on), add = TRUE)
      }

      font_scale <- font_scale_for("distribution_plot")

      sw <- selected_word()
      if (is.null(sw) || sw == "") {
        return(create_empty_plot("Select a word to view distribution", font_scale = font_scale))
      }

      info <- word_info()

      if (is.null(info) || nrow(info) == 0) {
        return(create_empty_plot("No data available", font_scale = font_scale))
      }

      create_distribution_plot(info, data(), font_scale = font_scale)
    }, res = 96)

    # Render quadrant diagram
    output$quadrant_diagram <- renderPlot({
      # Disable showtext for UI rendering to avoid DPI issues on shinyapps.io
      # Downloads still use showtext with correct DPI
      if (requireNamespace("showtext", quietly = TRUE)) {
        showtext_was_on <- showtext::showtext_auto()
        showtext::showtext_auto(enable = FALSE)
        on.exit(showtext::showtext_auto(enable = showtext_was_on), add = TRUE)
      }

      font_scale <- font_scale_for("quadrant_diagram")

      info <- word_info()
      # Get global mean from data for accurate positional comparison
      global_mean <- if (!is.null(data()) && !is.null(data()$global_stats)) {
        data()$global_stats$lr_position_mean
      } else {
        4.9  # Fallback default
      }
      create_quadrant_diagram(info, global_mean = global_mean, font_scale = font_scale)
    }, res = 96)
    
    # Download handler for distribution plot
    output$download_dist <- downloadHandler(
      filename = function() {
        info <- word_info()
        if (!is.null(info) && nrow(info) > 0 && !is.na(info$word_en[1])) {
          word_en <- tolower(gsub("[^a-zA-Z0-9]", "_", info$word_en[1]))
          return(paste0("distribution_", word_en, "_", Sys.Date(), ".png"))
        }
        paste0("distribution_", Sys.Date(), ".png")
      },
      content = function(file) {
        info <- word_info()
        if (!is.null(info) && nrow(info) > 0) {
          p <- create_distribution_plot(info, data())
          old_dpi <- getOption("showtext.dpi", 96)
          if (requireNamespace("showtext", quietly = TRUE)) {
            showtext::showtext_opts(dpi = 300)
            on.exit(showtext::showtext_opts(dpi = old_dpi), add = TRUE)
          }
          ggsave(
            file,
            plot = p,
            width = 8,
            height = 5,
            dpi = 300,
            bg = "white"
          )
        }
      }
    )

    # Return NULL - no longer returning selected word
    return(NULL)
  })
}
