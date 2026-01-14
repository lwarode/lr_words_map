# ==============================================================================
# Scatter Plot Module for Left-Right Associations App
# ==============================================================================
# Interactive scatter plot visualization (Figure 5/6 from paper)
# Features: word dropdown, top N selector, quadrant top N, aspect ratio, download
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

scatterPlotUI <- function(id) {
  ns <- NS(id)

  tagList(
    # Plot container
    div(
      class = "scatter-plot-container",

      # Header with controls
      div(
        class = "plot-header mb-3",
        
        # Row 1: Title
        div(
          class = "mb-2",
          h4("Word Associations in Left-Right Space", class = "mb-0")
        ),
        
        # Row 2: Word selector and Clear (mobile-friendly row)
        div(
          class = "d-flex align-items-center gap-2 mb-2 word-selector-row",
          div(
            class = "flex-grow-1",
            # Word selector dropdown - searchable
            selectizeInput(
              ns("select_word"),
              label = NULL,
              choices = NULL,  # Will be populated by server
              selected = NULL,
              width = "100%",
              options = list(
                placeholder = "Select a word...",
                create = FALSE
              )
            )
          ),
          # Clear selection link
          actionLink(
            ns("clear_word"),
            label = "Clear",
            class = "clear-btn",
            style = "font-size: 0.9rem; color: #5F6368; white-space: nowrap; padding: 0.5rem;"
          )
        ),
        
        # Row 3: Top N controls (all in one line)
        div(
          class = "d-flex align-items-center gap-1 mb-2 topn-controls-row",
          style = "flex-wrap: nowrap;",
          tags$label("Show top", class = "mb-0 small"),
          selectInput(
            ns("top_n_global"),
            label = NULL,
            choices = c("10", "20", "30", "50", "75", "100"),
            selected = "50",
            width = "75px"
          ),
          tags$label("overall +", class = "mb-0 small", style = "white-space: nowrap;"),
          selectInput(
            ns("top_n_quadrant"),
            label = NULL,
            choices = c("5", "10", "15", "20"),
            selected = "10",
            width = "75px"
          ),
          tags$label("per quadrant", class = "mb-0 small", style = "white-space: nowrap;")
        ),
        
        # Row 4: Other controls and download
        div(
          class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
          div(
            class = "d-flex align-items-center gap-3 flex-wrap",
            # Aspect ratio selector (hidden on small mobile via CSS)
            div(
              class = "d-flex align-items-center gap-1 aspect-ratio-control",
              tags$label("Aspect:", class = "mb-0 small"),
              selectInput(
                ns("aspect_ratio"),
                label = NULL,
                choices = c("Auto" = "auto", "1:1" = "1", "4:3" = "0.75", "16:9" = "0.5625"),
                selected = "auto",
                width = "80px"
              )
            ),
            # Toggle checkbox for reference lines only
            checkboxInput(
              ns("show_ref_lines"),
              "Show Reference Lines",
              value = TRUE
            )
          ),
          # Download button
          div(
            downloadButton(
              ns("download_plot"),
              "PNG",
              class = "btn btn-sm btn-outline-secondary"
            )
          )
        )
      ),

      # Main plot - height controlled by CSS for mobile responsiveness
      div(
        class = "plot-wrapper",
        add_spinner(plotlyOutput(ns("scatter_plot"), height = "auto"), type = 6, color = "#5F6368")
      ),

      # Legend/info below plot
      div(
        class = "plot-legend mt-3",
        div(
          class = "row",
          div(
            class = "col-md-8",
            tags$small(
              class = "text-muted",
              HTML(
                "<strong>X-Axis:</strong> Mean left-right self-placement of individuals using the word<br>",
                "<strong>Y-Axis:</strong> Semantic association score (-1 = left, +1 = right)<br>",
                "<span style='color: #E30019;'>●</span> Semantically Left &nbsp; ",
                "<span style='color: #4285F4;'>●</span> Semantically Right &nbsp; ",
                "<span style='color: #757575;'>●</span> Neutral"
              )
            )
          ),
          div(
            class = "col-md-4 text-end",
            tags$small(
              class = "text-muted",
              "Click on a point to view detailed information"
            )
          )
        )
      )
    )
  )
}

# ------------------------------------------------------------------------------
# Module Server
# ------------------------------------------------------------------------------

scatterPlotServer <- function(id, data, selected_word = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    
    # Internal reactive value for current selection
    current_selection <- reactiveVal(NULL)
    
    # Flag to prevent circular updates
    updating_from_click <- reactiveVal(FALSE)
    updating_from_dropdown <- reactiveVal(FALSE)
    
    # Update word dropdown choices when data loads
    observe({
      req(data())
      
      # Get top words by frequency (ordered by frequency, not alphabetically)
      top_words_df <- data()$word_associations %>%
        arrange(desc(frequency)) %>%
        slice_head(n = 500)
      
      # Format: "English (German)"
      labels <- paste0(top_words_df$word_en, " (", top_words_df$word, ")")
      
      # Values are always German words (the key in the data)
      # Add empty option at beginning for "no selection"
      choices <- c(setNames("", ""), setNames(top_words_df$word, labels))
      
      # Determine selected value
      sel_val <- current_selection()
      if (is.null(sel_val)) sel_val <- ""
      
      updateSelectizeInput(
        session, 
        "select_word",
        choices = choices,
        selected = sel_val,
        server = TRUE
      )
    })
    
    # When dropdown changes -> update current selection
    observeEvent(input$select_word, {
      if (updating_from_click()) {
        updating_from_click(FALSE)
        return()
      }
      
      if (!is.null(input$select_word) && input$select_word != "") {
        updating_from_dropdown(TRUE)
        current_selection(input$select_word)
      }
    }, ignoreInit = TRUE)
    
    # Clear word selection
    observeEvent(input$clear_word, {
      current_selection(NULL)
      updateSelectizeInput(session, "select_word", selected = "")
    })
    
    # When click happens on plot -> update dropdown
    # Use debounce to prevent rapid-fire clicks
    last_click_time <- reactiveVal(0)
    
    observeEvent(event_data("plotly_click", source = "scatter_plot"), {
      click_data <- event_data("plotly_click", source = "scatter_plot")
      
      if (!is.null(click_data) && !is.null(click_data$customdata)) {
        clicked_word <- click_data$customdata
        
        # Debounce: ignore clicks within 500ms of each other
        current_time <- as.numeric(Sys.time()) * 1000
        if (current_time - last_click_time() < 500) {
          return()
        }
        last_click_time(current_time)
        
        # Skip if already selected
        if (!is.null(current_selection()) && clicked_word == current_selection()) {
          return()
        }
        
        if (updating_from_dropdown()) {
          updating_from_dropdown(FALSE)
          return()
        }
        
        # Update the dropdown to match clicked word
        updating_from_click(TRUE)
        current_selection(clicked_word)
        updateSelectizeInput(session, "select_word", selected = clicked_word)
      }
    })
    
    # When external selected_word changes (from parent) -> update
    observeEvent(selected_word(), {
      ext_word <- selected_word()
      if (!is.null(ext_word) && ext_word != "" && ext_word != current_selection()) {
        updating_from_click(TRUE)
        current_selection(ext_word)
        updateSelectizeInput(session, "select_word", selected = ext_word)
      }
    })

    # Reactive: Create the ggplot object
    scatter_ggplot <- reactive({
      req(data())
      
      # Handle "auto" aspect ratio - pass NULL to skip coord_fixed
      # This lets plotly fill the available space naturally
      aspect <- input$aspect_ratio
      if (is.null(aspect) || aspect == "auto") {
        aspect_val <- NULL  # No fixed aspect ratio - plotly fills container
      } else {
        aspect_val <- as.numeric(aspect)
      }

      create_scatter_plot(
        data = data(),
        selected_word = current_selection(),
        show_labels = TRUE,  # Always show labels
        show_reference_lines = input$show_ref_lines,
        top_n_global = as.integer(input$top_n_global),
        top_n_quadrant = as.integer(input$top_n_quadrant),
        highlighted_words = current_selection(),
        aspect_ratio = aspect_val
      )
    })

    # Render the interactive plotly plot
    output$scatter_plot <- renderPlotly({
      req(scatter_ggplot())

      create_interactive_scatter(scatter_ggplot())
    })
    
    # Download handler
    output$download_plot <- downloadHandler(
      filename = function() {
        # Include selected word in filename if one is selected
        selected <- current_selection()
        if (!is.null(selected) && selected != "") {
          # Get English translation for filename
          word_data <- data()$word_associations %>%
            filter(word == selected)
          if (nrow(word_data) > 0 && !is.na(word_data$word_en[1])) {
            word_en <- tolower(gsub("[^a-zA-Z0-9]", "_", word_data$word_en[1]))
            return(paste0("lr_associations_", word_en, "_", Sys.Date(), ".png"))
          }
        }
        paste0("lr_associations_", Sys.Date(), ".png")
      },
      content = function(file) {
        # Handle "auto" aspect ratio for download
        aspect <- input$aspect_ratio
        if (is.null(aspect) || aspect == "auto") {
          aspect_val <- 1
        } else {
          aspect_val <- as.numeric(aspect)
        }
        
        # Create static ggplot for download
        p <- create_scatter_plot(
          data = data(),
          selected_word = current_selection(),
          show_labels = TRUE,  # Always show labels
          show_reference_lines = input$show_ref_lines,
          top_n_global = as.integer(input$top_n_global),
          top_n_quadrant = as.integer(input$top_n_quadrant),
          highlighted_words = current_selection(),
          aspect_ratio = aspect_val
        )
        
        ggsave(
          file, 
          plot = p, 
          width = 9, 
          height = 9 * aspect_val, 
          dpi = 300,
          bg = "white"
        )
      }
    )

    # Return the current selection for use by parent
    return(current_selection)
  })
}
