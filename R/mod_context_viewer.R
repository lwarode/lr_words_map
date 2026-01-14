# ==============================================================================
# Context Viewer Module for Left-Right Associations App
# ==============================================================================
# Displays political text examples with highlighted words
# ==============================================================================

# ------------------------------------------------------------------------------
# Module UI
# ------------------------------------------------------------------------------

contextViewerUI <- function(id) {
  ns <- NS(id)

  div(
    class = "context-viewer-container",

    # Header
    div(
      class = "card",
      div(
        class = "card-header bg-transparent d-flex justify-content-between align-items-center",
        tags$h6("Context Examples", class = "mb-0"),
        # Refresh button
        actionButton(
          ns("refresh_contexts"),
          label = NULL,
          icon = icon("refresh"),
          class = "btn btn-sm btn-outline-secondary"
        )
      ),

      # Context list
      div(
        class = "card-body p-0",
        uiOutput(ns("context_list"))
      )
    )
  )
}

# ------------------------------------------------------------------------------
# Module Server
# ------------------------------------------------------------------------------

contextViewerServer <- function(id, data, selected_word) {
  moduleServer(id, function(input, output, session) {

    # Reactive: Get context examples for selected word
    contexts <- reactive({
      req(data())

      if (is.null(selected_word())) {
        return(NULL)
      }

      get_word_contexts(data(), selected_word(), n = 5)
    })

    # Reactive value to force refresh
    refresh_trigger <- reactiveVal(0)

    # Observe refresh button
    observeEvent(input$refresh_contexts, {
      refresh_trigger(refresh_trigger() + 1)
    })

    # Render context list
    output$context_list <- renderUI({
      # Depend on refresh trigger
      refresh_trigger()

      if (is.null(selected_word())) {
        return(
          div(
            class = "text-center py-5",
            tags$i(class = "bi bi-file-text fs-1 text-muted"),
            tags$p(class = "text-muted mt-2 mb-0", "Select a word to view context examples")
          )
        )
      }

      ctx <- contexts()

      if (is.null(ctx) || nrow(ctx) == 0) {
        return(
          div(
            class = "text-center py-4",
            tags$i(class = "bi bi-exclamation-circle fs-2 text-muted"),
            tags$p(class = "text-muted mt-2 mb-0", "No context examples found for this word")
          )
        )
      }

      # Shuffle if refresh was clicked
      if (refresh_trigger() > 0 && nrow(ctx) > 1) {
        ctx <- ctx[sample(nrow(ctx)), ]
      }

      # Create context cards
      context_cards <- lapply(1:nrow(ctx), function(i) {
        row <- ctx[i, ]

        # Highlight the word in text
        highlighted_text <- highlight_word_in_text(row$text, selected_word())

        # Party color
        party_color <- PARTY_COLORS[row$speaker_party]
        if (is.na(party_color)) party_color <- "#9E9E9E"

        div(
          class = "context-item border-bottom p-3",

          # Text with highlighting
          div(
            class = "context-text mb-2",
            HTML(highlighted_text)
          ),

          # Metadata row
          div(
            class = "context-meta d-flex justify-content-between align-items-center",
            div(
              # Party badge
              span(
                class = "badge me-2",
                style = paste0("background-color: ", party_color, ";"),
                row$speaker_party
              ),
              # Speaker name
              tags$small(class = "text-muted", row$speaker_name)
            ),
            div(
              # Source and date
              tags$small(
                class = "text-muted",
                paste0(row$source_type, " | ", format(as.Date(row$date), "%b %Y"))
              )
            )
          )
        )
      })

      # Wrap in scrollable container
      div(
        class = "context-list-scroll",
        style = "max-height: 350px; overflow-y: auto;",
        context_cards
      )
    })

    # Return contexts for potential external use
    return(contexts)
  })
}

# ------------------------------------------------------------------------------
# Standalone Context Card Component
# ------------------------------------------------------------------------------

createContextCard <- function(text, word, speaker_party, speaker_name, source_type, date) {

  # Highlight word
  highlighted_text <- highlight_word_in_text(text, word)

  # Party color
  party_color <- PARTY_COLORS[speaker_party]
  if (is.na(party_color)) party_color <- "#9E9E9E"

  div(
    class = "card context-card mb-2",
    div(
      class = "card-body py-2 px-3",

      # Quote text
      div(
        class = "context-quote",
        tags$i(class = "bi bi-quote me-1 text-muted"),
        HTML(highlighted_text)
      ),

      # Footer with metadata
      div(
        class = "context-footer mt-2 d-flex justify-content-between",
        div(
          span(
            class = "badge",
            style = paste0("background-color: ", party_color, ";"),
            speaker_party
          ),
          tags$small(class = "ms-2 text-muted", speaker_name)
        ),
        tags$small(class = "text-muted", paste0(source_type, ", ", format(as.Date(date), "%Y")))
      )
    )
  )
}

# ------------------------------------------------------------------------------
# Expanded Context Modal (for viewing full text)
# ------------------------------------------------------------------------------

contextModalUI <- function(id) {
  ns <- NS(id)

  # Modal for expanded context view
  tags$div(
    class = "modal fade",
    id = ns("context_modal"),
    tabindex = "-1",

    div(
      class = "modal-dialog modal-lg",
      div(
        class = "modal-content",
        div(
          class = "modal-header",
          h5(class = "modal-title", "Full Context"),
          tags$button(
            type = "button",
            class = "btn-close",
            `data-bs-dismiss` = "modal"
          )
        ),
        div(
          class = "modal-body",
          uiOutput(ns("modal_content"))
        )
      )
    )
  )
}

# ------------------------------------------------------------------------------
# Helper: Create empty context placeholder
# ------------------------------------------------------------------------------

emptyContextPlaceholder <- function(message = "Select a word to view context examples") {
  div(
    class = "empty-context-placeholder text-center py-5",
    tags$i(class = "bi bi-file-earmark-text fs-1 text-muted"),
    tags$p(class = "text-muted mt-3", message)
  )
}
