# ==============================================================================
# Mapping Left-Right Associations - Interactive Visualization
# ==============================================================================
# Shiny application visualizing results from:
# "Mapping left-right associations: a framework using open-ended survey
# responses and political positions" by Lukas Warode (2025)
# ==============================================================================

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

# Set UTF-8 locale for proper German umlaut handling
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Set to TRUE when real data files are available in the data/ folder
USE_REAL_DATA <- TRUE

# Debug mode - set to FALSE for production deployment
DEBUG_MODE <- FALSE

# ------------------------------------------------------------------------------
# Load Packages
# ------------------------------------------------------------------------------

library(shiny)
library(bslib)
library(plotly)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(tidyr)
library(readr)
library(htmltools)
library(showtext)
library(sysfonts)

# Load Source Sans Pro font for UI elements (with error handling for shinyapps.io)
tryCatch({
  font_add_google("Source Sans Pro", "sourcesans")
  showtext_auto()
}, error = function(e) {
  message("Could not load Google Font via showtext (using CSS fallback): ", e$message)
})

# Optional: shinycssloaders for loading spinners
HAS_SPINNERS <- requireNamespace("shinycssloaders", quietly = TRUE)
if (HAS_SPINNERS) {
  library(shinycssloaders)
} else {
  # Fallback: create a dummy withSpinner function
  withSpinner <- function(ui, ...) ui
}

# ------------------------------------------------------------------------------
# Source Modules and Utilities
# ------------------------------------------------------------------------------

source("R/generate_placeholder_data.R")
source("R/utils_data.R")
source("R/utils_plots.R")
source("R/mod_scatter_plot.R")
source("R/mod_word_details.R")

# ------------------------------------------------------------------------------
# Load Data
# ------------------------------------------------------------------------------

# Load data at startup (either real or placeholder)
app_data <- load_app_data(use_real_data = USE_REAL_DATA)

if (DEBUG_MODE) message(paste("Loaded", nrow(app_data$word_associations), "word associations"))

# ------------------------------------------------------------------------------
# UI Definition
# ------------------------------------------------------------------------------

ui <- page_fluid(

  # Theme - Grey/neutral design (blue reserved for semantically right words)
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#5F6368",
    secondary = "#9E9E9E",
    success = "#34A853",
    warning = "#FBBC05",
    danger = "#EA4335",
    base_font = font_google("Source Sans Pro"),
    heading_font = font_google("Source Sans Pro"),
    font_scale = 0.95
  ),
  title = "Mapping Left-Right Associations",

 # Custom CSS
 tags$head(
   # Viewport meta for mobile
   tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"),
   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
   # Google Fonts: Source Sans Pro for UI
   tags$link(
     rel = "stylesheet",
     href = "https://fonts.googleapis.com/css2?family=Source+Sans+Pro:wght@300;400;600;700&display=swap"
   ),
   # Bootstrap Icons
   tags$link(
     rel = "stylesheet",
     href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.0/font/bootstrap-icons.css"
   ),
   # Favicon (optional)
   tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
   # JavaScript for clearing selectize and user guide toggle
   tags$script(HTML("
     Shiny.addCustomMessageHandler('clearSelectize', function(message) {
       var selectize = $('#' + message.id)[0];
       if (selectize && selectize.selectize) {
         selectize.selectize.clear();
       }
     });

     // Toggle aria-expanded on user guide header click
     $(document).on('click', '.user-guide-header', function() {
       var expanded = $(this).attr('aria-expanded') === 'true';
       $(this).attr('aria-expanded', !expanded);
     });
   "))
 ),

 # Main container
 div(
   class = "main-container",

   # App Header - Grey background, more prominent
   div(
     class = "app-header",
     style = "background: linear-gradient(135deg, #5F6368 0%, #424242 100%); padding: 1.5rem;",
     div(
       class = "row align-items-center",
       div(
         class = "col-md-7",
         h1("Mapping Left-Right Associations", style = "font-size: 1.8rem; font-weight: 700; margin-bottom: 0.5rem;"),
         p(
           class = "subtitle mb-0",
           style = "font-size: 1rem; opacity: 0.9;",
           "Interactive visualization of word associations in political ideology"
         )
       ),
       div(
         class = "col-md-5 text-md-end mt-3 mt-md-0",
         # Citation with Please cite message
         div(
           style = "background: rgba(255,255,255,0.1); padding: 0.75rem 1rem; border-radius: 8px; display: inline-block;",
           tags$span(style = "font-weight: 600; font-size: 0.95rem;", "Lukas Warode (2025)"),
           tags$br(),
           div(
             class = "d-flex align-items-center justify-content-end gap-2 mt-2",
             tags$span(style = "font-weight: 400; font-size: 0.85rem; opacity: 0.9;", "Please cite:"),
             tags$a(
               href = "https://doi.org/10.1057/s41599-025-05679-x",
               target = "_blank",
               class = "btn btn-outline-light btn-sm",
               tags$i(class = "bi bi-journal-text me-1"),
               "Paper"
             )
           )
         )
       )
     )
   ),

   # User Guide - Collapsible on mobile, always visible on desktop
   div(
     class = "user-guide",
     style = "background-color: #f8f9fa; margin-bottom: 1rem; border-radius: 6px; border-left: 4px solid #5F6368;",
     # Mobile: clickable header to toggle
     div(
       class = "user-guide-header d-lg-none",
       style = "padding: 0.75rem 1rem; cursor: pointer;",
       `data-bs-toggle` = "collapse",
       `data-bs-target` = "#userGuideContent",
       `aria-expanded` = "false",
       div(
         class = "d-flex justify-content-between align-items-center",
         tags$span(style = "font-size: 0.9rem; font-weight: 500; color: #424242;", "About this visualization"),
         tags$i(class = "bi bi-chevron-down", style = "color: #5F6368;")
       )
     ),
     # Desktop: always visible header text (no toggle)
     div(
       class = "d-none d-lg-block",
       style = "padding: 0.75rem 1rem;",
       tags$p(
         class = "mb-0",
         style = "font-size: 0.9rem; color: #424242; line-height: 1.5;",
         HTML(paste0(
           "This visualization maps how political words are associated with 'left' and 'right' based on open-ended survey responses from German electoral candidates (2013\u20132021). ",
           "Each point represents a <strong>word association</strong>, not an individual person \u2013 positioned by the average left-right self-placement of respondents who mentioned it. ",
           "Words in the <span style='color: #E30019;'>lower-left</span> and <span style='color: #4285F4;'>upper-right</span> quadrants reflect <em>in-ideological</em> associations (alignment with one's political side), while the opposite quadrants show <em>out-ideological</em> associations.",
           "<br><br>",
           "The panel next to the main figure summarizes each selected word. Position (mean) shows the average left-right self-placement of candidates who used it. The semantic score indicates how left (-1) or right (+1) the word is used. The frequency below shows how often it appeared (left, right, total). The left-right self-placement distribution below is based on individual candidates who used the word and is separated by those who associated it with the left or right."
         ))
       )
     ),
     # Mobile: collapsible content
     div(
       id = "userGuideContent",
       class = "collapse d-lg-none",
       style = "padding: 0 1rem 0.75rem 1rem;",
       tags$p(
         class = "mb-0",
         style = "font-size: 0.9rem; color: #424242; line-height: 1.5;",
         HTML(paste0(
           "This visualization maps how political words are associated with 'left' and 'right' based on open-ended survey responses from German electoral candidates (2013\u20132021). ",
           "Each point represents a <strong>word association</strong>, not an individual person \u2013 positioned by the average left-right self-placement of respondents who mentioned it. ",
           "Words in the <span style='color: #E30019;'>lower-left</span> and <span style='color: #4285F4;'>upper-right</span> quadrants reflect <em>in-ideological</em> associations (alignment with one's political side), while the opposite quadrants show <em>out-ideological</em> associations.",
           "<br><br>",
           "The panel next to the main figure summarizes each selected word. Position (mean) shows the average left-right self-placement of candidates who used it. The semantic score indicates how left (-1) or right (+1) the word is used. The frequency below shows how often it appeared (left, right, total). The left-right self-placement distribution below is based on individual candidates who used the word and is separated by those who associated it with the left or right."
         ))
       )
     )
   ),

   # Main Content Row
   div(
     class = "row g-3",

     # Left Column: Scatter Plot (larger)
     div(
       class = "col-lg-8 col-xl-8",
       scatterPlotUI("scatter")
     ),

     # Right Column: Details Panel (word details only, no context viewer)
     div(
       class = "col-lg-4 col-xl-4",
       wordDetailsUI("word_details")
     )
   ),

   # Footer - Citation with methods description
   div(
     class = "mt-4 pt-3 border-top",
     style = "background-color: #f8f9fa; padding: 1rem; border-radius: 8px;",
     div(
       class = "row",
       div(
         class = "col-12",
         # Citation with paper icon
         div(
           class = "mb-2",
           tags$span(
             tags$i(class = "bi bi-journal-text me-2"),
             HTML("<strong>Please cite:</strong>")
           )
         ),
         tags$p(
           class = "mb-2",
           style = "font-size: 0.95rem;",
           HTML(paste0(
             "Warode, L. (2025). ",
             "Mapping left-right associations: a framework using open-ended ",
             "survey responses and political positions. ",
             "<em>Humanities and Social Sciences Communications</em>, 12:1318. "
           )),
           tags$a(
             href = "https://doi.org/10.1057/s41599-025-05679-x",
             target = "_blank",
             style = "font-size: 0.9rem;",
             "https://doi.org/10.1057/s41599-025-05679-x"
           )
         ),
         # Methods paragraph
         tags$p(
           class = "mb-0 text-muted",
           style = "font-size: 0.85rem;",
           HTML(paste0(
             "<strong>About the data:</strong> ",
             "This visualization is based on open-ended survey responses from German electoral candidates ",
             "(Candidate Studies 2013, 2017, and 2021). Respondents were asked what they associate with ",
             "'left' and 'right' in political terms. Word positions reflect the mean left-right self-placement ",
             "of individuals who mentioned each word."
           ))
         )
       )
     )
   )
 )
)

# ------------------------------------------------------------------------------
# Server Logic
# ------------------------------------------------------------------------------

server <- function(input, output, session) {

 # Reactive: App data (could be made dynamic for filtering in future)
 data <- reactive({
   app_data
 })

 # Selected word state - central source of truth
 selected_word <- reactiveVal(NULL)

 # Scatter Plot Module - returns clicked/selected word
 scatter_selection <- scatterPlotServer("scatter", data, selected_word)

 # Word Details Module - displays details for selected word
 wordDetailsServer("word_details", data, selected_word)

 # Update central state when scatter plot selection changes
 observeEvent(scatter_selection(), {
   scatter_sel <- scatter_selection()
   current_sel <- selected_word()
   
   # Handle clear (NULL) case
   if (is.null(scatter_sel)) {
     if (!is.null(current_sel)) {
       selected_word(NULL)
     }
     return()
   }
   
   if (scatter_sel != "" && 
       (is.null(current_sel) || scatter_sel != current_sel)) {
     selected_word(scatter_sel)
   }
 }, ignoreInit = TRUE, ignoreNULL = FALSE)

 # Debug: Log selected word changes (only in debug mode)
 if (DEBUG_MODE) {
   observeEvent(selected_word(), {
     if (!is.null(selected_word())) {
       message(paste("Selected word:", selected_word()))
     }
   })
 }
}

# ------------------------------------------------------------------------------
# Run Application
# ------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)
