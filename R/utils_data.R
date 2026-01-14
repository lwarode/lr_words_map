# ==============================================================================
# Data Utilities for Left-Right Associations App
# ==============================================================================
# Functions for loading, processing, and transforming data.
# Loads lr_pos_sem.csv as primary data source.
# ==============================================================================

library(dplyr)
library(readr)

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

# Data directory path
DATA_DIR <- "data"

# ------------------------------------------------------------------------------
# Main Data Loading Function
# ------------------------------------------------------------------------------

load_app_data <- function(use_real_data = TRUE, data_dir = DATA_DIR) {

 if (use_real_data) {
   message("Loading real data from lr_pos_sem.csv...")
   data <- load_real_data(data_dir)
 } else {
   message("Generating placeholder data...")
   source("R/generate_placeholder_data.R", local = TRUE)
   data <- generate_all_placeholder_data(save_to_disk = FALSE)
 }

 # Add computed fields
 data <- enrich_data(data)

 return(data)
}

# ------------------------------------------------------------------------------
# Load Real Data from lr_pos_sem.csv
# ------------------------------------------------------------------------------

load_real_data <- function(data_dir = DATA_DIR) {

 # Primary data file
 lr_pos_sem_file <- file.path(data_dir, "lr_pos_sem.csv")
 
 if (!file.exists(lr_pos_sem_file)) {
   stop(paste("Missing data file:", lr_pos_sem_file))
 }

 # Load lr_pos_sem data
 lr_pos_sem <- read_csv(lr_pos_sem_file, show_col_types = FALSE)
 
 # Transform to app format - map column names
 word_associations <- lr_pos_sem %>%
   mutate(
     # Map columns to expected names
     semantic_score = association_score,
     lr_position_mean = lr_self_mean,
     frequency = total_freq,
     word_en = eng_word_transl,
     # Determine quadrant based on lr_semantic and position
     # lr_semantic: "left", "right", or "neutral"
     quadrant = case_when(
       lr_semantic == "left" & lr_self_mean < 6 ~ "in_left",
       lr_semantic == "left" & lr_self_mean >= 6 ~ "out_left",
       lr_semantic == "right" & lr_self_mean >= 6 ~ "in_right",
       lr_semantic == "right" & lr_self_mean < 6 ~ "out_right",
       TRUE ~ "neutral"
     )
   ) %>%
   # Filter to words with sufficient data
   filter(!is.na(lr_position_mean), !is.na(semantic_score)) %>%
   select(
     word, 
     word_en = eng_word_transl,
     semantic_score = association_score,
     lr_position_mean = lr_self_mean,
     lr_semantic,
     frequency = total_freq,
     left_freq,
     right_freq,
     lr_self_mean_left,
     lr_self_mean_right,
     quadrant
   )

 # Create position distributions from raw data
 # This will be populated per-word when needed
 position_distributions <- data.frame()
 
 # Load individual-level data for distribution plots (if available)
 df_exp_left <- NULL
 df_exp_right <- NULL
 
 df_exp_left_file <- file.path(data_dir, "df_exp_left.csv")
 df_exp_right_file <- file.path(data_dir, "df_exp_right.csv")
 
 if (file.exists(df_exp_left_file)) {
   df_exp_left <- read_csv(df_exp_left_file, show_col_types = FALSE)
   message(paste("Loaded", nrow(df_exp_left), "left association records"))
 }
 
 if (file.exists(df_exp_right_file)) {
   df_exp_right <- read_csv(df_exp_right_file, show_col_types = FALSE)
   message(paste("Loaded", nrow(df_exp_right), "right association records"))
 }

 data <- list(
   word_associations = word_associations,
   position_distributions = position_distributions,
   lr_pos_sem_raw = lr_pos_sem,  # Keep raw data for distribution plots
   df_exp_left = df_exp_left,    # Individual-level left associations
   df_exp_right = df_exp_right   # Individual-level right associations
 )

 return(data)
}

# ------------------------------------------------------------------------------
# Enrich Data with Computed Fields
# ------------------------------------------------------------------------------

enrich_data <- function(data) {

 # Add quadrant labels for display
 quadrant_labels <- c(
   "in_left" = "In-Ideology Left",
   "out_left" = "Out-Ideology Left",
   "in_right" = "In-Ideology Right",
   "out_right" = "Out-Ideology Right",
   "neutral" = "Neutral"
 )

 data$word_associations <- data$word_associations %>%
   mutate(
     quadrant_label = quadrant_labels[quadrant],
     # Display label combining German and English
     display_label = paste0(word, " (", word_en, ")"),
     # Tooltip text
     tooltip_text = paste0(
       "<b>", word_en, "</b> (", word, ")<br>",
       "Position: ", round(lr_position_mean, 2), "<br>",
       "Semantic: ", round(semantic_score, 2), "<br>",
       "Total Freq: ", frequency, "<br>",
       "Left Freq: ", left_freq, " | Right Freq: ", right_freq
     )
   )

 # Calculate global means for reference lines
 data$global_stats <- list(
   lr_position_mean = mean(data$word_associations$lr_position_mean, na.rm = TRUE),
   semantic_score_mean = mean(data$word_associations$semantic_score, na.rm = TRUE),
   n_words = nrow(data$word_associations)
 )
 
 # Get top 100 words by frequency for dropdown
 data$top_words <- data$word_associations %>%
   arrange(desc(frequency)) %>%
   slice_head(n = 100) %>%
   pull(word)

 return(data)
}

# ------------------------------------------------------------------------------
# Data Access Functions
# ------------------------------------------------------------------------------

#' Get word data for a specific word
get_word_data <- function(data, word) {
 data$word_associations %>%
   filter(word == !!word | word_en == !!word)
}

#' Get distribution data for a specific word (from raw lr_pos_sem data)
get_word_distribution <- function(data, word_name) {
 # Return the word's distribution info from word_associations
 word_data <- data$word_associations %>%
   filter(word == word_name)
 
 if (nrow(word_data) == 0) return(NULL)
 
 return(word_data)
}

# ------------------------------------------------------------------------------
# Summary Statistics Functions
# ------------------------------------------------------------------------------

#' Get summary statistics for the dataset
get_data_summary <- function(data) {

 word_stats <- data$word_associations %>%
   group_by(quadrant) %>%
   summarise(
     n_words = n(),
     total_frequency = sum(frequency),
     avg_frequency = mean(frequency),
     .groups = "drop"
   )

 list(
   total_words = nrow(data$word_associations),
   words_by_quadrant = word_stats,
   lr_range = range(data$word_associations$lr_position_mean, na.rm = TRUE),
   semantic_range = range(data$word_associations$semantic_score, na.rm = TRUE)
 )
}

# ------------------------------------------------------------------------------
# Search and Filter Functions
# ------------------------------------------------------------------------------

#' Search words by partial match
search_words <- function(data, query, language = "both") {

 query_lower <- tolower(query)

 results <- data$word_associations %>%
   filter(
     if (language == "german") {
       grepl(query_lower, tolower(word))
     } else if (language == "english") {
       grepl(query_lower, tolower(word_en))
     } else {
       grepl(query_lower, tolower(word)) | grepl(query_lower, tolower(word_en))
     }
   ) %>%
   arrange(desc(frequency))

 return(results)
}

#' Filter words by position and semantic score ranges
filter_words <- function(data,
                        lr_range = c(1, 11),
                        semantic_range = c(-1, 1),
                        quadrants = NULL,
                        min_frequency = 0) {

 filtered <- data$word_associations %>%
   filter(
     lr_position_mean >= lr_range[1],
     lr_position_mean <= lr_range[2],
     semantic_score >= semantic_range[1],
     semantic_score <= semantic_range[2],
     frequency >= min_frequency
   )

 if (!is.null(quadrants)) {
   filtered <- filtered %>%
     filter(quadrant %in% quadrants)
 }

 return(filtered)
}
