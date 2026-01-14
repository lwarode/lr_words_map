# ==============================================================================
# Placeholder Data Generation for Left-Right Associations App
# ==============================================================================
# This script generates realistic placeholder data based on the paper's findings.
# When real data is available, set USE_REAL_DATA <- TRUE in the main app.
# ==============================================================================

library(dplyr)
library(tidyr)

# ------------------------------------------------------------------------------
# German Party Definitions
# ------------------------------------------------------------------------------

get_party_definitions <- function() {
  tibble::tibble(
    party = c("LINKE", "GRUENE", "SPD", "FDP", "CDU", "CSU", "AfD"),
    party_full = c(
      "DIE LINKE",
      "Buendnis 90/Die Gruenen
",
      "SPD",
      "FDP",
      "CDU",
      "CSU",
      "AfD"
    ),
    party_color = c("#BE3075", "#1AA037", "#E3000F", "#FFED00", "#000000", "#008AC5", "#009EE0"),
    lr_mean = c(2.0, 3.3, 3.5, 6.3, 7.3, 7.5, 8.3),
    lr_sd = c(0.8, 1.0, 1.0, 1.2, 1.0, 1.0, 1.5)
  )
}

# ------------------------------------------------------------------------------
# Word Associations Data (Based on Figure 5 & 6 from paper)
# ------------------------------------------------------------------------------

generate_word_associations <- function() {

 # Words from the paper organized by quadrant
 # Quadrant definitions:
 # - in_left: Left-leaning position, left semantics (lower-left)
 # - out_left: Right-leaning position, left semantics (lower-right)
 # - in_right: Right-leaning position, right semantics (upper-right)
 # - out_right: Left-leaning position, right semantics (upper-left)
 # - center: Near center on both dimensions

 words_data <- tibble::tribble(
   ~word, ~word_en, ~lr_position_mean, ~semantic_score, ~frequency, ~quadrant,

 # IN-IDEOLOGY LEFT (lower-left quadrant) - positive left associations
   "gerechtigkeit", "justice", 3.2, -0.92, 280, "in_left",
   "frieden", "peace", 3.0, -0.95, 195, "in_left",
   "sozial", "social", 3.5, -0.78, 320, "in_left",
   "solidaritaet", "solidarity", 2.9, -0.88, 175, "in_left",
   "toleranz", "tolerance", 3.3, -0.72, 145, "in_left",
   "gleichheit", "equality", 3.4, -0.85, 210, "in_left",
   "teilhabe", "participation", 3.1, -0.80, 95, "in_left",
   "chancengleichheit", "equal opportunities", 3.2, -0.91, 88, "in_left",
   "weltoffen", "cosmopolitan", 3.0, -0.75, 72, "in_left",
   "gerecht", "just", 3.3, -0.82, 165, "in_left",
   "ausgleich", "compensation", 3.6, -0.70, 68, "in_left",
   "progressiv", "progressive", 3.1, -0.88, 82, "in_left",
   "offen", "open", 3.4, -0.65, 155, "in_left",
   "solidarisch", "solidary", 2.8, -0.90, 78, "in_left",
   "leben", "life", 4.0, -0.55, 125, "in_left",

   # OUT-IDEOLOGY LEFT (lower-right quadrant) - negative left associations
   "sozialismus", "socialism", 7.2, -0.82, 185, "out_left",
   "sozialistisch", "socialist", 7.4, -0.85, 95, "out_left",
   "bevormundung", "paternalism", 7.0, -0.72, 65, "out_left",
   "gleichmacherei", "egalitarianism", 7.5, -0.78, 55, "out_left",
   "umverteilung", "redistribution", 6.2, -0.68, 120, "out_left",
   "sozialstaat", "welfare state", 5.8, -0.72, 95, "out_left",
   "staatlich", "state", 6.5, -0.65, 88, "out_left",
   "kommunistisch", "communist", 7.8, -0.88, 45, "out_left",
   "enteignung", "expropriation", 7.6, -0.75, 38, "out_left",
   "kollektivismus", "collectivism", 7.3, -0.70, 32, "out_left",
   "staatsgläubigkeit", "faith in state", 7.1, -0.68, 42, "out_left",
   "ideologie", "ideology", 6.8, -0.58, 75, "out_left",
   "schulden", "debts", 7.0, -0.52, 55, "out_left",
   "multikulti", "multicultural", 7.2, -0.62, 48, "out_left",
   "sozialleistungen", "social benefits", 6.4, -0.70, 62, "out_left",

   # IN-IDEOLOGY RIGHT (upper-right quadrant) - positive right associations
   "patriotismus", "patriotism", 7.8, 0.92, 85, "in_right",
   "patriotisch", "patriotic", 7.6, 0.88, 65, "in_right",
   "heimat", "homeland", 7.5, 0.85, 95, "in_right",
   "familie", "family", 7.2, 0.72, 145, "in_right",
   "ordnung", "order", 7.0, 0.68, 110, "in_right",
   "rechtsstaatlichkeit", "rule of law", 6.8, 0.62, 78, "in_right",
   "eigenverantwortung", "personal responsibility", 7.1, 0.70, 92, "in_right",
   "leistungsprinzip", "performance principle", 7.3, 0.65, 68, "in_right",
   "stabilitaet", "stability", 6.9, 0.58, 88, "in_right",
   "werte", "values", 6.5, 0.55, 135, "in_right",
   "buergerlich", "bourgeois", 7.4, 0.78, 55, "in_right",
   "innere", "inner", 7.0, 0.52, 72, "in_right",
   "leistungsgerechtigkeit", "performance-based justice", 7.2, 0.60, 48, "in_right",
   "sicherheit", "security", 6.2, 0.45, 165, "in_right",
   "ueberbetonung", "overemphasis", 6.8, 0.58, 42, "in_right",

   # OUT-IDEOLOGY RIGHT (upper-left quadrant) - negative right associations
   "rassismus", "racism", 2.8, 0.95, 195, "out_right",
   "rassistisch", "racist", 2.6, 0.92, 125, "out_right",
   "faschismus", "fascism", 2.5, 0.98, 145, "out_right",
   "nationalismus", "nationalism", 3.2, 0.88, 210, "out_right",
   "nationalistisch", "nationalist", 3.0, 0.85, 95, "out_right",
   "konservatismus", "conservatism", 4.2, 0.82, 165, "out_right",
   "konservativ", "conservative", 4.5, 0.78, 185, "out_right",
   "auslaenderfeindlich", "xenophobic", 2.7, 0.90, 88, "out_right",
   "fremdenfeindlichkeit", "xenophobia", 2.9, 0.88, 75, "out_right",
   "ausgrenzung", "exclusion", 3.3, 0.72, 95, "out_right",
   "ausgrenzend", "exclusionary", 3.1, 0.75, 55, "out_right",
   "intoleranz", "intolerance", 3.0, 0.70, 68, "out_right",
   "egoismus", "egoism", 3.5, 0.82, 72, "out_right",
   "egoistisch", "egoistical", 3.4, 0.78, 58, "out_right",
   "rueckwaertsgewandt", "backward-looking", 3.2, 0.85, 82, "out_right",
   "reaktionaer", "reactionary", 3.0, 0.80, 55, "out_right",
   "rechtsextrem", "right-wing extremist", 7.0, 0.88, 65, "in_right",
   "hass", "hate", 3.5, 0.72, 85, "out_right",
   "abgrenzung", "demarcation", 4.0, 0.65, 78, "out_right",
   "antisemitismus", "anti-Semitism", 3.8, 0.75, 45, "out_right",
   "krieg", "war", 4.2, 0.58, 62, "out_right",
   "markt", "market", 4.5, 0.62, 95, "out_right",
   "neoliberal", "neoliberal", 4.0, 0.72, 88, "out_right",
   "unsozial", "antisocial", 3.6, 0.68, 52, "out_right",

   # CENTER - neutral/non-discriminatory words
   "staat", "state", 5.9, 0.05, 245, "center",
   "politik", "politics/policy", 4.8, -0.02, 310, "center",
   "menschen", "people", 4.5, -0.12, 285, "center",
   "freiheit", "freedom", 5.2, 0.08, 225, "center",
   "wirtschaft", "economy", 5.5, 0.15, 175, "center",
   "verantwortung", "responsibility", 5.8, 0.10, 145, "center",
   "gesellschaft", "society", 4.6, -0.08, 195, "center",
   "demokratie", "democracy", 4.2, -0.18, 165, "center",
   "interessen", "interests", 5.0, 0.12, 125, "center",
   "ungleichheit", "inequality", 4.3, 0.22, 95, "center",
   "liberal", "liberal", 5.5, 0.05, 115, "center",
   "national", "national", 5.2, 0.35, 135, "center",
   "minderheiten", "minorities", 4.0, -0.15, 72, "center",
   "steuern", "taxes", 5.3, 0.18, 88, "center"
 )

 # Add frequency columns for left/right associations
 words_data <- words_data %>%
   mutate(
     # Distribute frequency based on semantic score
     freq_left_assoc = case_when(
       semantic_score < -0.3 ~ as.integer(frequency * runif(n(), 0.7, 0.95)),
       semantic_score > 0.3 ~ as.integer(frequency * runif(n(), 0.05, 0.3)),
       TRUE ~ as.integer(frequency * runif(n(), 0.4, 0.6))
     ),
     freq_right_assoc = frequency - freq_left_assoc
   )

 return(words_data)
}

# ------------------------------------------------------------------------------
# Position Distributions Data
# ------------------------------------------------------------------------------

generate_position_distributions <- function(word_associations = NULL) {

 if (is.null(word_associations)) {
   word_associations <- generate_word_associations()
 }

 parties <- get_party_definitions()

 # Generate distribution data for each word
 distributions <- purrr::map_dfr(1:nrow(word_associations), function(i) {
   word_row <- word_associations[i, ]
   word <- word_row$word
   quadrant <- word_row$quadrant
   lr_mean <- word_row$lr_position_mean
   freq <- word_row$frequency
   semantic <- word_row$semantic_score

   # Generate left association distribution
   # More left-leaning individuals associate semantically left words with "left"
   if (semantic < -0.3) {
     left_assoc_mean <- lr_mean - runif(1, 0.5, 1.5)
     right_assoc_mean <- lr_mean + runif(1, 1.0, 2.5)
   } else if (semantic > 0.3) {
     left_assoc_mean <- lr_mean - runif(1, 1.0, 2.5)
     right_assoc_mean <- lr_mean + runif(1, 0.5, 1.5)
   } else {
     # Center words - smaller difference
     left_assoc_mean <- lr_mean - runif(1, 0.2, 0.8)
     right_assoc_mean <- lr_mean + runif(1, 0.2, 0.8)
   }

   # Clamp means to valid range
   left_assoc_mean <- max(1.5, min(10.5, left_assoc_mean))
   right_assoc_mean <- max(1.5, min(10.5, right_assoc_mean))

   # Generate counts for each position (1-11)
   left_counts <- sapply(1:11, function(pos) {
     # Use normal distribution centered on mean
     density_val <- dnorm(pos, mean = left_assoc_mean, sd = 1.8)
     round(density_val * word_row$freq_left_assoc * 5)
   })

   right_counts <- sapply(1:11, function(pos) {
     density_val <- dnorm(pos, mean = right_assoc_mean, sd = 1.8)
     round(density_val * word_row$freq_right_assoc * 5)
   })

   # Combine into dataframe
   bind_rows(
     tibble(
       word = word,
       association_with = "left",
       lr_position = 1:11,
       count = as.integer(pmax(0, left_counts))
     ),
     tibble(
       word = word,
       association_with = "right",
       lr_position = 1:11,
       count = as.integer(pmax(0, right_counts))
     )
   )
 })

 return(distributions)
}

# ------------------------------------------------------------------------------
# Party Usage Data
# ------------------------------------------------------------------------------

generate_party_usage <- function(word_associations = NULL) {

 if (is.null(word_associations)) {
   word_associations <- generate_word_associations()
 }

 parties <- get_party_definitions()

 # Generate party usage for each word
 party_usage <- purrr::map_dfr(1:nrow(word_associations), function(i) {
   word_row <- word_associations[i, ]
   word <- word_row$word
   lr_mean <- word_row$lr_position_mean
   freq <- word_row$frequency
   quadrant <- word_row$quadrant

   # Calculate expected usage per party based on ideological distance
   party_counts <- sapply(1:nrow(parties), function(j) {
     party_lr <- parties$lr_mean[j]

     # Base probability inversely related to distance
     distance <- abs(lr_mean - party_lr)

     # Adjust based on quadrant - in-ideology words used more by aligned parties
     if (quadrant == "in_left" && party_lr < 5) {
       base_prob <- exp(-distance * 0.3) * 1.5
     } else if (quadrant == "in_right" && party_lr > 6) {
       base_prob <- exp(-distance * 0.3) * 1.5
     } else if (quadrant == "out_left" && party_lr > 6) {
       base_prob <- exp(-distance * 0.2) * 1.3
     } else if (quadrant == "out_right" && party_lr < 5) {
       base_prob <- exp(-distance * 0.2) * 1.3
     } else {
       base_prob <- exp(-distance * 0.4)
     }

     # Add some noise
     base_prob <- base_prob * runif(1, 0.7, 1.3)

     return(base_prob)
   })

   # Normalize and scale to frequency
   party_counts <- party_counts / sum(party_counts)
   party_counts <- round(party_counts * freq * 0.8)  # 80% coverage

   tibble(
     word = word,
     party = parties$party,
     party_full = parties$party_full,
     count = as.integer(pmax(1, party_counts)),
     party_color = parties$party_color
   )
 })

 return(party_usage)
}

# ------------------------------------------------------------------------------
# Context Database (Political Text Examples)
# ------------------------------------------------------------------------------

generate_context_database <- function(word_associations = NULL) {

 if (is.null(word_associations)) {
   word_associations <- generate_word_associations()
 }

 parties <- get_party_definitions()

 # Template sentences by theme/quadrant
 # Using {word} as placeholder
 templates <- list(
   in_left = list(
     de = c(
       "Wir setzen uns fuer {word} in unserer Gesellschaft ein.",
       "{word} ist ein zentraler Wert unserer Politik.",
       "Ohne {word} kann es keine gerechte Gesellschaft geben.",
       "Die Foerderung von {word} steht im Mittelpunkt unseres Programms.",
       "{word} muss das Fundament unserer demokratischen Ordnung sein."
     ),
     en = c(
       "We are committed to {word} in our society.",
       "{word} is a central value of our politics.",
       "Without {word}, there can be no just society.",
       "Promoting {word} is at the heart of our program.",
       "{word} must be the foundation of our democratic order."
     )
   ),
   out_left = list(
     de = c(
       "Der {word} der Linken fuehrt in die Sackgasse.",
       "Wir lehnen {word} als politisches Konzept ab.",
       "{word} hat in der Geschichte nur Schaden angerichtet.",
       "Die Ideen des {word} sind ueberholt und gefaehrlich.",
       "Gegen {word} setzen wir auf individuelle Freiheit."
     ),
     en = c(
       "The {word} of the left leads to a dead end.",
       "We reject {word} as a political concept.",
       "{word} has only caused harm throughout history.",
       "The ideas of {word} are outdated and dangerous.",
       "Against {word}, we advocate for individual freedom."
     )
   ),
   in_right = list(
     de = c(
       "{word} ist ein Grundpfeiler unserer Wertordnung.",
       "Wir stehen fuer {word} und Verantwortung.",
       "Die Bewahrung von {word} liegt uns am Herzen.",
       "{word} gibt den Menschen Halt und Orientierung.",
       "Ohne {word} verliert unsere Gesellschaft ihren Zusammenhalt."
     ),
     en = c(
       "{word} is a cornerstone of our value system.",
       "We stand for {word} and responsibility.",
       "Preserving {word} is close to our hearts.",
       "{word} gives people support and orientation.",
       "Without {word}, our society loses its cohesion."
     )
   ),
   out_right = list(
     de = c(
       "Wir bekaempfen {word} in all seinen Formen.",
       "{word} hat keinen Platz in unserer Demokratie.",
       "Der wachsende {word} ist eine Gefahr fuer unsere Gesellschaft.",
       "Gegen {word} muessen wir entschieden vorgehen.",
       "{word} widerspricht unseren demokratischen Grundwerten."
     ),
     en = c(
       "We fight {word} in all its forms.",
       "{word} has no place in our democracy.",
       "Growing {word} is a danger to our society.",
       "We must take decisive action against {word}.",
       "{word} contradicts our fundamental democratic values."
     )
   ),
   center = list(
     de = c(
       "Die Frage nach {word} beschaeftigt alle politischen Lager.",
       "{word} ist ein Thema, das uns alle betrifft.",
       "Beim Thema {word} gibt es unterschiedliche Ansichten.",
       "Die Debatte um {word} muss sachlich gefuehrt werden.",
       "{word} erfordert pragmatische Loesungen."
     ),
     en = c(
       "The question of {word} concerns all political camps.",
       "{word} is an issue that affects us all.",
       "There are different views on the topic of {word}.",
       "The debate about {word} must be conducted objectively.",
       "{word} requires pragmatic solutions."
     )
   )
 )

 # Source types
 source_types <- c(
   "Parliamentary Speech",
   "Press Release",
   "Interview",
   "Party Conference",
   "Social Media"
 )

 # Generate 100 context entries
 set.seed(42)  # For reproducibility

 # Sample words ensuring coverage
 n_entries <- 100
 sampled_indices <- sample(1:nrow(word_associations), n_entries, replace = TRUE,
                           prob = word_associations$frequency / sum(word_associations$frequency))

 context_data <- purrr::map_dfr(1:n_entries, function(i) {
   word_row <- word_associations[sampled_indices[i], ]
   word <- word_row$word
   word_en <- word_row$word_en
   quadrant <- word_row$quadrant

   # Select appropriate template
   template_set <- templates[[quadrant]]
   template_de <- sample(template_set$de, 1)
   template_en <- sample(template_set$en, 1)

   # Fill in word
   text_de <- gsub("\\{word\\}", word, template_de)
   text_en <- gsub("\\{word\\}", word_en, template_en)

   # Select party based on quadrant
   if (quadrant %in% c("in_left", "out_right")) {
     party_weights <- ifelse(parties$lr_mean < 5, 3, 1)
   } else if (quadrant %in% c("in_right", "out_left")) {
     party_weights <- ifelse(parties$lr_mean > 5, 3, 1)
   } else {
     party_weights <- rep(1, nrow(parties))
   }
   selected_party <- sample(parties$party, 1, prob = party_weights)

   # Generate random date between 2013 and 2021
   random_days <- sample(0:2900, 1)
   date <- as.Date("2013-01-01") + random_days

   # Generate speaker name
   first_names <- c("Thomas", "Maria", "Stefan", "Anna", "Michael", "Julia",
                    "Andreas", "Sabine", "Peter", "Christine", "Klaus", "Monika")
   last_names <- c("Mueller", "Schmidt", "Schneider", "Fischer", "Weber",
                   "Meyer", "Wagner", "Becker", "Schulz", "Hoffmann")
   speaker_name <- paste(sample(first_names, 1), sample(last_names, 1))

   tibble(
     id = i,
     text = text_de,
     text_en = text_en,
     words_contained = word,
     speaker_party = selected_party,
     speaker_name = speaker_name,
     date = date,
     source_type = sample(source_types, 1)
   )
 })

 return(context_data)
}

# ------------------------------------------------------------------------------
# Master Function: Generate All Placeholder Data
# ------------------------------------------------------------------------------

generate_all_placeholder_data <- function(save_to_disk = FALSE, data_dir = "data") {

 message("Generating word associations...")
 word_associations <- generate_word_associations()

 message("Generating position distributions...")
 position_distributions <- generate_position_distributions(word_associations)

 message("Generating party usage data...")
 party_usage <- generate_party_usage(word_associations)

 message("Generating context database...")
 context_database <- generate_context_database(word_associations)

 data_list <- list(
   word_associations = word_associations,
   position_distributions = position_distributions,
   party_usage = party_usage,
   context_database = context_database,
   party_definitions = get_party_definitions()
 )

 if (save_to_disk) {
   if (!dir.exists(data_dir)) {
     dir.create(data_dir, recursive = TRUE)
   }

   message(paste("Saving data to", data_dir, "..."))
   readr::write_csv(word_associations, file.path(data_dir, "word_associations.csv"))
   readr::write_csv(position_distributions, file.path(data_dir, "position_distributions.csv"))
   readr::write_csv(party_usage, file.path(data_dir, "party_usage.csv"))
   readr::write_csv(context_database, file.path(data_dir, "context_database.csv"))
   message("Data saved successfully!")
 }

 return(data_list)
}

# ------------------------------------------------------------------------------
# Utility: Validate Data Structure
# ------------------------------------------------------------------------------

validate_data_structure <- function(data_list) {

 required_cols <- list(
   word_associations = c("word", "word_en", "lr_position_mean", "semantic_score",
                         "frequency", "quadrant"),
   position_distributions = c("word", "association_with", "lr_position", "count"),
   party_usage = c("word", "party", "party_full", "count", "party_color"),
   context_database = c("id", "text", "text_en", "words_contained",
                        "speaker_party", "speaker_name", "date", "source_type")
 )

 validation_results <- list()

 for (name in names(required_cols)) {
   if (!name %in% names(data_list)) {
     validation_results[[name]] <- list(valid = FALSE, message = "Dataset missing")
     next
   }

   missing_cols <- setdiff(required_cols[[name]], names(data_list[[name]]))
   if (length(missing_cols) > 0) {
     validation_results[[name]] <- list(
       valid = FALSE,
       message = paste("Missing columns:", paste(missing_cols, collapse = ", "))
     )
   } else {
     validation_results[[name]] <- list(valid = TRUE, message = "OK")
   }
 }

 return(validation_results)
}
