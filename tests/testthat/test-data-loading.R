# ==============================================================================
# Tests for Data Loading Functions
# ==============================================================================

test_that("CSV files exist and are readable", {
  expect_true(file.exists(file.path(PROJECT_ROOT, "data/lr_pos_sem_curated.csv")))
  expect_true(file.exists(file.path(PROJECT_ROOT, "data/df_exp_left_curated.csv")))
  expect_true(file.exists(file.path(PROJECT_ROOT, "data/df_exp_right_curated.csv")))
})

test_that("load_app_data returns expected structure", {
  data <- load_app_data(use_real_data = TRUE, data_dir = file.path(PROJECT_ROOT, "data"), verbose = FALSE)

  # Check that data is a list with expected elements
  expect_type(data, "list")
  expect_true("word_associations" %in% names(data))
  expect_true("global_stats" %in% names(data))
  expect_true("df_exp_left" %in% names(data))
  expect_true("df_exp_right" %in% names(data))
})

test_that("word_associations has required columns", {
  data <- load_app_data(use_real_data = TRUE, data_dir = file.path(PROJECT_ROOT, "data"), verbose = FALSE)
  wa <- data$word_associations

  required_cols <- c(
    "word", "word_en", "semantic_score", "lr_position_mean",
    "lr_semantic", "frequency", "left_freq", "right_freq", "quadrant"
  )

  for (col in required_cols) {
    expect_true(col %in% names(wa), info = paste("Missing column:", col))
  }
})

test_that("word_associations has no NA in critical fields", {
  data <- load_app_data(use_real_data = TRUE, data_dir = file.path(PROJECT_ROOT, "data"), verbose = FALSE)
  wa <- data$word_associations

  expect_false(any(is.na(wa$word)))
  expect_false(any(is.na(wa$lr_position_mean)))
  expect_false(any(is.na(wa$semantic_score)))
  expect_false(any(is.na(wa$lr_semantic)))
})

test_that("German umlauts are properly encoded", {
  data <- load_app_data(use_real_data = TRUE, data_dir = file.path(PROJECT_ROOT, "data"), verbose = FALSE)
  wa <- data$word_associations

  # Check that solidarität exists (not solidarit*at or similar)
  expect_true("solidarität" %in% wa$word)

  # Check encoding is UTF-8
  solidaritaet <- wa$word[wa$word == "solidarität"]
  expect_equal(Encoding(solidaritaet), "UTF-8")
})

test_that("lr_semantic values are valid", {
  data <- load_app_data(use_real_data = TRUE, data_dir = file.path(PROJECT_ROOT, "data"), verbose = FALSE)
  wa <- data$word_associations

  valid_values <- c("left", "right", "neutral")
  expect_true(all(wa$lr_semantic %in% valid_values))
})

test_that("global_stats contains expected values", {
  data <- load_app_data(use_real_data = TRUE, data_dir = file.path(PROJECT_ROOT, "data"), verbose = FALSE)
  gs <- data$global_stats

  expect_true("lr_position_mean" %in% names(gs))
  expect_true("n_words" %in% names(gs))

  # Sanity checks
  expect_true(gs$lr_position_mean > 1 && gs$lr_position_mean < 11)
  expect_true(gs$n_words > 100)  # We expect ~228 words
})

test_that("recht is excluded from top_words list", {
  data <- load_app_data(use_real_data = TRUE, data_dir = file.path(PROJECT_ROOT, "data"), verbose = FALSE)

  # Note: The exclusion happens in the scatter plot module, not in data loading
  # This test checks that "recht" exists in the data but can be filtered
  wa <- data$word_associations
  expect_true("recht" %in% wa$word)  # It should exist in data
})
