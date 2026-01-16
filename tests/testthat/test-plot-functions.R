# ==============================================================================
# Tests for Plot Functions
# ==============================================================================

# Load data once for all tests
test_data <- load_app_data(
  use_real_data = TRUE,
  data_dir = file.path(PROJECT_ROOT, "data"),
  verbose = FALSE
)

test_that("create_scatter_plot returns a ggplot object", {
  p <- create_scatter_plot(
    data = test_data,
    selected_word = NULL,
    show_labels = TRUE,
    show_reference_lines = TRUE,
    top_n_global = 50,
    top_n_quadrant = 10,
    highlighted_words = NULL,
    aspect_ratio = 1
  )

  expect_s3_class(p, "ggplot")
})

test_that("create_scatter_plot handles selected word", {
  p <- create_scatter_plot(
    data = test_data,
    selected_word = "sozial",
    show_labels = TRUE,
    show_reference_lines = TRUE,
    top_n_global = 50,
    top_n_quadrant = 10,
    highlighted_words = "sozial",
    aspect_ratio = 1
  )

  expect_s3_class(p, "ggplot")
})

test_that("create_scatter_plot handles NULL aspect_ratio", {
  p <- create_scatter_plot(
    data = test_data,
    selected_word = NULL,
    show_labels = TRUE,
    show_reference_lines = TRUE,
    top_n_global = 50,
    top_n_quadrant = 10,
    highlighted_words = NULL,
    aspect_ratio = NULL
  )

  expect_s3_class(p, "ggplot")
})

test_that("create_quadrant_diagram returns ggplot with no selection", {
  p <- create_quadrant_diagram(word_info = NULL)
  expect_s3_class(p, "ggplot")
})

test_that("create_quadrant_diagram handles left-semantic word", {
  word_info <- test_data$word_associations %>%
    filter(lr_semantic == "left") %>%
    slice_head(n = 1)

  p <- create_quadrant_diagram(word_info = word_info)
  expect_s3_class(p, "ggplot")
})

test_that("create_quadrant_diagram handles right-semantic word", {
  word_info <- test_data$word_associations %>%
    filter(lr_semantic == "right") %>%
    slice_head(n = 1)

  p <- create_quadrant_diagram(word_info = word_info)
  expect_s3_class(p, "ggplot")
})

test_that("create_quadrant_diagram handles neutral word", {
  word_info <- test_data$word_associations %>%
    filter(lr_semantic == "neutral") %>%
    slice_head(n = 1)

  p <- create_quadrant_diagram(word_info = word_info)
  expect_s3_class(p, "ggplot")
})

test_that("create_distribution_plot returns ggplot for valid word", {
  word_info <- test_data$word_associations %>%
    filter(word == "sozial") %>%
    slice_head(n = 1)

  p <- create_distribution_plot(word_info = word_info, data = test_data)
  expect_s3_class(p, "ggplot")
})

test_that("create_distribution_plot handles NULL word_info", {
  p <- create_distribution_plot(word_info = NULL, data = test_data)
  expect_s3_class(p, "ggplot")  # Returns empty plot
})

test_that("create_empty_plot returns ggplot", {
  p <- create_empty_plot("Test message")
  expect_s3_class(p, "ggplot")
})

test_that("theme_academia returns valid theme", {
  theme <- theme_academia()
  expect_s3_class(theme, "theme")
})

test_that("SEMANTIC_COLORS has correct structure", {
  expect_true(exists("SEMANTIC_COLORS"))
  expect_true("left" %in% names(SEMANTIC_COLORS))
  expect_true("right" %in% names(SEMANTIC_COLORS))
  expect_true("neutral" %in% names(SEMANTIC_COLORS))
})
