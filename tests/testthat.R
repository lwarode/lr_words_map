# ==============================================================================
# Test Runner for Left-Right Associations App
# ==============================================================================
# Run with: testthat::test_local() or devtools::test()
# ==============================================================================

library(testthat)

# Source the app files
source("R/utils_data.R")
source("R/utils_plots.R")

test_check("lr_words_map")
