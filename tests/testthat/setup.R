# ==============================================================================
# Test Setup - Run before all tests
# ==============================================================================

# Set UTF-8 locale for proper German umlaut handling
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Define project root as a global constant for tests to use
# testthat 3.x resets working directory, so we use absolute paths
PROJECT_ROOT <- normalizePath(file.path(dirname(getwd()), ".."))

# Confirm setup
message("Test setup complete. Project root: ", PROJECT_ROOT)
