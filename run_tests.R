#!/usr/bin/env Rscript
# ==============================================================================
# Test Runner for Left-Right Associations App
# ==============================================================================
# Run with: Rscript run_tests.R
# ==============================================================================

# Change to the project root directory
# Use commandArgs to get script path (works with Rscript)
args <- commandArgs(trailingOnly = FALSE)
script_arg <- args[grep("--file=", args)]
if (length(script_arg) > 0) {
  script_path <- sub("--file=", "", script_arg)
  script_dir <- dirname(normalizePath(script_path))
  setwd(script_dir)
}

# Set UTF-8 locale for proper German umlaut handling
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# Load required packages
library(testthat)
library(dplyr)
library(readr)
library(ggplot2)
library(ggrepel)
library(plotly)
library(htmltools)

# Source the app files
source("R/utils_data.R")
source("R/utils_plots.R")

# Run tests
cat("\n========================================\n")
cat("Running Left-Right Associations App Tests\n")
cat("Working directory:", getwd(), "\n")
cat("========================================\n\n")

test_results <- test_dir("tests/testthat", reporter = "summary")

cat("\n========================================\n")
cat("Test run complete\n")
cat("========================================\n")
