#!/usr/bin/env Rscript
# ==============================================================================
# Deployment Script for Left-Right Associations App
# ==============================================================================
# This script helps deploy the app to shinyapps.io
#
# Prerequisites:
# 1. Install rsconnect: install.packages("rsconnect")
# 2. Configure your shinyapps.io account (see below)
#
# Usage:
#   Rscript deploy.R           # Deploy the app
#   Rscript deploy.R --check   # Check dependencies only
# ==============================================================================

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

# App name on shinyapps.io (change if desired)
APP_NAME <- "lr-words-map"

# Required packages for the app
REQUIRED_PACKAGES <- c(
  "shiny",
  "bslib",
  "plotly",
  "ggplot2",
  "ggrepel",
  "dplyr",
  "tidyr",
  "readr",
  "htmltools",
  "showtext",
  "sysfonts"
)

# Optional packages
OPTIONAL_PACKAGES <- c(
  "shinycssloaders"  # Loading spinners
)

# ------------------------------------------------------------------------------
# Helper Functions
# ------------------------------------------------------------------------------

check_packages <- function() {
  cat("\n=== Checking Required Packages ===\n")

  all_ok <- TRUE

  for (pkg in REQUIRED_PACKAGES) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      version <- as.character(packageVersion(pkg))
      cat(sprintf("  ✓ %-18s %s\n", pkg, version))
    } else {
      cat(sprintf("  ✗ %-18s NOT INSTALLED\n", pkg))
      all_ok <- FALSE
    }
  }

  cat("\n=== Checking Optional Packages ===\n")

  for (pkg in OPTIONAL_PACKAGES) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      version <- as.character(packageVersion(pkg))
      cat(sprintf("  ✓ %-18s %s\n", pkg, version))
    } else {
      cat(sprintf("  - %-18s not installed (optional)\n", pkg))
    }
  }

  cat("\n=== Checking Deployment Package ===\n")

  if (requireNamespace("rsconnect", quietly = TRUE)) {
    version <- as.character(packageVersion("rsconnect"))
    cat(sprintf("  ✓ %-18s %s\n", "rsconnect", version))
  } else {
    cat("  ✗ rsconnect         NOT INSTALLED\n")
    cat("\n  To install: install.packages('rsconnect')\n")
    all_ok <- FALSE
  }

  return(all_ok)
}

check_account <- function() {
  cat("\n=== Checking shinyapps.io Account ===\n")

  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    cat("  ✗ rsconnect not installed - cannot check account\n")
    return(FALSE)
  }

  accounts <- rsconnect::accounts()

  if (nrow(accounts) == 0) {
    cat("  ✗ No shinyapps.io account configured\n")
    cat("\n  To configure your account:\n")
    cat("  1. Go to https://www.shinyapps.io/admin/#/tokens\n")
    cat("  2. Click 'Show Token' and copy the rsconnect::setAccountInfo() command\n")
    cat("  3. Run that command in R\n")
    return(FALSE)
  }

  cat(sprintf("  ✓ Account configured: %s\n", accounts$name[1]))
  return(TRUE)
}

check_data_files <- function() {
  cat("\n=== Checking Data Files ===\n")

  required_files <- c(
    "data/lr_pos_sem_curated.csv",
    "data/df_exp_left_curated.csv",
    "data/df_exp_right_curated.csv"
  )

  all_ok <- TRUE

  for (f in required_files) {
    if (file.exists(f)) {
      size <- file.info(f)$size
      cat(sprintf("  ✓ %-35s %s\n", f, format(size, big.mark = ",")))
    } else {
      cat(sprintf("  ✗ %-35s MISSING\n", f))
      all_ok <- FALSE
    }
  }

  return(all_ok)
}

deploy_app <- function() {
  cat("\n=== Deploying to shinyapps.io ===\n")

  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    stop("rsconnect package not installed. Run: install.packages('rsconnect')")
  }

  # Files to deploy
  app_files <- c(
    "app.R",
    "R/",
    "www/",
    "data/"
  )

  cat("Deploying app:", APP_NAME, "\n")
  cat("Files included:", paste(app_files, collapse = ", "), "\n\n")

  rsconnect::deployApp(
    appDir = getwd(),
    appName = APP_NAME,
    appFiles = app_files,
    forceUpdate = TRUE
  )
}

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

if ("--check" %in% args) {
  # Check mode - just verify everything is ready
  cat("========================================\n")
  cat("Deployment Readiness Check\n")
  cat("========================================\n")

  pkg_ok <- check_packages()
  data_ok <- check_data_files()
  account_ok <- check_account()

  cat("\n========================================\n")
  if (pkg_ok && data_ok && account_ok) {
    cat("✓ All checks passed! Ready to deploy.\n")
    cat("  Run: Rscript deploy.R\n")
  } else {
    cat("✗ Some checks failed. Please fix the issues above.\n")
  }
  cat("========================================\n")

} else {
  # Deploy mode
  cat("========================================\n")
  cat("Deploying Left-Right Associations App\n")
  cat("========================================\n")

  # Run checks first
  pkg_ok <- check_packages()
  data_ok <- check_data_files()
  account_ok <- check_account()

  if (!pkg_ok || !data_ok || !account_ok) {
    cat("\n✗ Pre-deployment checks failed. Please fix the issues above.\n")
    quit(status = 1)
  }

  # Deploy
  deploy_app()
}
