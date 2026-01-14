# ==============================================================================
# Install Required Packages for Left-Right Associations App
# ==============================================================================
# Run this script once to install all required packages
# ==============================================================================

# List of required packages
required_packages <- c(
  "shiny",
  "bslib",
  "plotly",
  "ggplot2",
  "dplyr",
  "tidyr",
  "readr",
  "purrr",
  "htmltools",
  "shinycssloaders",  # Optional but recommended for loading spinners
  "showtext",  # For custom font rendering in plots
  "sysfonts"   # Font management for showtext
)

# Function to install missing packages
install_if_missing <- function(packages) {
  missing <- packages[!(packages %in% installed.packages()[, "Package"])]

  if (length(missing) > 0) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, dependencies = TRUE)
  } else {
    message("All required packages are already installed!")
  }
}

# Install missing packages
install_if_missing(required_packages)

# Verify installation
message("\nPackage verification:")
for (pkg in required_packages) {
  status <- if (requireNamespace(pkg, quietly = TRUE)) "OK" else "MISSING"
  message(sprintf("  %-20s: %s", pkg, status))
}

message("\nDone! You can now run the app with: shiny::runApp()")
