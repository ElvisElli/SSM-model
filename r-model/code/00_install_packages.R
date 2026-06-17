# =============================================================================
# SSM Soybean Model - One-Time Package Setup
# =============================================================================
# Run this script once before using the model for the first time.
# It installs all required R packages from CRAN.
#
# Note: 'parallel' is part of base R and does not need to be installed.
#
# Usage:
#   Rscript r-model/code/00_install_packages.R
# Or from R console:
#   source("r-model/code/00_install_packages.R")
# =============================================================================

packages_needed <- c(
  # Core model and I/O
  "readxl",       # Read Excel weather input files (.xlsx)
  "jsonlite",     # Read JSON soil parameter files
  "dplyr",        # Data manipulation
  # Validation and plotting (09_validate_plots.R, 10_daily_plots.R)
  "openxlsx",     # Read Excel reference output for validation
  "tidyr",        # Data reshaping
  "ggplot2",      # Plotting
  "gridExtra",    # Multi-panel plot layouts
  "scales",       # Axis formatting
  "ggpmisc",      # R² and equation annotations on plots
  "viridis",      # Colour-blind-friendly palettes
  "RColorBrewer", # Additional colour palettes
  "purrr"         # Functional utilities (map, reduce)
)

cat("Checking required packages...\n")

installed <- rownames(installed.packages())
to_install <- packages_needed[!packages_needed %in% installed]

if (length(to_install) > 0) {
  cat("Installing:", paste(to_install, collapse = ", "), "\n")
  install.packages(to_install, repos = "https://cloud.r-project.org")
} else {
  cat("All packages already installed.\n")
}

# Verify
ok <- sapply(packages_needed, requireNamespace, quietly = TRUE)
if (all(ok)) {
  cat("\nAll packages verified successfully.\n")
  cat("You are ready to run the SSM Soybean Model.\n")
} else {
  warning("Failed to load: ", paste(names(ok)[!ok], collapse = ", "),
          "\nTry re-running this script or installing manually.")
}
