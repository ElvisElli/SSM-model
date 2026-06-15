# =============================================================================
# SSM Soybean Model - Package Installation
# =============================================================================
# Run this script once before using the model for the first time.
# It installs all required R packages.
# =============================================================================

packages_needed <- c(
  "readxl",       # Read Excel weather input files (.xlsx)
  "openxlsx",     # Read Excel output files for validation
  "dplyr",        # Data manipulation
  "tidyr",        # Data reshaping
  "ggplot2",      # Plotting and visualization
  "purrr",        # Functional programming (map, reduce)
  "jsonlite",     # Read JSON parameter files
  "scales",       # Plot scales and formatting
  "ggpmisc",      # Statistical annotations on plots
  "gridExtra",    # Arrange multiple plots
  "viridis",      # Color-blind-friendly palettes
  "RColorBrewer"  # Additional color palettes
)

# Install any missing packages
installed <- rownames(installed.packages())
to_install <- packages_needed[!packages_needed %in% installed]

if (length(to_install) > 0) {
  message("Installing packages: ", paste(to_install, collapse = ", "))
  install.packages(to_install, repos = "https://cloud.r-project.org")
} else {
  message("All required packages are already installed.")
}

# Verify installation
loaded <- sapply(packages_needed, requireNamespace, quietly = TRUE)
if (all(loaded)) {
  message("All packages successfully loaded.")
} else {
  warning("Failed to load: ", paste(names(loaded)[!loaded], collapse = ", "))
}
