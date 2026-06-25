# =============================================================================
# SSM Soybean Model - Input Reading Functions
# =============================================================================
# Functions to read all model inputs:
#   - Weather data from SSM-formatted Excel files (.xlsx)
#   - Soil profile data from CSV files
#   - Scenario table (scenarios.csv)
#   - Soil layer JSON (soil_data.json)
#
# All file paths are relative to the r-model root directory.
# =============================================================================

library(readxl)
library(dplyr)
library(jsonlite)

# --- Set the base path for inputs (relative to r-model root) ---------------
# Set this path before calling the functions below.
# Alternatively, pass `base_path` to each function.
SSM_BASE_PATH <- if (requireNamespace("here", quietly=TRUE)) here::here() else getwd()  # or set manually: SSM_BASE_PATH <- "/path/to/r-model"


# =============================================================================
# FUNCTION: read_weather
# Reads daily weather data from an SSM-formatted Excel file.
#
# The SSM weather file has:
#   Row 1:    Location name
#   Rows 2-9: Metadata (latitude, longitude, altitude, etc.)
#   Row 10:   Column headers (YEAR, DOY, SRAD, TMAX, TMIN, RAIN)
#   Row 11+:  Daily weather data
#
# Args:
#   filepath  - Full path to the SSM weather .xlsx file
#   skip_rows - Number of metadata rows before the data header (default: 9)
#
# Returns:
#   A data.frame with columns: YEAR, DOY, SRAD, TMAX, TMIN, RAIN
# =============================================================================
read_weather <- function(filepath, skip_rows = 9) {
  # Read the metadata to get location name
  meta <- suppressMessages(read_excel(filepath, col_names = FALSE, n_max = 1))
  loc_name <- as.character(meta[[1, 1]])

  # Read the daily weather data (skip metadata rows, use row 10 as header)
  wth <- suppressMessages(
    read_excel(filepath, skip = skip_rows, col_names = TRUE)
  )

  # Standardize column names (the Excel file uses YEAR, DOY, SRAD, TMAX, TMIN, RAIN)
  colnames(wth)[1:6] <- c("YEAR", "DOY", "SRAD", "TMAX", "TMIN", "RAIN")

  # Keep only the 6 core columns and remove any trailing NA rows
  wth <- wth[, 1:6]
  wth <- wth[!is.na(wth$YEAR), ]
  wth$YEAR <- as.integer(wth$YEAR)
  wth$DOY  <- as.integer(wth$DOY)

  attr(wth, "location") <- loc_name
  return(wth)
}


# =============================================================================
# FUNCTION: read_soil
# Reads soil profile data from an SSM-formatted CSV file.
#
# CSV structure:
#   Row 1:  Column headers (Layer#, DLYER, SAT, ...)
#   Row 2:  "<-- SoilRowNo" (marker)
#   Row 3:  "Code, Description"
#   Row 4:  Location name
#   Row 5:  Parameter labels (NLYER, LDRAIN, SALB, U, CN2)
#   Row 6:  Parameter values
#   Row 7+: Layer data (one row per soil layer)
#
# Args:
#   filepath - Full path to the _ssm_format.csv soil file
#
# Returns:
#   A named list with:
#     $meta   - data.frame with NLYER, LDRAIN, SALB, U, CN2
#     $layers - data.frame with layer-wise soil properties
# =============================================================================
read_soil <- function(filepath) {
  raw <- read.csv(filepath, header = FALSE, stringsAsFactors = FALSE)

  # Extract profile metadata (row 6 = index 6 in R, values for NLYER etc.)
  meta <- list(
    NLYER  = as.numeric(raw[6, 1]),   # Number of soil layers
    LDRAIN = as.numeric(raw[6, 2]),   # Drainage layer index (0 = bottom layer)
    SALB   = as.numeric(raw[6, 3]),   # Soil albedo
    U      = as.numeric(raw[6, 4]),   # Stage-1 evaporation upper limit (mm)
    CN2    = as.numeric(raw[6, 5])    # SCS curve number for runoff
  )

  # Soil name is in row 4
  soil_name <- raw[4, 1]

  # Layer data starts at row 7 (1-indexed)
  layer_data <- raw[7:nrow(raw), ]
  # Assign column names from row 1 of the CSV
  col_headers <- c("Layer", "DLYER", "SAT", "DUL", "LL", "ADRY",
                   "iWL", "DRAINF", "FG", "BDL", "NORG", "FMIN", "iNSOL")
  colnames(layer_data) <- col_headers[1:ncol(layer_data)]

  # Convert to numeric and remove incomplete rows
  layer_data <- layer_data[!is.na(as.numeric(layer_data$Layer)), ]
  layer_data <- as.data.frame(lapply(layer_data, as.numeric))

  return(list(name = soil_name, meta = meta, layers = layer_data))
}


# =============================================================================
# FUNCTION: read_scenarios
# Reads the scenario table that defines all model runs.
#
# The scenarios.csv file contains one row per scenario with all resolved
# parameters (location, management, soil, crop parameters) merged together.
#
# Args:
#   filepath - Path to scenarios.csv
#
# Returns:
#   A data.frame where each row is one scenario to simulate
# =============================================================================
read_scenarios <- function(filepath) {
  scn <- read.csv(filepath, stringsAsFactors = FALSE)
  return(scn)
}


# =============================================================================
# FUNCTION: read_soil_json
# Reads soil layer data from the consolidated JSON file (soil_data.json).
# This JSON contains all 10 soil profiles with full layer data.
#
# Args:
#   filepath - Path to soil_data.json
#
# Returns:
#   A named list indexed by SoilRowNo (as character), each containing
#   $name, $nlyer, $ldrain, $salb, $cn2, $layers (list of layer dicts)
# =============================================================================
read_soil_json <- function(filepath) {
  soil_json <- fromJSON(filepath, simplifyVector = FALSE)
  return(soil_json)
}


# =============================================================================
# FUNCTION: get_soil_layers_from_csv
# Reads the soil layer data for a specific location directly from CSV.
# Convenience wrapper used in the main model when running by location name.
#
# Args:
#   loc_name  - Location name (e.g., "Albany_MO", "Jonesboro_AR")
#   soil_dir  - Directory containing *_ssm_format.csv files
#
# Returns:
#   Named list with $meta and $layers
# =============================================================================
get_soil_layers_from_csv <- function(loc_name, soil_dir) {
  # Normalize: match name patterns like "Albany MO" → "Albany_MO"
  loc_clean <- gsub(" ", "_", loc_name)
  # Try exact match first
  csvfile <- file.path(soil_dir, paste0(loc_clean, "_ssm_format.csv"))
  if (!file.exists(csvfile)) {
    # Try case-insensitive search
    all_files <- list.files(soil_dir, pattern = "_ssm_format.csv", full.names = TRUE)
    matches <- all_files[grepl(loc_clean, basename(all_files), ignore.case = TRUE)]
    if (length(matches) == 0) stop(paste("No soil file found for:", loc_name))
    csvfile <- matches[1]
  }
  read_soil(csvfile)
}


# =============================================================================
# FUNCTION: build_soil_from_json_entry
# Converts a JSON soil entry (from soil_data.json) into the format expected
# by the soil water sub-model. Called by the main model loop.
#
# Args:
#   json_entry - One element from the soil_data.json list
#
# Returns:
#   Named list with $meta and $layers (data.frame)
# =============================================================================
build_soil_from_json_entry <- function(json_entry) {
  meta <- list(
    NLYER  = as.numeric(json_entry$nlyer),
    LDRAIN = as.numeric(json_entry$ldrain),
    SALB   = as.numeric(json_entry$salb),
    CN2    = as.numeric(json_entry$cn2)
  )

  layers <- do.call(rbind, lapply(json_entry$layers, function(l) {
    data.frame(
      Layer  = as.numeric(l$layer),
      DLYER  = as.numeric(l$dlyer),
      SAT    = as.numeric(l$sat),
      DUL    = as.numeric(l$dul),
      LL     = as.numeric(l$ll),
      ADRY   = as.numeric(l$adry),
      iWL    = as.numeric(l$iwl),
      DRAINF = as.numeric(l$drainf),
      FG     = as.numeric(l$fg),
      BDL    = as.numeric(l$bdl),
      NORG   = as.numeric(l$norg),
      FMIN   = as.numeric(l$fmin),
      iNSOL  = as.numeric(l$insol)
    )
  }))

  list(name = json_entry$soil_name %||% "Unknown", meta = meta, layers = layers)
}

# Helper: null-coalescing operator
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && a != "NULL") a else b
