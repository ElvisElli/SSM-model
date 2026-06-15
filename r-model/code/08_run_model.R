# =============================================================================
# SSM Soybean Model - Batch Run Script
# =============================================================================
# Runs the SSM model for all 10 locations × 24 scenarios × 30 years.
# Reads inputs from the r-model/inputs/ directory and writes results to
# r-model/outputs/results/.
#
# Usage:
#   Rscript 08_run_model.R
#
# Or from R console:
#   source("08_run_model.R")
#   results <- run_all_scenarios()
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(jsonlite)
})

# --- Set paths relative to the r-model root --------------------------------
# Adjust BASE_DIR if running from a different location
BASE_DIR    <- normalizePath(file.path(dirname(sys.frame(1)$ofile), ".."),
                              mustWork = FALSE)
if (!dir.exists(BASE_DIR)) {
  # Fallback: guess from current working directory
  BASE_DIR <- normalizePath(".", mustWork = FALSE)
  if (!file.exists(file.path(BASE_DIR, "inputs/scenarios.csv"))) {
    # Try one level up (if cwd is /code)
    BASE_DIR <- normalizePath("..", mustWork = FALSE)
  }
}

INPUT_DIR   <- file.path(BASE_DIR, "inputs")
CODE_DIR    <- file.path(BASE_DIR, "code")
OUTPUT_DIR  <- file.path(BASE_DIR, "outputs", "results")
WEATHER_DIR <- file.path(INPUT_DIR, "weather")
SOIL_DIR    <- file.path(INPUT_DIR, "soil")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Source all sub-models
for (f in c("01_read_inputs.R", "02_phenology.R", "03_crop_lai.R",
            "04_dm_production.R", "05_dm_distribution.R",
            "06_soil_water.R", "07_ssm_model.R")) {
  source(file.path(CODE_DIR, f))
}

cat("SSM Soybean Model - Starting batch run\n")
cat("Base directory:", BASE_DIR, "\n")
cat("Output directory:", OUTPUT_DIR, "\n\n")


# =============================================================================
# FUNCTION: run_all_scenarios
# Loops through all scenarios in scenarios.csv, runs each simulation year,
# and saves results to CSV files (one per location).
#
# Returns:
#   data.frame of all yearly summary results
# =============================================================================
run_all_scenarios <- function(scenarios_file = NULL, save_daily = FALSE) {
  if (is.null(scenarios_file)) {
    scenarios_file <- file.path(INPUT_DIR, "scenarios.csv")
  }

  scn_table <- read_scenarios(scenarios_file)
  cat(sprintf("Loaded %d scenarios\n", nrow(scn_table)))

  # Load soil data (pre-load all 10 profiles from JSON)
  soil_json <- read_soil_json(file.path(INPUT_DIR, "soil_data.json"))

  # Map soil_row numbers to their JSON keys (soil_row is the row index in Excel)
  # The JSON keys are "5", "23", "41", "59", "77", "95", "113", "131", "149", "167"
  unique_soil_rows <- unique(scn_table$soil_row)

  # Location-to-soil-file mapping for CSV fallback
  # (loc name → ssm_format CSV file)
  loc_soil_map <- c(
    "Jonesboro"    = "Jonesboro_AR_ssm_format.csv",
    "Marianna"     = "Marianna_AR_ssm_format.csv",
    "Keiser"       = "Keiser_AR_ssm_format.csv",
    "Rohwer"       = "Rowher_AR_ssm_format.csv",
    "Eustis"       = "Eustis_NE_ssm_format.csv",
    "Lincoln"      = "Lincoln_NE_ssm_format.csv",
    "North Platte" = "NorthPlatte_NE_ssm_format.csv",
    "Albany"       = "Albany_MO_ssm_format.csv",
    "Mount Vernon" = "MountVernon_MO_ssm_format.csv",
    "Novelty"      = "Novelty_MO_ssm_format.csv"
  )

  # Pre-load all weather files (cache in memory for speed)
  cat("Loading weather files...\n")
  weather_cache <- list()

  all_results <- list()
  result_idx  <- 0

  # Group scenarios by location (to load weather once per location)
  unique_locs <- unique(scn_table$loc_name)
  cat(sprintf("Found %d unique locations\n\n", length(unique_locs)))

  for (loc in unique_locs) {
    loc_scns <- scn_table[scn_table$loc_name == loc, ]
    wth_file <- loc_scns$wth_file[1]
    wth_path <- file.path(WEATHER_DIR, wth_file)

    # Load weather (or use cache)
    if (!wth_file %in% names(weather_cache)) {
      cat(sprintf("  Loading weather: %s\n", wth_file))
      if (!file.exists(wth_path)) {
        warning(sprintf("Weather file not found: %s — skipping location %s", wth_path, loc))
        next
      }
      weather_cache[[wth_file]] <- read_weather(wth_path)
    }
    wth_data <- weather_cache[[wth_file]]

    # Load soil for this location
    soil_row_key <- as.character(loc_scns$soil_row[1])
    if (soil_row_key %in% names(soil_json)) {
      soil <- build_soil_from_json_entry(soil_json[[soil_row_key]])
    } else {
      # Fallback to CSV
      soil_csv <- loc_soil_map[loc]
      if (!is.na(soil_csv)) {
        soil <- read_soil(file.path(SOIL_DIR, soil_csv))
      } else {
        warning(sprintf("No soil data for location: %s", loc))
        next
      }
    }

    cat(sprintf("Running %d scenarios for %s...\n", nrow(loc_scns), loc))

    loc_results <- list()

    for (si in seq_len(nrow(loc_scns))) {
      scn <- as.list(loc_scns[si, ])

      # Determine years to simulate
      start_year <- as.integer(scn$fyear)
      n_years    <- as.integer(scn$yrno)
      years      <- seq(start_year, start_year + n_years - 1)

      for (yr in years) {
        # Check if this year exists in weather data
        if (!yr %in% wth_data$YEAR) next

        result <- tryCatch(
          run_ssm_year(scn, wth_data, soil, yr, verbose = save_daily),
          error = function(e) {
            message(sprintf("    ERROR: scenario=%s, year=%d: %s",
                            scn$scenario, yr, e$message))
            NULL
          }
        )

        if (!is.null(result)) {
          result_idx <- result_idx + 1
          loc_results[[result_idx]] <- result$summary
        }
      }
    }

    if (length(loc_results) > 0) {
      loc_df <- do.call(rbind, loc_results)
      # Save per-location results
      out_file <- file.path(OUTPUT_DIR, paste0(gsub(" ", "_", loc), "_results.csv"))
      write.csv(loc_df, out_file, row.names = FALSE)
      cat(sprintf("  Saved %d rows to %s\n", nrow(loc_df), basename(out_file)))
      all_results[[loc]] <- loc_df
    }
  }

  # Combine all results into one file
  if (length(all_results) > 0) {
    all_df <- do.call(rbind, all_results)
    all_file <- file.path(OUTPUT_DIR, "all_results.csv")
    write.csv(all_df, all_file, row.names = FALSE)
    cat(sprintf("\nAll results saved: %d rows to %s\n", nrow(all_df), all_file))
    return(all_df)
  }

  cat("No results generated.\n")
  return(data.frame())
}

# Run if called directly (not when sourced from another script)
if (!interactive()) {
  t_start <- proc.time()
  results  <- run_all_scenarios()
  t_end    <- proc.time()
  cat(sprintf("\nTotal run time: %.1f seconds\n", (t_end - t_start)["elapsed"]))
}
