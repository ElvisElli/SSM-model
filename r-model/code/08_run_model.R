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
# Supports: Rscript 08_run_model.R, source("..."), RStudio R project, or
# setting BASE_DIR externally before sourcing.
if (!exists("BASE_DIR") || !dir.exists(BASE_DIR)) {
  BASE_DIR <- tryCatch({
    # Walk call stack looking for an $ofile entry (set when file is sourced)
    script_path <- NULL
    for (i in seq_len(sys.nframe())) {
      ofile <- sys.frame(i)$ofile
      if (!is.null(ofile) && nchar(ofile) > 0) {
        script_path <- normalizePath(ofile, mustWork = FALSE)
        break
      }
    }
    if (!is.null(script_path)) {
      d <- dirname(script_path)
      if (basename(d) == "code") dirname(d) else d   # r-model/code → r-model
    } else {
      # Rscript without $ofile: try working directory heuristics
      cwd <- getwd()
      if (file.exists(file.path(cwd, "inputs/scenarios.csv"))) {
        cwd
      } else if (file.exists(file.path(cwd, "r-model/inputs/scenarios.csv"))) {
        file.path(cwd, "r-model")
      } else if (file.exists(file.path(cwd, "../inputs/scenarios.csv"))) {
        normalizePath(file.path(cwd, ".."), mustWork = FALSE)
      } else {
        stop("Cannot find r-model base directory. Open SSM-soybean.Rproj or set BASE_DIR.")
      }
    }
  }, error = function(e) stop(e$message))
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
# Loops through all scenarios in scenarios.csv in file order, runs each
# simulation year, and saves results to CSV files (one per location).
#
# VBA carry-over behaviour (critical for correctness):
#   FTSWRZ is a module-level Double in VBA that is never explicitly reset
#   between scenarios. It retains its value from the last simulated day of
#   the previous scenario-year and is used on the first day of the next
#   scenario-year to determine stage-1 vs stage-2 soil evaporation.
#   We replicate this by processing scenarios strictly in CSV row order and
#   threading a single `global_ftswrz` scalar through every scenario-year.
#   The value starts at 0 (VBA Double default) before the very first row.
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

  # Pre-load soil data (all profiles from JSON)
  soil_json <- read_soil_json(file.path(INPUT_DIR, "soil_data.json"))

  # Cache weather files by filename
  weather_cache <- list()
  # Cache built soil objects by soil_row key
  soil_cache    <- list()

  all_rows   <- list()   # collects every yearly summary row in run order
  result_idx <- 0

  # Single global FTSWRZ that flows across ALL scenario-years in CSV row order,
  # matching the un-reset VBA module-level variable. Starts at 0 (Double default).
  global_ftswrz <- 0

  cat(sprintf("Processing %d scenarios in CSV row order...\n\n", nrow(scn_table)))

  for (i in seq_len(nrow(scn_table))) {
    scn <- as.list(scn_table[i, ])
    loc <- scn$loc_name

    # --- Weather (cached) ---
    wth_file <- scn$wth_file
    if (!wth_file %in% names(weather_cache)) {
      wth_path <- file.path(WEATHER_DIR, wth_file)
      if (!file.exists(wth_path)) {
        warning(sprintf("Weather file not found: %s — skipping scenario %s",
                        wth_path, scn$scenario))
        next
      }
      cat(sprintf("  Loading weather: %s\n", wth_file))
      weather_cache[[wth_file]] <- read_weather(wth_path)
    }
    wth_data <- weather_cache[[wth_file]]

    # --- Soil (cached by soil_row key) ---
    soil_key <- as.character(scn$soil_row)
    if (!soil_key %in% names(soil_cache)) {
      if (soil_key %in% names(soil_json)) {
        soil_cache[[soil_key]] <- build_soil_from_json_entry(soil_json[[soil_key]])
      } else {
        warning(sprintf("No soil data for soil_row=%s (scenario %s)", soil_key, scn$scenario))
        next
      }
    }
    soil <- soil_cache[[soil_key]]

    # --- Year loop ---
    start_year <- as.integer(scn$fyear)
    n_years    <- as.integer(scn$yrno)

    for (yr in seq(start_year, start_year + n_years - 1)) {
      if (!yr %in% wth_data$YEAR) next

      result <- tryCatch(
        run_ssm_year(scn, wth_data, soil, yr, verbose = save_daily,
                     init_ftswrz = global_ftswrz),
        error = function(e) {
          message(sprintf("    ERROR: scenario=%s, year=%d: %s",
                          scn$scenario, yr, e$message))
          NULL
        }
      )

      if (!is.null(result)) {
        result_idx         <- result_idx + 1
        all_rows[[result_idx]] <- result$summary
        global_ftswrz      <- result$final_ftswrz
      }
    }
  }

  if (result_idx == 0) {
    cat("No results generated.\n")
    return(data.frame())
  }

  all_df <- do.call(rbind, all_rows)

  # Save per-location CSVs and one combined file
  for (loc in unique(all_df$loc_name)) {
    loc_df   <- all_df[all_df$loc_name == loc, ]
    out_file <- file.path(OUTPUT_DIR, paste0(gsub(" ", "_", loc), "_results.csv"))
    write.csv(loc_df, out_file, row.names = FALSE)
    cat(sprintf("  Saved %d rows to %s\n", nrow(loc_df), basename(out_file)))
  }

  all_file <- file.path(OUTPUT_DIR, "all_results.csv")
  write.csv(all_df, all_file, row.names = FALSE)
  cat(sprintf("\nAll results saved: %d rows to %s\n", nrow(all_df), all_file))
  return(all_df)
}

# Run if called directly (not when sourced from another script)
if (!interactive()) {
  t_start <- proc.time()
  results  <- run_all_scenarios()
  t_end    <- proc.time()
  cat(sprintf("\nTotal run time: %.1f seconds\n", (t_end - t_start)["elapsed"]))
}
