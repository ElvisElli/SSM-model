# =============================================================================
# SSM Soybean Model — Model Testing Runner
# Runs all test scenarios defined in test_scenarios.csv and saves results to
# model-testing/ssm_testing/outputs/results/
# =============================================================================
#
# HOW TO RUN ON YOUR COMPUTER:
# 1. Open SSM-model.Rproj in RStudio (or set working directory to the repo root,
#    e.g., setwd("C:/path/to/SSM-model"))
# 2. Install packages if needed:
#    install.packages(c("readxl", "dplyr", "jsonlite"))
# 3. source("model-testing/ssm_testing/run_model_testing.R")
# 4. Results saved to: model-testing/ssm_testing/outputs/results/
#
# Optional: to save daily outputs (larger files, needed for in-season plots):
#    SAVE_DAILY <- TRUE   (change the flag below, then re-source)
#
# Optional: parallel mode (faster on multi-core machines):
#    USE_PARALLEL <- TRUE  (change the flag below, then re-source)
# =============================================================================

# ---- USER-CONFIGURABLE FLAGS (change these before sourcing) -----------------
SAVE_DAILY   <- TRUE   # Save per-day simulation outputs (needed for biomass plots)
USE_PARALLEL <- FALSE  # Use multiple CPU cores? (set TRUE to speed up on local machine)
N_CORES      <- NULL   # Number of cores (NULL = auto-detect physical cores - 1)
# -----------------------------------------------------------------------------

# --- Auto-install missing packages -------------------------------------------
local({
  needed <- c("readxl", "dplyr", "jsonlite")
  miss   <- needed[!sapply(needed, requireNamespace, quietly = TRUE)]
  if (length(miss) > 0) {
    message("Auto-installing missing packages: ", paste(miss, collapse = ", "))
    install.packages(miss, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
})

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(jsonlite)
})

# --- Locate the repo root and key directories --------------------------------
#
# Strategy:
#   1. Try to detect from this script's own path (works when sourced in RStudio
#      or via Rscript path/to/run_model_testing.R)
#   2. Fall back to walking up from getwd() looking for r-model/inputs/scenarios.csv
#
if (!exists("REPO_ROOT") || !dir.exists(REPO_ROOT)) {
  REPO_ROOT <- tryCatch({
    script_path <- NULL
    for (i in seq_len(sys.nframe())) {
      ofile <- sys.frame(i)$ofile
      if (!is.null(ofile) && nchar(ofile) > 0) {
        script_path <- normalizePath(ofile, mustWork = FALSE)
        break
      }
    }

    if (!is.null(script_path)) {
      # Script is at <repo>/model-testing/ssm_testing/run_model_testing.R
      # So repo root is 2 levels up from the script directory
      candidate <- normalizePath(file.path(dirname(script_path), "..", ".."),
                                 mustWork = FALSE)
      if (file.exists(file.path(candidate, "r-model", "inputs", "scenarios.csv"))) {
        candidate
      } else {
        stop("Could not find r-model/inputs/scenarios.csv relative to script location.")
      }
    } else {
      # No script path — try walking up from working directory
      cwd <- getwd()
      if (file.exists(file.path(cwd, "r-model", "inputs", "scenarios.csv"))) {
        cwd
      } else if (file.exists(file.path(cwd, "..", "r-model", "inputs", "scenarios.csv"))) {
        normalizePath(file.path(cwd, ".."), mustWork = FALSE)
      } else {
        stop(paste(
          "Cannot locate repo root. Please either:\n",
          "  a) Open SSM-model.Rproj in RStudio (sets working directory automatically), or\n",
          "  b) Set REPO_ROOT manually before sourcing:\n",
          "     REPO_ROOT <- 'C:/path/to/SSM-model'\n",
          "     source('model-testing/ssm_testing/run_model_testing.R')"
        ))
      }
    }
  }, error = function(e) stop(e$message))
}

# Derived paths
RMODEL_DIR   <- file.path(REPO_ROOT, "r-model")
CODE_DIR     <- file.path(RMODEL_DIR, "code")
INPUT_DIR    <- file.path(RMODEL_DIR, "inputs")
WEATHER_DIR  <- file.path(INPUT_DIR, "weather")
SOIL_FILE    <- file.path(INPUT_DIR, "soil_data.json")

TESTING_DIR  <- file.path(REPO_ROOT, "model-testing", "ssm_testing")
SCENARIOS_FILE <- file.path(TESTING_DIR, "inputs", "test_scenarios.csv")
# Also check the old location for backward compatibility
if (!file.exists(SCENARIOS_FILE)) {
  SCENARIOS_FILE <- file.path(TESTING_DIR, "test_scenarios.csv")
}

OUTPUT_DIR   <- file.path(TESTING_DIR, "outputs", "results")
DAILY_DIR    <- file.path(OUTPUT_DIR, "daily")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
if (SAVE_DAILY) dir.create(DAILY_DIR, recursive = TRUE, showWarnings = FALSE)

cat("=============================================================\n")
cat("SSM Soybean Model — Model Testing\n")
cat("=============================================================\n")
cat("Repo root    :", REPO_ROOT, "\n")
cat("Scenarios    :", SCENARIOS_FILE, "\n")
cat("Output dir   :", OUTPUT_DIR, "\n")
cat("Save daily   :", SAVE_DAILY, "\n")
cat("Parallel     :", USE_PARALLEL, "\n\n")

# --- Source all model sub-modules --------------------------------------------
#
# The SSM model is split across 7 files:
#   01_read_inputs.R   – read_scenarios(), read_weather(), read_soil_json(),
#                        build_soil_from_json_entry()
#   02_phenology.R     – thermal-time phenology functions
#   03_crop_lai.R      – leaf area index
#   04_dm_production.R – dry matter production (RUE)
#   05_dm_distribution.R – assimilate partitioning
#   06_soil_water.R    – soil water balance
#   07_ssm_model.R     – top-level run_ssm_year() integrator
#
cat("Sourcing model sub-modules...\n")
for (.f in c("01_read_inputs.R", "02_phenology.R", "03_crop_lai.R",
             "04_dm_production.R", "05_dm_distribution.R",
             "06_soil_water.R", "07_ssm_model.R")) {
  source(file.path(CODE_DIR, .f))
}
cat("  All 7 sub-modules loaded.\n\n")

# --- Define run_one_scenario (mirrors 08_run_model.R) ------------------------
#
# Runs all simulation years for a single scenario row from test_scenarios.csv.
# FTSWRZ (fractional transpirable soil water) carries over year-to-year within
# a scenario (continuous field), but resets to 1.0 at scenario start.
#
run_one_scenario <- function(scn, wth_data, soil,
                             save_daily  = FALSE,
                             init_ftswrz = 1.0) {
  start_year   <- as.integer(scn$fyear)
  n_years      <- as.integer(scn$yrno)
  local_ftswrz <- init_ftswrz
  yearly_rows  <- list()
  daily_rows   <- list()
  yr_idx       <- 0L
  dy_idx       <- 0L

  for (yr in seq(start_year, start_year + n_years - 1L)) {
    if (!yr %in% wth_data$YEAR) {
      message(sprintf("  Warning: year %d not in weather for scenario %s — skipped",
                      yr, scn$scenario))
      next
    }

    result <- tryCatch(
      run_ssm_year(scn, wth_data, soil, yr,
                   verbose     = save_daily,
                   init_ftswrz = local_ftswrz),
      error = function(e) {
        message(sprintf("  ERROR scenario=%s year=%d: %s",
                        scn$scenario, yr, e$message))
        NULL
      }
    )

    if (!is.null(result)) {
      yr_idx                <- yr_idx + 1L
      yearly_rows[[yr_idx]] <- result$summary
      local_ftswrz          <- result$final_ftswrz

      if (save_daily && !is.null(result$daily) && nrow(result$daily) > 0) {
        d       <- result$daily
        d$sName <- scn$scenario
        d$year  <- yr
        dy_idx               <- dy_idx + 1L
        daily_rows[[dy_idx]] <- d
      }
    }
  }

  yearly_out <- if (yr_idx > 0L) do.call(rbind, yearly_rows) else data.frame()
  daily_out  <- if (dy_idx > 0L) do.call(rbind, daily_rows)  else NULL

  list(data         = yearly_out,
       daily        = daily_out,
       final_ftswrz = local_ftswrz)
}

# --- Load scenarios -----------------------------------------------------------
if (!file.exists(SCENARIOS_FILE)) {
  stop(paste("Scenarios file not found:", SCENARIOS_FILE,
             "\nExpected at: model-testing/ssm_testing/inputs/test_scenarios.csv"))
}

scn_table <- read_scenarios(SCENARIOS_FILE)
cat(sprintf("Loaded %d test scenarios from %s\n\n",
            nrow(scn_table), basename(SCENARIOS_FILE)))

# --- Pre-load and cache weather and soil data --------------------------------
#
# Each unique weather file is loaded once and stored in weather_cache.
# Each unique soil_row is looked up once from soil_data.json.
# This avoids re-reading the same Excel weather file for every scenario.
#
cat("Pre-loading weather and soil data...\n")
weather_cache <- list()
soil_json     <- read_soil_json(SOIL_FILE)
soil_cache    <- list()

for (i in seq_len(nrow(scn_table))) {
  scn      <- as.list(scn_table[i, ])
  wth_file <- scn$wth_file
  soil_key <- as.character(scn$soil_row)

  if (!wth_file %in% names(weather_cache)) {
    wth_path <- file.path(WEATHER_DIR, wth_file)
    if (!file.exists(wth_path)) {
      warning(sprintf("Weather file not found: %s (scenario %s skipped)", wth_path, scn$scenario))
      next
    }
    cat(sprintf("  Loading weather: %s\n", wth_file))
    weather_cache[[wth_file]] <- read_weather(wth_path)
  }

  if (!soil_key %in% names(soil_cache)) {
    if (soil_key %in% names(soil_json)) {
      cat(sprintf("  Loading soil: %s\n", soil_key))
      soil_cache[[soil_key]] <- build_soil_from_json_entry(soil_json[[soil_key]])
    } else {
      warning(sprintf("No soil data for soil_row=%s (scenario %s)", soil_key, scn$scenario))
    }
  }
}
cat("\n")

# --- Build per-scenario work items -------------------------------------------
scn_items <- lapply(seq_len(nrow(scn_table)), function(i) {
  scn  <- as.list(scn_table[i, ])
  wth  <- weather_cache[[scn$wth_file]]
  soil <- soil_cache[[as.character(scn$soil_row)]]
  if (is.null(wth) || is.null(soil)) return(NULL)
  list(scn = scn, wth_data = wth, soil = soil, save_daily = SAVE_DAILY)
})
scn_items <- Filter(Negate(is.null), scn_items)
n_scn     <- length(scn_items)
cat(sprintf("Running %d scenarios...\n\n", n_scn))

# Worker function (same for serial and parallel)
worker_fn <- function(item) {
  run_one_scenario(item$scn, item$wth_data, item$soil,
                   save_daily  = item$save_daily,
                   init_ftswrz = 1.0)
}

# --- Run scenarios (serial or parallel) --------------------------------------
t0 <- proc.time()

if (USE_PARALLEL) {
  n_phys <- parallel::detectCores(logical = FALSE)
  if (is.null(N_CORES) || N_CORES < 1L)
    N_CORES <- max(1L, n_phys - 1L)
  N_CORES <- min(as.integer(N_CORES), n_scn)
  cat(sprintf("Parallel mode: %d cores (machine: %d physical)\n\n", N_CORES, n_phys))

  if (.Platform$OS.type == "windows") {
    cl <- parallel::makeCluster(N_CORES)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterExport(cl, "CODE_DIR", envir = environment())
    parallel::clusterEvalQ(cl, {
      suppressPackageStartupMessages({
        library(readxl); library(dplyr); library(jsonlite)
      })
      for (.f in c("01_read_inputs.R", "02_phenology.R", "03_crop_lai.R",
                   "04_dm_production.R", "05_dm_distribution.R",
                   "06_soil_water.R", "07_ssm_model.R")) {
        source(file.path(CODE_DIR, .f))
      }
    })
    parallel::clusterExport(cl, "run_one_scenario")
    results_list <- parallel::parLapply(cl, scn_items, worker_fn)
  } else {
    results_list <- parallel::mclapply(scn_items, worker_fn, mc.cores = N_CORES)
  }
} else {
  cat(sprintf("Serial mode: running %d scenarios one at a time...\n\n", n_scn))
  results_list <- vector("list", n_scn)
  for (k in seq_len(n_scn)) {
    cat(sprintf("  [%d/%d] %s\n", k, n_scn, scn_items[[k]]$scn$scenario))
    results_list[[k]] <- worker_fn(scn_items[[k]])
  }
}

elapsed <- (proc.time() - t0)["elapsed"]
cat(sprintf("\nRun time: %.1f seconds (%.1f min)\n\n", elapsed, elapsed / 60))

# --- Combine and save yearly results -----------------------------------------
yearly_data <- lapply(results_list, `[[`, "data")
non_empty   <- Filter(function(d) !is.null(d) && nrow(d) > 0, yearly_data)

if (length(non_empty) == 0) {
  stop("No results were generated. Check weather file paths and scenario settings.")
}

all_df <- do.call(rbind, non_empty)

# Save combined file (this is what 03_analyze_results.R reads)
out_yearly <- file.path(OUTPUT_DIR, "test_results_yearly.csv")
write.csv(all_df, out_yearly, row.names = FALSE)
cat(sprintf("Yearly results: %d rows saved to\n  %s\n\n", nrow(all_df), out_yearly))

# --- Save daily CSVs (one per scenario) --------------------------------------
if (SAVE_DAILY) {
  n_daily_saved <- 0L
  for (k in seq_len(length(results_list))) {
    d <- results_list[[k]]$daily
    if (!is.null(d) && nrow(d) > 0) {
      sname    <- scn_items[[k]]$scn$scenario
      out_file <- file.path(DAILY_DIR, paste0(sname, "_daily.csv"))
      id_cols  <- intersect(c("sName", "year", "doy", "DAP"), names(d))
      val_cols <- setdiff(names(d), id_cols)
      write.csv(d[, c(id_cols, val_cols)], out_file, row.names = FALSE)
      n_daily_saved <- n_daily_saved + 1L
    }
  }
  cat(sprintf("Daily outputs: %d files saved to\n  %s/\n\n", n_daily_saved, DAILY_DIR))
}

# --- Summary table -----------------------------------------------------------
cat("=============================================================\n")
cat("SUMMARY TABLE — Mean simulated yield by site × year × treatment\n")
cat("=============================================================\n")

summary_tbl <- all_df %>%
  mutate(
    site      = sub("^TEST-([A-Za-z]+)-.*", "\\1", sName),
    Year      = as.integer(sub("^TEST-[A-Za-z]+-([0-9]+)-.*", "\\1", sName)),
    Treatment = ifelse(grepl("-IRRI-", sName), "Irrigated", "Rainfed")
  ) %>%
  group_by(site, Year, Treatment) %>%
  summarise(
    n_scenarios  = n(),
    mean_yield   = round(mean(Ywet,      na.rm = TRUE), 0),
    mean_biomass = round(mean(WTOP * 10, na.rm = TRUE), 0),
    mean_HI      = round(mean(HI,        na.rm = TRUE), 3),
    mean_R8_DAP  = round(mean(R8,        na.rm = TRUE), 0),
    .groups = "drop"
  )

print(as.data.frame(summary_tbl), row.names = FALSE)

cat("\n=============================================================\n")
cat("DONE. Next step: run 03_analyze_results.R to generate plots\n")
cat("  and compare simulated vs observed.\n")
cat("=============================================================\n")
