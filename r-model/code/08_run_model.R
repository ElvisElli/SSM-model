# =============================================================================
# SSM Soybean Model - Batch Run Script
# =============================================================================
# Runs the SSM model for all scenarios defined in scenarios.csv.
# Reads inputs from the r-model/inputs/ directory and writes results to
# r-model/outputs/results/.
#
# Usage (command line):
#   Rscript 08_run_model.R                     # serial (exact VBA match)
#   Rscript 08_run_model.R --parallel          # parallel, auto-detect cores
#   Rscript 08_run_model.R --parallel --cores 8
#
# Usage (R console / RStudio):
#   source("r-model/code/08_run_model.R")
#   results <- run_all_scenarios()
#   results <- run_all_scenarios(parallel = TRUE)
#   results <- run_all_scenarios(parallel = TRUE, n_cores = 8)
#
# IMPORTANT — serial vs. parallel:
#   Serial mode (default) threads a single FTSWRZ value across all scenarios
#   in CSV row order, exactly replicating the VBA carry-over behaviour.
#   Parallel mode runs each scenario independently (FTSWRZ starts at 0) for
#   speed; results differ slightly from the VBA reference at season boundaries.
# =============================================================================

# --- Auto-install any missing packages (silent if all present) ---------------
local({
  needed <- c("readxl", "dplyr", "jsonlite")  # 'parallel' is built into base R
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

# --- Portable path detection -------------------------------------------------
# Works from: Rscript, source(), RStudio project, or with BASE_DIR pre-set.
if (!exists("BASE_DIR") || !dir.exists(BASE_DIR)) {
  BASE_DIR <- tryCatch({
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
      if (basename(d) == "code") dirname(d) else d
    } else {
      cwd <- getwd()
      if      (file.exists(file.path(cwd, "inputs/scenarios.csv")))        cwd
      else if (file.exists(file.path(cwd, "r-model/inputs/scenarios.csv"))) file.path(cwd, "r-model")
      else if (file.exists(file.path(cwd, "../inputs/scenarios.csv")))      normalizePath(file.path(cwd, ".."), mustWork = FALSE)
      else stop("Cannot locate r-model base directory. Open SSM-soybean.Rproj or set BASE_DIR.")
    }
  }, error = function(e) stop(e$message))
}

INPUT_DIR   <- file.path(BASE_DIR, "inputs")
CODE_DIR    <- file.path(BASE_DIR, "code")
OUTPUT_DIR  <- file.path(BASE_DIR, "outputs", "results")
WEATHER_DIR <- file.path(INPUT_DIR, "weather")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Source all sub-models
for (.f in c("01_read_inputs.R", "02_phenology.R", "03_crop_lai.R",
             "04_dm_production.R", "05_dm_distribution.R",
             "06_soil_water.R", "07_ssm_model.R")) {
  source(file.path(CODE_DIR, .f))
}

cat("SSM Soybean Model\n")
cat("Base directory:", BASE_DIR, "\n")
cat("Output directory:", OUTPUT_DIR, "\n\n")


# =============================================================================
# HELPER: run_one_scenario
# Runs all simulation years for a single scenario.
# FTSWRZ carries over between years within the scenario (from init_ftswrz
# on the first year, then from each year's final value for the next).
#
# Args:
#   scn          - Named list (one row from scenarios.csv)
#   wth_data     - data.frame of weather for this location
#   soil         - Soil object from build_soil_from_json_entry
#   save_daily   - Collect daily time-step outputs? (logical)
#   init_ftswrz  - FTSWRZ carried in from the previous scenario (serial mode)
#                  or 0 (parallel mode / first scenario)
#
# Returns:
#   Named list:
#     $data         - data.frame of yearly summaries (one row per year)
#     $final_ftswrz - FTSWRZ value at end of last simulated day
# =============================================================================
run_one_scenario <- function(scn, wth_data, soil,
                             save_daily  = FALSE,
                             init_ftswrz = 0) {
  start_year   <- as.integer(scn$fyear)
  n_years      <- as.integer(scn$yrno)
  local_ftswrz <- init_ftswrz
  rows         <- list()
  idx          <- 0L

  for (yr in seq(start_year, start_year + n_years - 1L)) {
    if (!yr %in% wth_data$YEAR) next

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
      idx          <- idx + 1L
      rows[[idx]]  <- result$summary
      local_ftswrz <- result$final_ftswrz
    }
  }

  data_out <- if (idx > 0L) do.call(rbind, rows) else data.frame()
  list(data = data_out, final_ftswrz = local_ftswrz)
}


# =============================================================================
# FUNCTION: run_all_scenarios
# Reads scenarios.csv, runs every scenario, and writes CSV outputs.
#
# Args:
#   scenarios_file - Path to scenarios.csv (NULL → r-model/inputs/scenarios.csv)
#   save_daily     - Collect daily outputs? (logical, default FALSE)
#   parallel       - Run scenarios in parallel? (logical, default FALSE)
#   n_cores        - Cores to use in parallel mode (NULL → physical cores − 1)
#
# Returns:
#   data.frame of all yearly summary results
# =============================================================================
run_all_scenarios <- function(scenarios_file = NULL,
                              save_daily     = FALSE,
                              parallel       = FALSE,
                              n_cores        = NULL) {
  if (is.null(scenarios_file))
    scenarios_file <- file.path(INPUT_DIR, "scenarios.csv")

  scn_table <- read_scenarios(scenarios_file)
  cat(sprintf("Loaded %d scenarios\n", nrow(scn_table)))

  # --- Pre-load and cache weather and soil data ---
  weather_cache <- list()
  soil_json     <- read_soil_json(file.path(INPUT_DIR, "soil_data.json"))
  soil_cache    <- list()

  for (i in seq_len(nrow(scn_table))) {
    scn      <- as.list(scn_table[i, ])
    wth_file <- scn$wth_file
    soil_key <- as.character(scn$soil_row)

    if (!wth_file %in% names(weather_cache)) {
      wth_path <- file.path(WEATHER_DIR, wth_file)
      if (!file.exists(wth_path)) {
        warning(sprintf("Weather file not found: %s — scenario %s skipped",
                        wth_path, scn$scenario))
        next
      }
      cat(sprintf("  Loading weather: %s\n", wth_file))
      weather_cache[[wth_file]] <- read_weather(wth_path)
    }

    if (!soil_key %in% names(soil_cache)) {
      if (soil_key %in% names(soil_json)) {
        soil_cache[[soil_key]] <- build_soil_from_json_entry(soil_json[[soil_key]])
      } else {
        warning(sprintf("No soil data for soil_row=%s (scenario %s)", soil_key, scn$scenario))
      }
    }
  }

  # Build per-scenario work items (scn + resolved data objects)
  scn_items <- lapply(seq_len(nrow(scn_table)), function(i) {
    scn  <- as.list(scn_table[i, ])
    wth  <- weather_cache[[scn$wth_file]]
    soil <- soil_cache[[as.character(scn$soil_row)]]
    if (is.null(wth) || is.null(soil)) return(NULL)
    list(scn = scn, wth_data = wth, soil = soil)
  })
  scn_items <- Filter(Negate(is.null), scn_items)
  n_scn     <- length(scn_items)

  if (parallel) {
    # -------------------------------------------------------------------------
    # PARALLEL MODE — each scenario is independent (FTSWRZ starts at 0)
    # -------------------------------------------------------------------------
    n_phys <- parallel::detectCores(logical = FALSE)
    if (is.null(n_cores) || n_cores < 1L)
      n_cores <- max(1L, n_phys - 1L)
    n_cores <- min(as.integer(n_cores), n_scn)

    cat(sprintf("Parallel mode: %d scenarios on %d cores (machine: %d physical cores)\n\n",
                n_scn, n_cores, n_phys))

    worker_fn <- function(item) {
      run_one_scenario(item$scn, item$wth_data, item$soil,
                       save_daily  = save_daily,
                       init_ftswrz = 0)$data
    }

    if (.Platform$OS.type == "windows") {
      # Windows: fork not available; use PSOCK cluster and re-source files
      cl <- parallel::makeCluster(n_cores)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      parallel::clusterExport(cl, "CODE_DIR", envir = environment())
      parallel::clusterEvalQ(cl, {
        suppressPackageStartupMessages({
          library(readxl); library(dplyr); library(jsonlite)
        })
        for (.f in c("01_read_inputs.R","02_phenology.R","03_crop_lai.R",
                     "04_dm_production.R","05_dm_distribution.R",
                     "06_soil_water.R","07_ssm_model.R")) {
          source(file.path(CODE_DIR, .f))
        }
      })
      parallel::clusterExport(cl, "run_one_scenario")
      results_list <- parallel::parLapply(cl, scn_items, worker_fn)
    } else {
      # Unix / Linux / macOS: fork-based (workers inherit parent environment)
      results_list <- parallel::mclapply(scn_items, worker_fn, mc.cores = n_cores)
    }

  } else {
    # -------------------------------------------------------------------------
    # SERIAL MODE — exact VBA carry-over: FTSWRZ threads across all scenarios
    # -------------------------------------------------------------------------
    cat(sprintf("Serial mode: %d scenarios in CSV row order...\n\n", n_scn))
    global_ftswrz <- 0   # VBA module-level Double default
    results_list  <- vector("list", n_scn)

    for (k in seq_len(n_scn)) {
      item   <- scn_items[[k]]
      result <- run_one_scenario(item$scn, item$wth_data, item$soil,
                                 save_daily  = save_daily,
                                 init_ftswrz = global_ftswrz)
      results_list[[k]] <- result$data
      global_ftswrz     <- result$final_ftswrz
    }
  }

  # --- Combine and save ---
  non_empty <- Filter(function(d) !is.null(d) && nrow(d) > 0, results_list)
  if (length(non_empty) == 0) {
    cat("No results generated.\n")
    return(data.frame())
  }
  all_df <- do.call(rbind, non_empty)

  for (loc in unique(all_df$Location)) {
    loc_df   <- all_df[all_df$Location == loc, ]
    out_file <- file.path(OUTPUT_DIR, paste0(gsub(" ", "_", loc), "_results.csv"))
    write.csv(loc_df, out_file, row.names = FALSE)
    cat(sprintf("  Saved %d rows → %s\n", nrow(loc_df), basename(out_file)))
  }

  all_file <- file.path(OUTPUT_DIR, "all_results.csv")
  write.csv(all_df, all_file, row.names = FALSE)
  cat(sprintf("\nAll results saved: %d rows → %s\n", nrow(all_df), all_file))
  return(all_df)
}


# =============================================================================
# COMMAND-LINE ENTRY POINT
# Parses --parallel and optional --cores N when run via Rscript.
# =============================================================================
if (!interactive()) {
  args    <- commandArgs(trailingOnly = TRUE)
  do_par  <- "--parallel" %in% args
  ci      <- which(args == "--cores")
  n_cores <- if (length(ci) > 0 && ci[1] < length(args)) as.integer(args[ci[1] + 1]) else NULL

  if (do_par) {
    cat(sprintf("Parallel execution requested%s\n",
                if (!is.null(n_cores)) sprintf(" (%d cores)", n_cores) else " (auto)"))
  }

  t0      <- proc.time()
  results <- run_all_scenarios(parallel = do_par, n_cores = n_cores)
  elapsed <- (proc.time() - t0)["elapsed"]
  cat(sprintf("\nTotal run time: %.1f seconds\n", elapsed))
}
