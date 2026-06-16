#!/usr/bin/env Rscript
# Test hypothesis: FTSWRZ is a single global scalar that carries over
# sequentially across ALL scenario-years in scenarios.csv row order
# (matching VBA's un-reset module-level FTSWRZ variable), starting at 0
# before the very first scenario/year of the whole run.
#
# Verify against:
#   KS-RFD-LTE-check 1990: expect stage-2 (carried-in FTSWRZ < 0.5)
#   LN-RFD-LTE-check 1990: expect stage-1 (carried-in FTSWRZ >= 0.5)

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(jsonlite)
})

BASE_DIR <- "/home/user/SSM-model/r-model"
INPUT_DIR   <- file.path(BASE_DIR, "inputs")
CODE_DIR    <- file.path(BASE_DIR, "code")
WEATHER_DIR <- file.path(INPUT_DIR, "weather")

for (f in c("01_read_inputs.R","02_phenology.R","03_crop_lai.R",
            "04_dm_production.R","05_dm_distribution.R",
            "06_soil_water.R","07_ssm_model.R")) {
  source(file.path(CODE_DIR, f))
}

soil_json <- read_soil_json(file.path(INPUT_DIR, "soil_data.json"))
scn_table <- read_scenarios(file.path(INPUT_DIR, "scenarios.csv"))

cat(sprintf("Total scenarios: %d\n", nrow(scn_table)))

target_names <- c("KS-RFD-LTE-check", "LN-RFD-LTE-check")
target_rows  <- which(scn_table$scenario %in% target_names)
cat("Target rows:", paste(target_rows, collapse=", "), "\n")
max_row <- max(target_rows)

weather_cache <- list()
ftswrz <- 0  # VBA Double default at start of entire run

t0 <- proc.time()
for (i in seq_len(max_row)) {
  scn <- as.list(scn_table[i, ])
  wth_file <- scn$wth_file
  if (!wth_file %in% names(weather_cache)) {
    weather_cache[[wth_file]] <- read_weather(file.path(WEATHER_DIR, wth_file))
  }
  wth <- weather_cache[[wth_file]]
  soil_key <- as.character(scn$soil_row)
  soil <- build_soil_from_json_entry(soil_json[[soil_key]])

  start_year <- as.integer(scn$fyear)
  n_years    <- as.integer(scn$yrno)
  years      <- seq(start_year, start_year + n_years - 1)

  if (i %in% target_rows) {
    cat(sprintf("\n=== Row %d: %s ===\n", i, scn$scenario))
    cat(sprintf("Carried-in FTSWRZ at start of first year (%d): %.4f -> %s\n",
                start_year, ftswrz, if (ftswrz < 0.5) "STAGE-2 (expected ratio~0.414)" else "STAGE-1 (expected ratio~1.0)"))
  }

  for (yr in years) {
    if (!yr %in% wth$YEAR) next
    res <- tryCatch(
      run_ssm_year(scn, wth, soil, yr, verbose = (i %in% target_rows && yr == start_year),
                   init_ftswrz = ftswrz),
      error = function(e) { message(sprintf("ERROR row %d yr %d: %s", i, yr, e$message)); NULL }
    )
    if (is.null(res)) next
    if (i %in% target_rows && yr == start_year) {
      d <- res$daily
      cat("First day (Pdoy+1):\n")
      print(d[1, c("doy","SEVP","PET","FTSWRZ","CE")])
      cat(sprintf("Implied sowing-day SEVP = %.4f, PET-implied ratio\n", d$CE[1] - d$SEVP[1]))
      cat("IPASW:", res$summary$IPASW, " WGRN:", res$summary$WGRN, "\n")
    }
    ftswrz <- res$final_ftswrz
  }
}
t1 <- proc.time()
cat(sprintf("\nDone. Elapsed: %.1f s\n", (t1-t0)["elapsed"]))
