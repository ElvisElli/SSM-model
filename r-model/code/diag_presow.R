#!/usr/bin/env Rscript
# Quick diagnostic: check sowing-day state after pre-sowing fix
# Runs KS-RFD-LTE-check/1990 and LN-RFD-LTE-check/1990 with verbose daily output
# to verify SEVP/PET ratio and IPASW.

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

run_one <- function(scenario_name, year, wth_file_name, soil_key) {
  scn <- as.list(scn_table[scn_table$scenario == scenario_name, ])
  wth <- read_weather(file.path(WEATHER_DIR, wth_file_name))
  soil <- build_soil_from_json_entry(soil_json[[soil_key]])
  res <- run_ssm_year(scn, wth, soil, year, verbose = TRUE)
  res
}

cat("=== KS-RFD-LTE-check 1990 ===\n")
ks <- run_one("KS-RFD-LTE-check", 1990, "SSM_Keiser_AR.xlsx", "41")
cat("IPASW (ISOLWAT):", ks$summary$IPASW, "\n")
cat("WGRN:", ks$summary$WGRN, "\n")
d <- ks$daily
sow_row <- d[d$doy == as.integer(scn_table$pdoy[scn_table$scenario=="KS-RFD-LTE-check"][1]), ]
if (nrow(sow_row) > 0) {
  cat("Sowing day DOY:", sow_row$doy, "SEVP:", sow_row$SEVP, "PET:", sow_row$PET,
      "ratio:", round(sow_row$SEVP/sow_row$PET, 4), "FTSWRZ:", sow_row$FTSWRZ, "\n")
}
# Show first 5 days
cat("First 5 days:\n")
print(d[1:min(5,nrow(d)), c("doy","SEVP","PET","FTSWRZ","CRAIN","CE","WSFG")])

cat("\n=== LN-RFD-LTE-check 1990 ===\n")
ln <- run_one("LN-RFD-LTE-check", 1990, "SSM_Lincoln_NE.xlsx", "95")
cat("IPASW (ISOLWAT):", ln$summary$IPASW, "\n")
cat("WGRN:", ln$summary$WGRN, "\n")
d2 <- ln$daily
sow_row2 <- d2[d2$doy == as.integer(scn_table$pdoy[scn_table$scenario=="LN-RFD-LTE-check"][1]), ]
if (nrow(sow_row2) > 0) {
  cat("Sowing day DOY:", sow_row2$doy, "SEVP:", sow_row2$SEVP, "PET:", sow_row2$PET,
      "ratio:", round(sow_row2$SEVP/sow_row2$PET, 4), "FTSWRZ:", sow_row2$FTSWRZ, "\n")
}
cat("First 5 days:\n")
print(d2[1:min(5,nrow(d2)), c("doy","SEVP","PET","FTSWRZ","CRAIN","CE","WSFG")])
