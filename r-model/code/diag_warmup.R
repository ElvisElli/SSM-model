#!/usr/bin/env Rscript
# Verify warm-up year carry-over gives correct stage-1/stage-2 on sowing day
# for KS-RFD-LTE-check 1990 (expect stage-2: SEVP/PET ~0.414) and
# LN-RFD-LTE-check 1990 (expect stage-1: SEVP/PET ~1.000)

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

run_with_warmup <- function(scenario_name, year, wth_file_name, soil_key) {
  scn <- as.list(scn_table[scn_table$scenario == scenario_name, ])
  wth <- read_weather(file.path(WEATHER_DIR, wth_file_name))
  soil <- build_soil_from_json_entry(soil_json[[soil_key]])

  warmup_yr <- year - 1L
  prev_ftswrz <- 0
  if (warmup_yr %in% wth$YEAR) {
    wu <- run_ssm_year(scn, wth, soil, warmup_yr, verbose = FALSE, init_ftswrz = 0)
    prev_ftswrz <- wu$final_ftswrz
    cat(sprintf("  Warm-up year %d: final_ftswrz=%.4f final_ftswrz_shallow=%.4f WGRN=%.2f\n",
                warmup_yr, wu$final_ftswrz, wu$final_ftswrz_shallow, wu$summary$WGRN))
  }

  res <- run_ssm_year(scn, wth, soil, year, verbose = TRUE, init_ftswrz = prev_ftswrz)
  list(res = res, prev_ftswrz = prev_ftswrz)
}

cat("=== KS-RFD-LTE-check 1990 (expect stage-2, ratio~0.414) ===\n")
ks <- run_with_warmup("KS-RFD-LTE-check", 1990, "SSM_Keiser_AR.xlsx", "41")
cat("init_ftswrz used:", ks$prev_ftswrz, "\n")
cat("IPASW:", ks$res$summary$IPASW, " (Excel: 215.36)\n")
cat("WGRN:", ks$res$summary$WGRN, " (Excel: 166.28)\n")
d <- ks$res$daily
cat("First day (Pdoy+1):\n")
print(d[1, c("doy","SEVP","PET","FTSWRZ","CE")])
cat(sprintf("Implied sowing-day SEVP = CE_day1 - SEVP_day1 = %.4f (Excel sowing day SEVP=2.767, PET=6.679, ratio=0.414)\n",
            d$CE[1] - d$SEVP[1]))

cat("\n=== LN-RFD-LTE-check 1990 (expect stage-1, ratio~1.0) ===\n")
ln <- run_with_warmup("LN-RFD-LTE-check", 1990, "SSM_Lincoln_NE.xlsx", "95")
cat("init_ftswrz used:", ln$prev_ftswrz, "\n")
cat("IPASW:", ln$res$summary$IPASW, "\n")
cat("WGRN:", ln$res$summary$WGRN, "\n")
d2 <- ln$res$daily
cat("First day (Pdoy+1):\n")
print(d2[1, c("doy","SEVP","PET","FTSWRZ","CE")])
cat(sprintf("Implied sowing-day SEVP = CE_day1 - SEVP_day1 = %.4f (Excel sowing day SEVP=6.845=PET, ratio=1.0)\n",
            d2$CE[1] - d2$SEVP[1]))
