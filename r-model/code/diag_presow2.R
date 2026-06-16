#!/usr/bin/env Rscript
# Trace pre-sowing soil water day by day for KS-RFD-LTE-check 1990

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

scn <- as.list(scn_table[scn_table$scenario == "KS-RFD-LTE-check", ])
wth <- read_weather(file.path(WEATHER_DIR, "SSM_Keiser_AR.xlsx"))
soil <- build_soil_from_json_entry(soil_json[["41"]])

year <- 1990
Pdoy <- as.integer(scn$pdoy)
cat("Pdoy:", Pdoy, "\n")

crop_pars <- as.list(scn)
crop_pars$pden <- as.numeric(scn$pden)
crop_pars$lat  <- as.numeric(scn$lat)
bd_thres <- init_phenology_legume(crop_pars)
sw <- init_soil_water(soil, crop_pars, CO2=as.numeric(scn$co2), CO2REF=as.numeric(scn$CO2REF))

cat("Initial ATSWSL (DUL):", sw$ATSWSL, "\n")
cat("Initial DEPORT:", sw$DEPORT, "\n\n")

wth_year <- wth[wth$YEAR == year, ]
wrow <- which(wth_year$DOY == 1)[1]
SNOW <- 0
water <- as.integer(scn$water)
VPDF <- as.numeric(scn$vpdf)
PDEN <- as.numeric(scn$pden)

results <- data.frame()
while (wrow <= nrow(wth_year) && wth_year$DOY[wrow] < Pdoy) {
  w <- get_weather_row(wth_year, wrow, 0, 1, SNOW)
  SNOW <- w$SNOW
  state_pre <- list(CBD=0, DAP=0, bd=0, DDMP=0, TR=0, LAI=0, BLSLAI=0,
                    VPDF=VPDF, WSFL=1, WSFG=1, PDEN=PDEN)
  sw <- step_soil_water(sw, state_pre, bd_thres, w, water, as.numeric(scn$irglvl))
  results <- rbind(results, data.frame(doy=w$doy, RAIN=w$RAIN, SEVP=sw$SEVP, PET=sw$PET,
                                         DYSE=sw$DYSE, ATSW1=sw$ATSW[1], FTSWRZ=sw$FTSWRZ,
                                         ATSWSL=sw$ATSWSL, CE=sw$CE))
  wrow <- wrow + 1
}

cat("Total days simulated pre-sowing:", nrow(results), "\n")
cat("Final ATSWSL:", tail(results$ATSWSL,1), "\n")
cat("Final CE (cumulative evap):", tail(results$CE,1), "\n")
cat("Total RAIN pre-sowing:", sum(results$RAIN), "\n")
cat("Num days RAIN>10:", sum(results$RAIN>10), "\n\n")

cat("Days where DYSE resets to 1 (after being >1):\n")
resets <- which(diff(results$DYSE) < 0)
cat("Number of resets:", length(resets), "\n")
print(head(results[resets,], 20))

cat("\nMax DYSE reached:", max(results$DYSE), "\n")
cat("\nLast 20 days before sowing:\n")
print(tail(results, 20))

cat("\nFirst 20 days:\n")
print(head(results, 20))
