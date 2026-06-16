#!/usr/bin/env Rscript
# Debug Eustis 2000 soil water trace

setwd("/home/user/SSM-model/r-model")
source("code/01_read_inputs.R")
source("code/02_phenology.R")
source("code/03_crop_lai.R")
source("code/04_dm_production.R")
source("code/05_dm_distribution.R")
source("code/06_soil_water.R")
source("code/07_ssm_model.R")

# Load scenarios
scns <- read_scenarios("inputs/scenarios.csv")
scn  <- scns[scns$scenario == "EU-RFD-ELY-check", ]

cat("sim_doy:", scn$sim_doy, "  pdoy:", scn$pdoy, "\n")
cat("DEPORT:", scn$DEPORT, "  MEED:", scn$MEED, "  GRTDP:", scn$GRTDP, "\n")

# Load soil via JSON (same as run_model.R does)
soil_json <- read_soil_json("inputs/soil_data.json")
soil <- build_soil_from_json_entry(soil_json[["77"]])
cat("NLYER:", soil$meta$NLYER, "  LDRAIN:", soil$meta$LDRAIN, "\n")
cat("Layer 1: DLYER=", soil$layers$DLYER[1], "DUL=", soil$layers$DUL[1],
    "LL=", soil$layers$LL[1], "iWL=", soil$layers$iWL[1], "\n")

wth <- read_weather(paste0("inputs/weather/", scn$wth_file))
year <- 2000
wth_year <- wth[wth$YEAR == year, ]
wth_next <- wth[wth$YEAR == (year + 1), ]
wth_sim  <- rbind(wth_year, wth_next)

# Initialize
crop_pars <- as.list(scn)
crop_pars$lat <- as.numeric(scn$lat)
sw <- init_soil_water(soil, crop_pars)
bd_thres <- init_phenology_legume(crop_pars)

cat("\n--- Initial soil state ---\n")
cat("WL[1:3]:", round(sw$WL[1:3], 4), "\n")
cat("ATSW[1:3]:", round(sw$ATSW[1:3], 4), "\n")
cat("TTSW[1:3]:", round(sw$TTSW[1:3], 4), "\n")
cat("FTSW[1:3]:", round(sw$FTSW[1:3], 4), "\n")
cat("FTSWRZ:", sw$FTSWRZ, "\n")
cat("DEPORT:", sw$DEPORT, "  RTLN:", sw$RTLN, "\n")

Pdoy   <- as.integer(scn$pdoy)
SimDoy <- as.integer(scn$sim_doy)
if (SimDoy == Pdoy) SimDoy <- Pdoy - 1
tchng  <- as.numeric(scn$tchng)
pchng  <- as.numeric(scn$pchng)
SNOW   <- 0; WSFL <- 1; WSFG <- 1; VPDF <- as.numeric(scn$vpdf)
PDEN   <- as.numeric(scn$pden); water <- as.integer(scn$water)
IRGLVL <- as.numeric(scn$irglvl)

cat("\n--- SimDoy:", SimDoy, "  Pdoy:", Pdoy, "---\n")

wrow <- which(wth_sim$DOY == SimDoy & wth_sim$YEAR == year)
wrow <- wrow[1]

# Run pre-sowing loop with tracing
cat("\n--- PRE-SOWING LOOP ---\n")
while (wrow <= nrow(wth_sim) && wth_sim$DOY[wrow] != Pdoy) {
  w <- get_weather_row(wth_sim, wrow, tchng, pchng, SNOW)
  SNOW <- w$SNOW
  cat(sprintf("DOY=%d: RAIN=%.1f TMAX=%.1f TMIN=%.1f SRAD=%.1f\n",
              w$doy, w$RAIN, w$TMAX, w$TMIN, w$SRAD))

  state_pre <- list(CBD=0, DAP=0, bd=0, DDMP=0, TR=0, LAI=0, BLSLAI=0,
                    VPDF=VPDF, WSFL=WSFL, WSFG=WSFG, PDEN=PDEN)
  sw <- step_soil_water(sw, state_pre, bd_thres, w, water, IRGLVL)
  cat(sprintf("  After: WL1=%.3f ATSW1=%.3f FTSWRZ=%.4f SEVP=%.3f DRAIN=%.3f DYSE=%d\n",
              sw$WL[1], sw$ATSW[1], sw$FTSWRZ, sw$SEVP, sw$DRAIN, sw$DYSE))
  WSFL <- sw$WSFL; WSFG <- sw$WSFG

  wrow <- wrow + 1
}

cat("\n--- SOWING DAY (Pdoy=", Pdoy, ") ---\n")
if (wrow <= nrow(wth_sim) && wth_sim$DOY[wrow] == Pdoy) {
  w <- get_weather_row(wth_sim, wrow, tchng, pchng, SNOW)
  SNOW <- w$SNOW
  cat(sprintf("DOY=%d: RAIN=%.1f TMAX=%.1f TMIN=%.1f SRAD=%.1f\n",
              w$doy, w$RAIN, w$TMAX, w$TMIN, w$SRAD))
  state_sow <- list(CBD=0, DAP=0, bd=0, DDMP=0, TR=0, LAI=0, BLSLAI=0,
                    VPDF=VPDF, WSFL=WSFL, WSFG=WSFG, PDEN=PDEN)
  sw <- step_soil_water(sw, state_sow, bd_thres, w, water, IRGLVL)
  cat(sprintf("  After: WL1=%.3f ATSW1=%.3f FTSWRZ=%.4f SEVP=%.3f DRAIN=%.3f DYSE=%d\n",
              sw$WL[1], sw$ATSW[1], sw$FTSWRZ, sw$SEVP, sw$DRAIN, sw$DYSE))
  WSFL <- sw$WSFL; WSFG <- sw$WSFG
  wrow <- wrow + 1
}

cat("\n--- MAIN LOOP (first 5 days) ---\n")
cat("Day 0 (before main loop): FTSWRZ=", sw$FTSWRZ, " WSFL=", WSFL, " WSFG=", WSFG, "\n")

# Run scenario normally for first few days to get FTSWRZ
result <- run_ssm_year(scn, wth, soil, 2000, verbose=TRUE)
if (!is.null(result$daily) && length(result$daily) > 0) {
  cat("Daily rows:", length(result$daily), "\n")
  cat("First row class:", class(result$daily[[1]]), "\n")
  cat("First row str:\n"); str(result$daily[[1]])
  # Extract columns we care about
  get_col <- function(lst, nm) sapply(lst, function(r) {
    v <- tryCatch(r[[nm]], error=function(e) NULL)
    if(is.null(v)||length(v)==0) NA_real_ else as.numeric(v[1])
  })
  dap     <- get_col(result$daily, "DAP")
  doy     <- get_col(result$daily, "doy")
  cbd     <- get_col(result$daily, "CBD")
  lai     <- get_col(result$daily, "LAI")
  ddmp    <- get_col(result$daily, "DDMP")
  ftswrz  <- get_col(result$daily, "FTSWRZ")
  wsfd    <- get_col(result$daily, "WSFD")
  wsfg    <- get_col(result$daily, "WSFG")
  tr      <- get_col(result$daily, "TR")
  sevp    <- get_col(result$daily, "SEVP")
  wgrn    <- get_col(result$daily, "WGRN")

  cat("\n--- DAILY outputs first 15 rows ---\n")
  cat(sprintf("%-4s %-4s %-7s %-6s %-6s %-7s %-6s %-6s %-5s %-5s %-6s\n",
              "DAP","doy","CBD","LAI","DDMP","FTSWRZ","WSFD","WSFG","TR","SEVP","WGRN"))
  for (i in 1:min(15, length(dap))) {
    cat(sprintf("%-4d %-4d %-7.3f %-6.3f %-6.3f %-7.4f %-6.4f %-6.4f %-5.3f %-5.3f %-6.2f\n",
                dap[i], doy[i], cbd[i], lai[i], ddmp[i], ftswrz[i], wsfd[i], wsfg[i], tr[i], sevp[i], wgrn[i]))
  }
  cat("\nFinal WGRN:", wgrn[length(wgrn)], "\n")
} else {
  cat("No daily output\n")
}
cat("dtR8:", result$summary$R8, "  WGRN:", result$summary$WGRN, "\n")
