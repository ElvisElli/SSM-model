# =============================================================================
# SSM Soybean Model - Main Integration Module
# =============================================================================
# Integrates all sub-models into a complete daily simulation loop.
#
# The simulation follows this daily sequence:
#   1. Read weather data for the day
#   2. PhenologyBD  - advance CBD, calculate DTU, check maturity
#   3. CropLAI      - update LAI
#   4. DMProduction - calculate daily DM production (DDMP) and TR
#   5. DMDistribution - partition DM to grain and vegetative organs
#   6. SoilWater    - update soil water balance, calculate stress factors
#
# Water stress factors (WSFL, WSFG, WSFD, WSFN) calculated in SoilWater
# feed back into the NEXT day's phenology and LAI calculations.
#
# The model simulates from the day before sowing (SimDoy) through maturity.
# =============================================================================

# Source all sub-model files (relative to code directory)
source_ssm_modules <- function(code_dir = ".") {
  source(file.path(code_dir, "01_read_inputs.R"))
  source(file.path(code_dir, "02_phenology.R"))
  source(file.path(code_dir, "03_crop_lai.R"))
  source(file.path(code_dir, "04_dm_production.R"))
  source(file.path(code_dir, "05_dm_distribution.R"))
  source(file.path(code_dir, "06_soil_water.R"))
}


# =============================================================================
# FUNCTION: run_ssm_scenario
# Runs the SSM model for one scenario (location × management × soil × crop)
# over one year.
#
# Args:
#   scn        - Named list (one row from scenarios.csv) with all parameters:
#                loc_name, lat, vpdf, co2, tchng, pchng,
#                fyear, yrno, pdoy, sim_doy, pden, water, irglvl,
#                fix_find, lpdoy, stop_doy, nitrogen, ...
#                + all crop parameters (PHYL, PLACON, ...)
#   wth_data   - data.frame with weather (YEAR, DOY, SRAD, TMAX, TMIN, RAIN)
#   soil_data  - Named list from init_soil_water (via read_soil)
#   year       - Simulation year (integer)
#   verbose    - Print daily output? (logical, default FALSE)
#
# Returns:
#   Named list with:
#     $summary - One-row data.frame of seasonal summary outputs
#     $daily   - data.frame of daily outputs (if verbose = TRUE)
# =============================================================================
run_ssm_year <- function(scn, wth_data, soil, year, verbose = FALSE, init_ftswrz = 0) {

  # ---- Scenario parameters -----------------------------------------------
  LAT      <- as.numeric(scn$lat)
  VPDF     <- as.numeric(scn$vpdf)
  CO2      <- as.numeric(scn$co2)
  CO2REF   <- as.numeric(scn$CO2REF)
  tchng    <- as.numeric(scn$tchng)
  pchng    <- as.numeric(scn$pchng)
  Pdoy     <- as.integer(scn$pdoy)    # sowing DOY
  SimDoy   <- as.integer(scn$sim_doy)  # simulation start DOY
  Lpdoy    <- as.integer(scn$lpdoy)    # last possible sowing DOY
  StopDoy  <- as.integer(scn$stop_doy)
  PDEN     <- as.numeric(scn$pden)
  water    <- as.integer(scn$water)
  IRGLVL   <- as.numeric(scn$irglvl)
  if (!is.numeric(IRGLVL) || is.na(IRGLVL)) IRGLVL <- 0.5
  FixFind  <- as.integer(scn$fix_find)
  nitrogen <- as.integer(scn$nitrogen)
  MC       <- as.numeric(scn$MC)  # grain moisture content %

  # ---- Crop pars object for sub-model inits ------------------------------
  crop_pars        <- as.list(scn)
  crop_pars$pden   <- PDEN
  crop_pars$lat    <- LAT     # needed by init_dm_production for hourly solar calc

  # ---- Initialize sub-models ---------------------------------------------
  bd_thres <- init_phenology_legume(crop_pars)
  lai_st   <- init_crop_lai(crop_pars)
  dmp_pars <- init_dm_production(crop_pars, CO2 = CO2, CO2REF = CO2REF)
  dmd_st   <- init_dm_distribution(crop_pars)
  sw       <- init_soil_water(soil, crop_pars, CO2 = CO2, CO2REF = CO2REF,
                               init_ftswrz = init_ftswrz)

  # ---- Build weather lookup for this year ---------------------------------
  # VBA reads sequentially; we subset weather to the simulation year
  # The model can span calendar year boundary (sow in year Y, mature in Y+1)
  wth_year <- wth_data[wth_data$YEAR == year, ]
  wth_next <- wth_data[wth_data$YEAR == (year + 1), ]
  wth_sim  <- rbind(wth_year, wth_next)  # combine in case of year rollover

  # ---- State variables ---------------------------------------------------
  CBD    <- 0; DAP <- 0; DAS <- 0; MAT <- 0L; MATYP <- 1L
  WSFD   <- 1; WSFL <- 1; WSFG <- 1; WSFN <- 1; WSXF <- 1
  SNOW   <- 0

  # Accumulators for environmental summaries
  DAYT <- 0; SRAINT <- 0; STMINT <- 0; STMAXT <- 0; SSRADT <- 0; SUMETT <- 0
  DAY2 <- 0; SRAIN2 <- 0; STMIN2 <- 0; STMAX2 <- 0; SSRAD2 <- 0; SUMET2 <- 0
  DAY3 <- 0; SRAIN3 <- 0; STMIN3 <- 0; STMAX3 <- 0; SSRAD3 <- 0; SUMET3 <- 0

  # Stage-day tracking (initialized to DAP+1 sentinel)
  dtEMR <- 9999L; dtR0 <- -9L; dtR1 <- 9999L; dtR3 <- 9999L
  dtR5  <- 9999L; dtR7 <- 9999L; dtR8 <- 9999L

  # DM at R5/anthesis
  R5ANTLAI <- 0; R5ANTDM <- 0
  BLSLAI    <- 0
  DDMP      <- 0

  daily_out <- list()

  # ====================================================================
  # FIND SIMULATION START DATE (advance weather to SimDoy)
  # ====================================================================
  wrow <- which(wth_sim$DOY == SimDoy & wth_sim$YEAR == year)
  if (length(wrow) == 0) {
    warning(sprintf("SimDoy=%d not found in weather for year=%d", SimDoy, year))
    return(NULL)
  }
  wrow <- wrow[1]  # start at SimDoy

  # ====================================================================
  # ADVANCE TO SOWING DATE (FixFind=0: fixed sowing date = Pdoy)
  # Run soil water before sowing if needed (no-op when SimDoy == Pdoy,
  # which is the case for every scenario in scenarios.csv — Excel does
  # not run a multi-day pre-sowing loop; soil starts at DUL and only the
  # sowing day itself runs step_soil_water before the crop loop begins).
  # ====================================================================
  while (wrow <= nrow(wth_sim) && wth_sim$DOY[wrow] != Pdoy) {
    w <- get_weather_row(wth_sim, wrow, tchng, pchng, SNOW)
    SNOW <- w$SNOW

    if (water %in% c(1, 2, 3)) {
      state_pre <- list(CBD=0, DAP=0, bd=0, DDMP=0, TR=0, LAI=0, BLSLAI=0,
                        VPDF=VPDF, WSFL=WSFL, WSFG=WSFG, PDEN=PDEN)
      sw <- step_soil_water(sw, state_pre, bd_thres, w, water, IRGLVL)
    }
    DAS  <- DAS + 1
    wrow <- wrow + 1
    if (wrow > nrow(wth_sim)) break
  }

  # Sowing day (Pdoy): CBD stays 0, run soil water only — no phenology.
  # DAP stays at 0; step_phenology_legume increments it to 1 on the first growth day
  # (Pdoy+1), matching Excel where DAP=0 on sowing day and DAP=1 on Pdoy+1.
  if (wrow <= nrow(wth_sim) && wth_sim$DOY[wrow] == Pdoy) {
    w    <- get_weather_row(wth_sim, wrow, tchng, pchng, SNOW)
    SNOW <- w$SNOW
    if (water %in% c(1, 2, 3)) {
      state_sow <- list(CBD=0, DAP=0, bd=0, DDMP=0, TR=0, LAI=0, BLSLAI=0,
                        VPDF=VPDF, WSFL=WSFL, WSFG=WSFG, PDEN=PDEN)
      sw <- step_soil_water(sw, state_sow, bd_thres, w, water, IRGLVL)
      WSFL <- sw$WSFL; WSFG <- sw$WSFG; WSFD <- sw$WSFD; WSFN <- sw$WSFN
    }
    DAS  <- DAS + 1
    wrow <- wrow + 1
  }

  # ====================================================================
  # MAIN DAILY SIMULATION LOOP (from Pdoy+1 to maturity)
  # Order matches the Excel VBA model:
  #   Phenology → CropLAI (yesterday's GLF) → DMProduction → DMDistribution → SoilWater
  # ====================================================================
  i_day <- 0L  # sequential daily output counter

  while (MAT == 0 && wrow <= nrow(wth_sim)) {
    # --- Read weather for today -------------------------------------------
    w    <- get_weather_row(wth_sim, wrow, tchng, pchng, SNOW)
    SNOW <- w$SNOW
    doy  <- w$doy

    # Next day's Tmin for hourly temperature curve (afternoon limb); VBA: TMINA
    TMINA <- if (wrow + 1 <= nrow(wth_sim)) wth_sim$TMIN[wrow + 1] + tchng else w$TMIN

    # Shared state object
    state <- list(
      CBD = CBD, DAP = DAP, bd = 0, DTU = 0,
      WSFD = WSFD, WSFL = WSFL, WSFG = WSFG, WSFN = WSFN,
      LAI  = lai_st$LAI, BLSLAI = BLSLAI,
      DDMP = DDMP, TR = 0,
      TMIN = w$TMIN, TMAX = w$TMAX, TMP = w$TMP,
      VPDF = VPDF, PDEN = PDEN, TMINA = TMINA,
      dtEMR = dtEMR, dtR0 = dtR0, dtR1 = dtR1, dtR3 = dtR3,
      dtR5 = dtR5, dtR7 = dtR7, dtR8 = dtR8,
      MAT = MAT, MATYP = MATYP
    )

    # --- 1. PHENOLOGY (advance CBD, calculate bd, DTU) -------------------
    pheno_pars <- list(
      TBD = as.numeric(scn$TBD), TP1D = as.numeric(scn$TP1D),
      TP2D = as.numeric(scn$TP2D), TCD = as.numeric(scn$TCD),
      cpp = as.numeric(scn$cpp), ppsen = as.numeric(scn$ppsen),
      LAT = LAT, StopDoy = StopDoy
    )
    state <- step_phenology_legume(state, pheno_pars, bd_thres, w)

    CBD    <- state$CBD; DAP <- state$DAP
    bd     <- state$bd;  DTU <- state$DTU
    dtEMR  <- state$dtEMR; dtR0 <- state$dtR0; dtR1 <- state$dtR1
    dtR3   <- state$dtR3;  dtR5 <- state$dtR5; dtR7 <- state$dtR7
    dtR8   <- state$dtR8
    MAT    <- state$MAT; MATYP <- state$MATYP
    state$CBD <- CBD; state$DAP <- DAP
    state$bd  <- bd;  state$DTU <- DTU

    # --- 2. CROP LAI (uses yesterday's GLF = dmd_st$GLF) ----------------
    lai_st <- step_crop_lai(lai_st, state, dmd_st$GLF, bd_thres)

    # Pre-mature senescence: LAI too low during seed fill
    if (CBD > bd_thres$bdBSG && CBD < bd_thres$bdTSG && lai_st$LAI < 0.05) {
      CBD   <- bd_thres$bdTSG
      MATYP <- 2L
    }

    # Save BLSLAI for soil evaporation after BSG
    if (CBD <= bd_thres$bdBLS) BLSLAI <- lai_st$LAI
    lai_st$BLSLAI <- BLSLAI
    state$LAI    <- lai_st$LAI
    state$BLSLAI <- BLSLAI

    # --- 3. DM PRODUCTION (DDMP, TR) using today's updated LAI ----------
    dmp_result <- step_dm_production(dmp_pars, state, bd_thres, w)
    DDMP   <- dmp_result$DDMP
    FINT   <- dmp_result$FINT
    TCFRUE <- dmp_result$TCFRUE
    state$DDMP <- DDMP
    state$TR   <- dmp_result$TR

    # --- 4. DM DISTRIBUTION (SGR, WGRN, GLF for next day) --------------
    dmd_st <- step_dm_distribution(dmd_st, state, bd_thres)

    # Save LAI and DM at R5 (seed fill start)
    if (CBD <= bd_thres$bdR5) {
      R5ANTLAI <- lai_st$LAI
      R5ANTDM  <- dmd_st$WTOP
    }

    # --- 5. SOIL WATER --------------------------------------------------
    if (water %in% c(1, 2, 3)) {
      # VBA SoilWater resets cumulative balances at DAP=1 (first growth day),
      # discarding any pre-sowing evaporation/drainage from the sowing day.
      # iATSW/ISOLWAT are also snapped to the post-sowing-day soil state.
      if (DAP == 1L) {
        sw$CE     <- 0; sw$CTR    <- 0; sw$CRAIN  <- 0
        sw$CRUNOF <- 0; sw$CDRAIN <- 0
        sw$iATSW  <- sw$ATSW
        sw$iFTSW  <- sw$FTSW
        sw$ISOLWAT <- sw$ATSWSL
      }
      sw <- step_soil_water(sw, state, bd_thres, w, water, IRGLVL)

      # Feed water stress back for next day
      WSFL  <- sw$WSFL
      WSFG  <- sw$WSFG
      WSFD  <- sw$WSFD
      WSFN  <- sw$WSFN
      WSXF  <- sw$WSXF

      # Flood kill
      if (sw$FLDUR > sw$FLDKIL) {
        CBD   <- bd_thres$bdTSG
        MATYP <- 5L
      }
    }

    # --- 6. ENVIRONMENTAL SUMMARIES (sowing to maturity) ----------------
    if (CBD <= bd_thres$bdMAT) {
      DAYT  <- DAYT  + 1; SRAINT <- SRAINT + w$RAIN; STMINT <- STMINT + w$TMIN
      STMAXT<- STMAXT + w$TMAX; SSRADT <- SSRADT + w$SRAD
      SUMETT <- SUMETT + sw$SEVP + sw$TR
    }
    if (CBD <= bd_thres$bdBSG) {
      DAY2  <- DAY2  + 1; SRAIN2 <- SRAIN2 + w$RAIN; STMIN2 <- STMIN2 + w$TMIN
      STMAX2<- STMAX2 + w$TMAX; SSRAD2 <- SSRAD2 + w$SRAD
      SUMET2 <- SUMET2 + sw$SEVP + sw$TR
    }
    if (CBD > bd_thres$bdBSG && CBD <= bd_thres$bdMAT) {
      DAY3  <- DAY3  + 1; SRAIN3 <- SRAIN3 + w$RAIN; STMIN3 <- STMIN3 + w$TMIN
      STMAX3<- STMAX3 + w$TMAX; SSRAD3 <- SSRAD3 + w$SRAD
      SUMET3 <- SUMET3 + sw$SEVP + sw$TR
    }

    # --- Daily output (optional) ----------------------------------------
    if (verbose) {
      i_day <- i_day + 1L
      daily_out[[i_day]] <- list(
        doy = doy, DAP = DAP, TMP = w$TMP, DTU = DTU, CBD = CBD,
        MSNN = lai_st$MSNN, GLAI = lai_st$GLAI, DLAI = lai_st$DLAI,
        LAI = lai_st$LAI, TCFRUE = TCFRUE, FINT = FINT, DDMP = DDMP,
        GLF = dmd_st$GLF, GST = dmd_st$GST, SGR = dmd_st$SGR,
        WLF = dmd_st$WLF, WST = dmd_st$WST, WVEG = dmd_st$WVEG,
        WGRN = dmd_st$WGRN, WTOP = dmd_st$WTOP, DEPORT = sw$DEPORT,
        RAIN = w$RAIN, IRGW = sw$IRGW, RUNOF = sw$RUNOF, PET = sw$PET,
        SEVP = sw$SEVP, TR = sw$TR, DRAIN = sw$DRAIN,
        ATSWRZ = sw$ATSWRZ, FTSWRZ = sw$FTSWRZ,
        CRAIN = sw$CRAIN, CIRGW = sw$CIRGW, IRGNO = sw$IRGNO,
        CRUNOF = sw$CRUNOF, CE = sw$CE, CTR = sw$CTR, CDRAIN = sw$CDRAIN,
        WSFL = WSFL, WSFG = WSFG, WSFD = WSFD
      )
    }

    wrow <- wrow + 1
  }

  # ====================================================================
  # SEASONAL SUMMARY OUTPUT
  # ====================================================================
  MTMINT <- if (DAYT > 0) STMINT / DAYT else NA
  MTMAXT <- if (DAYT > 0) STMAXT / DAYT else NA
  MTMIN2 <- if (DAY2 > 0) STMIN2 / DAY2 else NA
  MTMAX2 <- if (DAY2 > 0) STMAX2 / DAY2 else NA
  MTMIN3 <- if (DAY3 > 0) STMIN3 / DAY3 else NA
  MTMAX3 <- if (DAY3 > 0) STMAX3 / DAY3 else NA

  # Wet yield (adjusted for moisture content)
  Ywet <- dmd_st$WGRN / (1 - MC / 100) * 10   # kg/ha (×10 converts g/m² to kg/ha)

  summary_row <- data.frame(
    sName    = scn$scenario,
    Location = scn$loc_name,
    Manag    = scn$manag_name,
    Soil     = scn$soil_name,
    Crop     = scn$crop_name,
    Pyear    = year,
    Pdoy     = Pdoy,
    dtEMR    = dtEMR,
    R0       = dtR0,
    R1       = dtR1,
    R2       = -9L,   # not used for legumes
    R3       = dtR3,
    R4       = -9L,   # not used for legumes
    R5       = dtR5,
    R6       = -9L,   # not used for legumes
    R7       = dtR7,
    R8       = dtR8,
    MSNN     = lai_st$MSNN,
    MXLAI    = lai_st$MXLAI,
    R5ANTDM  = R5ANTDM,
    WTOP     = dmd_st$WTOP,
    WGRN     = dmd_st$WGRN,
    HI       = dmd_st$HI,
    IPASW    = sw$ISOLWAT,
    CRAIN    = sw$CRAIN,
    CIRGW    = sw$CIRGW,
    IRGNO    = sw$IRGNO,
    ATSWSL   = sw$ATSWSL,
    CRUNOF   = sw$CRUNOF,
    CE       = sw$CE,
    CTR      = sw$CTR,
    CDRAIN   = sw$CDRAIN,
    ET       = sw$CE + sw$CTR,
    EoverET  = sw$CE / max(sw$CE + sw$CTR, 1e-8),
    # N outputs (zero for nitrogen=0 runs)
    NLF = 0, NST = 0, NVEG = 0, NGRN = 0, CNUP = 0, CUMBNF = 0,
    ISOLN = 0, CNFERT = 0, CNMIN = 0, CNSOL = 0, CNVOL = 0, CNLEACH = 0, CNDNIT = 0,
    MATYP    = MATYP,
    SRAINT   = SRAINT,
    MTMINT   = MTMINT,
    MTMAXT   = MTMAXT,
    SSRADT   = SSRADT,
    SUMETT   = SUMETT,
    SRAIN2   = SRAIN2,
    MTMIN2   = MTMIN2,
    MTMAX2   = MTMAX2,
    SSRAD2   = SSRAD2,
    SUMET2   = SUMET2,
    SRAIN3   = SRAIN3,
    MTMIN3   = MTMIN3,
    MTMAX3   = MTMAX3,
    SSRAD3   = SSRAD3,
    SUMET3   = SUMET3,
    Ywet     = Ywet,
    stringsAsFactors = FALSE
  )

  daily_df <- if (verbose && length(daily_out) > 0) {
    do.call(rbind, lapply(daily_out, as.data.frame))
  } else {
    NULL
  }

  list(summary = summary_row, daily = daily_df,
       final_ftswrz = sw$FTSWRZ,
       layer_state = list(
         iATSW = sw$iATSW, iFTSW = sw$iFTSW,
         fATSW = sw$ATSW,  fFTSW = sw$FTSW
       ))
}


# =============================================================================
# HELPER: get_weather_row
# Extracts and adjusts one day of weather data from the weather data.frame.
# Applies temperature (tchng) and precipitation (pchng) adjustments.
# Implements snow accumulation and melt.
#
# Args:
#   wth   - data.frame of weather data
#   row   - row index to read
#   tchng - temperature change (°C)
#   pchng - precipitation multiplier (fraction)
#   SNOW  - current snow accumulation (mm)
#
# Returns:
#   Named list: YEAR, doy, SRAD, TMAX, TMIN, TMP, RAIN, SNOW
# =============================================================================
get_weather_row <- function(wth, row, tchng = 0, pchng = 1, SNOW = 0) {
  if (row > nrow(wth)) {
    stop(paste("Weather data exhausted at row", row))
  }
  Yr   <- wth$YEAR[row]
  doy  <- wth$DOY[row]
  SRAD <- wth$SRAD[row]
  TMAX <- wth$TMAX[row] + tchng
  TMIN <- wth$TMIN[row] + tchng
  RAIN <- wth$RAIN[row] * pchng
  RAIN <- max(RAIN, 0)  # can't be negative after adjustment

  TMP  <- (TMAX + TMIN) / 2

  # Snow accumulation and melt
  SNOMLT <- 0
  if (TMAX <= 1) {
    SNOW <- SNOW + RAIN
    RAIN <- 0
  } else if (TMAX > 1 && SNOW > 0) {
    SNOMLT <- TMAX + RAIN * 0.4
    if (SNOMLT > SNOW) SNOMLT <- SNOW
    SNOW <- SNOW - SNOMLT
    RAIN <- RAIN + SNOMLT
  }

  list(YEAR = Yr, doy = doy, SRAD = SRAD, TMAX = TMAX, TMIN = TMIN,
       TMP = TMP, RAIN = RAIN, SNOW = SNOW)
}
