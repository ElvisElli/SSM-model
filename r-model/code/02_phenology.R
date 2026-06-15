# =============================================================================
# SSM Soybean Model - Phenology Sub-Model (Legumes)
# =============================================================================
# Implements the biological day (BD) phenology framework for legumes (soybean).
#
# Reference: Soltani & Sinclair (2012), Modeling Physiology of Crop
# Development, Growth and Yield.
#
# Biological days (BD) accumulate based on temperature and photoperiod
# responses. Each unit of BD represents one unit of physiological time.
# Phenological stages are defined as cumulative BD thresholds.
#
# Stage sequence for legumes (soybean):
#   SOW → EMR (emergence) → R1 (flowering) → R3 (beginning pod) →
#   R5 (beginning seed) → R7 (physiological maturity) → R8 (harvest maturity)
# =============================================================================


# =============================================================================
# FUNCTION: temperature_function
# Calculates the normalized temperature response (0–1) for phenology and RUE.
# Uses a trapezoidal (dent-like) function based on Soltani & Sinclair (2012).
#
# Args:
#   TMP  - Mean daily temperature (°C)
#   TBD  - Base temperature (development stops below this)
#   TP1D - Lower optimal temperature (maximum response above this)
#   TP2D - Upper optimal temperature (response starts declining above this)
#   TCD  - Ceiling temperature (development stops above this)
#
# Returns:
#   Scalar value between 0 and 1 (normalized temperature response)
# =============================================================================
temperature_function <- function(TMP, TBD, TP1D, TP2D, TCD) {
  if (TMP <= TBD || TMP >= TCD) return(0)
  if (TMP > TBD  && TMP < TP1D) return((TMP - TBD) / (TP1D - TBD))
  if (TMP >= TP1D && TMP <= TP2D) return(1)
  if (TMP > TP2D && TMP < TCD)  return((TCD - TMP) / (TCD - TP2D))
  return(0)
}


# =============================================================================
# FUNCTION: calc_daylength
# Calculates astronomical daylength (h) for a given day of year and latitude.
# Uses the standard solar declination approach.
#
# Args:
#   doy - Day of year (1–365)
#   LAT - Latitude (decimal degrees)
#
# Returns:
#   Named list: $DAYL (daylength in hours), $SINLD, $COSLD, $AOB
#   (intermediate values reused in hourly VPD calculations)
# =============================================================================
calc_daylength <- function(doy, LAT) {
  Pi  <- pi
  RDN <- Pi / 180

  # Solar declination (radians)
  DEC <- sin(23.45 * RDN) * cos(2 * Pi * (doy + 10) / 365)
  DEC <- atan(DEC / sqrt(1 - DEC^2)) * (-1)  # convert to radians (asin equiv.)

  SINLD <- sin(RDN * LAT) * sin(DEC)
  COSLD <- cos(RDN * LAT) * cos(DEC)

  AOB  <- SINLD / COSLD
  # Guard against arcsin domain issues
  AOB_safe <- max(-0.9999, min(0.9999, AOB))
  AOB2 <- atan(AOB_safe / sqrt(1 - AOB_safe^2))

  DAYL <- 12 * (1 + 2 * AOB2 / Pi)

  list(DAYL = DAYL, SINLD = SINLD, COSLD = COSLD, AOB = AOB, DEC = DEC)
}


# =============================================================================
# FUNCTION: photoperiod_function_legume
# Calculates the photoperiod response factor (ppfun) for legumes (soybean).
#
# Soybean is a short-day plant (SDP): flowering is promoted by short days
# and inhibited by long days. Represented by ppsen < 0.
#
# The photoperiod effective daylength pp = DAYL + 0.9 (adds ~54 min for
# twilight effect on photoperiod perception).
#
# Response is active only between bdBRP and bdTRP (vegetative period).
#
# Args:
#   pp    - Effective photoperiod (DAYL + 0.9, hours)
#   cpp   - Critical photoperiod (hours) — above this, development is slowed
#   ppsen - Photoperiod sensitivity coefficient
#             Positive: long-day plant (responds when pp < cpp)
#             Negative: short-day plant (responds when pp > cpp)
#   CBD   - Current cumulative biological days
#   bdBRP - Biological day when photoperiod response begins (emergence)
#   bdTRP - Biological day when photoperiod response ends
#
# Returns:
#   Scalar 0–1 (1 = no constraint, <1 = development slowed by photoperiod)
# =============================================================================
photoperiod_function_legume <- function(pp, cpp, ppsen, CBD, bdBRP, bdTRP) {
  # Outside the photoperiod-sensitive window: no effect
  if (CBD < bdBRP || CBD > bdTRP) return(1)

  if (ppsen >= 0) {
    # Long-day plant: slow development when pp < cpp
    ppfun <- 1 - ppsen * (cpp - pp)
  } else {
    # Short-day plant (soybean): slow development when pp > cpp
    ppfun <- 1 - (-ppsen) * (pp - cpp)
  }

  # Constrain to [0, 1]
  ppfun <- max(0, min(1, ppfun))
  return(ppfun)
}


# =============================================================================
# FUNCTION: init_phenology_legume
# Initializes phenological stage thresholds (in biological days) for a legume.
# Called once at the start of each simulation year.
#
# For legumes, stage bioday thresholds are supplied as parameters in the
# crop parameter table. The physiological stages (BSG, TSG, etc.) may either
# be input directly or linked to the phenological stages within the model code.
#
# Args:
#   crop_pars - Named list of crop parameters (from scenarios.csv row)
#
# Returns:
#   Named list of all bioday thresholds for the simulation
# =============================================================================
init_phenology_legume <- function(crop_pars) {
  # Phenological stage durations (in biological days)
  bdSOWEMR <- as.numeric(crop_pars$bdSOWEMR)  # Sow to emergence
  bdEMRR1  <- as.numeric(crop_pars$bdEMRR1)   # Emergence to R1 (flowering)
  bdR1R3   <- as.numeric(crop_pars$bdR1R3)    # R1 to R3 (beginning pod)
  bdR3R5   <- as.numeric(crop_pars$bdR3R5)    # R3 to R5 (beginning seed)
  bdR5R7   <- as.numeric(crop_pars$bdR5R7)    # R5 to R7 (physiol. maturity)
  bdR7R8   <- as.numeric(crop_pars$bdR7R8)    # R7 to R8 (harvest maturity)

  # Cumulative bioday thresholds for each stage
  bdEMR <- bdSOWEMR
  bdR1  <- bdEMR + bdEMRR1
  bdR3  <- bdR1  + bdR1R3
  bdR5  <- bdR3  + bdR3R5
  bdR7  <- bdR5  + bdR5R7
  bdR8  <- bdR7  + bdR7R8
  bdMAT <- bdR8

  # Physiological stage thresholds — these drive LAI senescence,
  # DM distribution (seed filling), and root growth.
  # Supplied directly from the crop parameter sheet (rows 91–104).
  bdBRP  <- as.numeric(crop_pars$bdBRP)  # Begin photoperiod response (= bdEMR)
  bdTRP  <- as.numeric(crop_pars$bdTRP)  # End photoperiod response
  bdBSG  <- as.numeric(crop_pars$bdBSG)  # Begin seed growth (linear HI phase)
  bdTSG  <- as.numeric(crop_pars$bdTSG)  # End seed growth
  bdTLM  <- as.numeric(crop_pars$bdTLM)  # End leaf production on main stem
  bdTLP  <- as.numeric(crop_pars$bdTLP)  # End leaf production on plant
  bdBLS  <- as.numeric(crop_pars$bdBLS)  # Begin leaf senescence (= bdTLP)
  bdFLW  <- as.numeric(crop_pars$bdFLW)  # Flowering bioday

  # Flowering window (for heat/frost effects on yield — currently set to 0)
  bdBXTF <- bdR1 - 5   # Begin flowering window
  bdTXTF <- bdR1 + 10  # End flowering window

  # Root growth window
  bdBRG  <- bdEMR  # Begin root depth growth
  bdTRG  <- bdBSG  # End root depth growth

  # N fixation begins after this bioday (legumes; very early for soybean)
  bdBNF  <- as.numeric(crop_pars$bdBNF)

  list(
    bdSOWEMR = bdSOWEMR, bdEMR = bdEMR,
    bdR1 = bdR1, bdR3 = bdR3, bdR5 = bdR5, bdR7 = bdR7, bdR8 = bdR8,
    bdMAT = bdMAT,
    bdBRP = bdBRP, bdTRP = bdTRP,
    bdBSG = bdBSG, bdTSG = bdTSG,
    bdBLG = bdEMR,  # begin leaf growth = emergence
    bdTLM = bdTLM, bdTLP = bdTLP, bdBLS = bdBLS,
    bdFLW = bdFLW, bdBXTF = bdBXTF, bdTXTF = bdTXTF,
    bdBRG = bdBRG, bdTRG = bdTRG, bdBNF = bdBNF
  )
}


# =============================================================================
# FUNCTION: step_phenology_legume
# Advances phenology by one day for legumes (soybean).
#
# Calculates:
#   1. Temperature response (tempfun)
#   2. Photoperiod response (ppfun)
#   3. Daily biological day increment (bd)
#   4. Daily thermal time (DTU = (TP1D-TBD) * tempfun, in °C·d)
#   5. Updates cumulative biological days (CBD)
#   6. Tracks days to each phenological stage (dtEMR, dtR1, ..., dtR8)
#
# Args:
#   state    - Named list of current state variables
#   pars     - Named list: TBD, TP1D, TP2D, TCD, cpp, ppsen, StopDoy
#   bd_thres - Named list of bioday thresholds (from init_phenology_legume)
#   weather  - Named list: TMP, doy
#
# Returns:
#   Updated state list with CBD, bd, DTU, dtEMR, dtR1..dtR8, MAT, MATYP
# =============================================================================
step_phenology_legume <- function(state, pars, bd_thres, weather) {
  TMP  <- weather$TMP
  doy  <- weather$doy
  CBD  <- state$CBD
  WSFD <- state$WSFD
  DAP  <- state$DAP

  # Temperature response (trapezoidal function)
  tempfun <- temperature_function(TMP, pars$TBD, pars$TP1D, pars$TP2D, pars$TCD)

  # Daylength and photoperiod
  dl   <- calc_daylength(doy, pars$LAT)
  DAYL <- dl$DAYL
  pp   <- DAYL + 0.9  # effective photoperiod (civil twilight)

  # Photoperiod response
  ppfun <- photoperiod_function_legume(pp, pars$cpp, pars$ppsen,
                                        CBD, bd_thres$bdBRP, bd_thres$bdTRP)

  # Daily biological day (bd) and thermal time (DTU)
  if (CBD <= bd_thres$bdEMR) {
    # Pre-emergence: no water stress effect on rate
    DTU <- (pars$TP1D - pars$TBD) * tempfun
    bd  <- tempfun * ppfun
  } else {
    # Post-emergence: water deficit can accelerate maturity
    DTU <- (pars$TP1D - pars$TBD) * tempfun * WSFD
    bd  <- tempfun * ppfun * WSFD
  }

  CBD <- CBD + bd
  DAP <- DAP + 1

  # Track days to reach each stage (updated while CBD is still below threshold)
  dtEMR <- if (CBD < bd_thres$bdEMR) DAP + 1 else state$dtEMR
  dtR1  <- if (CBD < bd_thres$bdR1)  DAP + 1 else state$dtR1
  dtR3  <- if (CBD < bd_thres$bdR3)  DAP + 1 else state$dtR3
  dtR5  <- if (CBD < bd_thres$bdR5)  DAP + 1 else state$dtR5
  dtR7  <- if (CBD < bd_thres$bdR7)  DAP + 1 else state$dtR7
  dtR8  <- if (CBD < bd_thres$bdR8)  DAP + 1 else state$dtR8
  dtR0  <- -9  # not used for legumes (wheat/maize stage)

  # Check maturity
  MAT   <- state$MAT
  MATYP <- state$MATYP
  if (CBD > bd_thres$bdMAT) {
    MAT   <- 1L
    MATYP <- 1L  # normal maturity
  }
  if (doy == pars$StopDoy) {
    MAT   <- 1L
    MATYP <- 4L  # stopped at StopDoy
  }

  state$CBD    <- CBD
  state$DAP    <- DAP
  state$bd     <- bd
  state$DTU    <- DTU
  state$DAYL   <- DAYL
  state$pp     <- pp
  state$tempfun <- tempfun
  state$ppfun  <- ppfun
  state$dtEMR  <- dtEMR
  state$dtR0   <- dtR0
  state$dtR1   <- dtR1
  state$dtR3   <- dtR3
  state$dtR5   <- dtR5
  state$dtR7   <- dtR7
  state$dtR8   <- dtR8
  state$MAT    <- MAT
  state$MATYP  <- MATYP

  return(state)
}
