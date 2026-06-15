# =============================================================================
# SSM Soybean Model - Dry Matter Production Sub-Model
# =============================================================================
# Calculates daily dry matter (DM) production via radiation use efficiency (RUE).
#
# DM production = intercepted PAR × RUE
#
# Key processes:
#   1. RUE adjusted for temperature (TCFRUE) and water stress (WSFG)
#   2. CO2 effect on RUE and transpiration efficiency coefficient (TEC)
#   3. PAR interception using Beer's law: FINT = 1 - exp(-KPAR × LAI)
#   4. Two modes: daily VPD (vpdtp=0) or hourly VPD (vpdtp=1)
#      For all 10 test locations, vpdtp=0 is used (simpler daily mode).
#
# Reference: Soltani & Sinclair (2012), Chapter 6
# =============================================================================


# =============================================================================
# FUNCTION: init_dm_production
# Pre-computes CO2-adjusted parameters for DM production.
# Called once per simulation year.
#
# CO2 effect equations (Soltani & Sinclair 2012):
#   RUEREF = 1 + CO2RES × log10(CO2REF / 330)
#   RUECO2 = 1 + CO2RES × log10(CO2 / 330)
#   Adjusted IRUE = IRUE_base × (RUECO2 / RUEREF)
#   Adjusted TEC  = TECREF  × (RUECO2 / RUEREF)
#
# Args:
#   crop_pars - Named list of crop parameters
#   CO2       - Ambient CO2 concentration (ppm)
#   CO2REF    - Reference CO2 concentration (ppm; when experiment was done)
#
# Returns:
#   Named list of DM production parameters (IRUE_adj, TEC_adj, KPAR, etc.)
# =============================================================================
init_dm_production <- function(crop_pars, CO2 = 420, CO2REF = 385) {
  TBRUE   <- as.numeric(crop_pars$TBRUE)
  TP1RUE  <- as.numeric(crop_pars$TP1RUE)
  TP2RUE  <- as.numeric(crop_pars$TP2RUE)
  TCRUE   <- as.numeric(crop_pars$TCRUE)
  KPAR    <- as.numeric(crop_pars$KPAR)    # Light extinction coefficient
  IRUE    <- as.numeric(crop_pars$IRUE)    # Base RUE (g DM per MJ PAR)
  CO2RES  <- as.numeric(crop_pars$CO2RES)  # CO2 responsiveness of RUE
  TECREF  <- as.numeric(crop_pars$TECREF)  # Reference TEC (Pa)
  vpdtp   <- as.numeric(crop_pars$vpdtp %||% 0)  # VPD mode: 0=daily, 1=hourly
  VPDcr   <- as.numeric(crop_pars$VPDcr  %||% 2) # Critical VPD (kPa)

  # CO2 adjustment factor
  # VBA uses Log()/Log(10) = log10(); R's log() is natural log
  RUEREF <- 1 * (1 + CO2RES * log10(CO2REF / 330))
  RUECO2 <- 1 * (1 + CO2RES * log10(CO2 / 330))
  co2_factor <- RUECO2 / RUEREF

  # Adjusted RUE and TEC
  IRUE_adj <- IRUE * co2_factor
  TEC_adj  <- TECREF * co2_factor

  list(
    TBRUE   = TBRUE,
    TP1RUE  = TP1RUE,
    TP2RUE  = TP2RUE,
    TCRUE   = TCRUE,
    KPAR    = KPAR,
    IRUE    = IRUE_adj,
    TEC     = TEC_adj,
    vpdtp   = vpdtp,
    VPDcr   = VPDcr
  )
}


# =============================================================================
# FUNCTION: step_dm_production
# Calculates daily DM production and transpiration for one day.
#
# Daily mode (vpdtp=0):
#   DDMP = SRAD × 0.48 × FINT × RUE   [g DM/m²/d]
#   TR   = DDMP × VPD / TEC             [mm/d]
#
# Hourly mode (vpdtp=1) is not implemented here (not needed for soybean runs).
#
# Args:
#   dmp_pars  - Named list from init_dm_production
#   state     - Named list: CBD, LAI, WSFG, TMIN, TMAX, SRAD, VPDF
#   bd_thres  - Named list: bdEMR, bdTSG
#   weather   - Named list: TMIN, TMAX, SRAD, doy
#
# Returns:
#   Named list: DDMP, FINT, RUE, TCFRUE, TR
# =============================================================================
step_dm_production <- function(dmp_pars, state, bd_thres, weather) {
  TMP   <- (weather$TMAX + weather$TMIN) / 2
  SRAD  <- weather$SRAD
  TMIN  <- weather$TMIN
  TMAX  <- weather$TMAX
  VPDF  <- state$VPDF   # VPD adjustment factor from location parameters

  CBD   <- state$CBD
  LAI   <- state$LAI
  WSFG  <- state$WSFG

  # --- Temperature function for RUE (same trapezoidal form as phenology) ---
  TCFRUE <- temperature_function(TMP,
                                  dmp_pars$TBRUE, dmp_pars$TP1RUE,
                                  dmp_pars$TP2RUE, dmp_pars$TCRUE)

  # RUE adjusted for temperature and water stress
  RUE <- dmp_pars$IRUE * TCFRUE * WSFG

  # RUE is zero outside the growing period
  if (CBD < bd_thres$bdEMR || CBD > bd_thres$bdTSG) RUE <- 0

  # --- PAR interception via Beer's law ---
  FINT <- 1 - exp(-dmp_pars$KPAR * LAI)

  # --- Daily DM production ---
  # PAR fraction of solar radiation = 0.48 (48% of total solar)
  DDMP <- SRAD * 0.48 * FINT * RUE   # g DM/m²/d

  # --- Daily VPD mode (vpdtp = 0) ---
  # VPD estimated from min/max temperatures
  VPTMIN <- 0.6108 * exp(17.27 * TMIN / (TMIN + 237.3))
  VPTMAX <- 0.6108 * exp(17.27 * TMAX / (TMAX + 237.3))
  VPD    <- VPDF * (VPTMAX - VPTMIN)   # kPa

  # Transpiration driven by DM production and VPD
  # TEC is in Pa, VPD in kPa → convert: VPD * 1000 / TEC
  TR <- DDMP * VPD / dmp_pars$TEC   # mm/d
  if (TR < 0) TR <- 0

  list(
    DDMP   = DDMP,
    FINT   = FINT,
    RUE    = RUE,
    TCFRUE = TCFRUE,
    TR     = TR,
    VPD    = VPD
  )
}
