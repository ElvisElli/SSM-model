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
#   4. Two modes controlled by vpdtp parameter:
#      vpdtp=0 (daily): DDMP = SRAD × 0.48 × FINT × RUE; TR = DDMP × VPD / TEC
#      vpdtp=1 (hourly): integrates radiation and VPD over 24 hourly time steps;
#        when hourly VPD exceeds VPDcr, DDMP and TR are reduced by (VPDcr/VPD)²
#        (limited transpiration / LT trait behaviour matching the Excel VBA model)
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
#   crop_pars - Named list of crop parameters (must include $lat for hourly mode)
#   CO2       - Ambient CO2 concentration (ppm)
#   CO2REF    - Reference CO2 concentration (ppm; when experiment was done)
#
# Returns:
#   Named list of DM production parameters (IRUE_adj, TEC_adj, KPAR, LAT, etc.)
# =============================================================================
init_dm_production <- function(crop_pars, CO2 = 420, CO2REF = 385) {
  TBRUE   <- as.numeric(crop_pars$TBRUE)
  TP1RUE  <- as.numeric(crop_pars$TP1RUE)
  TP2RUE  <- as.numeric(crop_pars$TP2RUE)
  TCRUE   <- as.numeric(crop_pars$TCRUE)
  KPAR    <- as.numeric(crop_pars$KPAR)    # Light extinction coefficient
  IRUE    <- as.numeric(crop_pars$IRUE)    # Base RUE (g DM per MJ PAR)
  CO2RES  <- as.numeric(crop_pars$CO2RES)  # CO2 responsiveness of RUE
  TECREF  <- as.numeric(crop_pars$TECREF)  # Reference TEC
  vpdtp   <- as.numeric(crop_pars$vpdtp %||% 0)  # VPD mode: 0=daily, 1=hourly
  VPDcr   <- as.numeric(crop_pars$VPDcr  %||% 2) # Critical VPD (kPa)
  LAT     <- as.numeric(crop_pars$lat    %||% 35) # Latitude for hourly solar calc

  # CO2 adjustment factor
  RUEREF <- 1 * (1 + CO2RES * log10(CO2REF / 330))
  RUECO2 <- 1 * (1 + CO2RES * log10(CO2 / 330))
  co2_factor <- RUECO2 / RUEREF

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
    VPDcr   = VPDcr,
    LAT     = LAT
  )
}


# =============================================================================
# FUNCTION: step_dm_production
# Calculates daily DM production and transpiration for one day.
#
# Daily mode (vpdtp=0):
#   DDMP = SRAD × 0.48 × FINT × RUE         [g DM/m²/d]
#   TR   = DDMP × VPD_daily / TEC            [mm/d]
#
# Hourly mode (vpdtp=1, limited transpiration trait):
#   Integrates over daylight hours H=1..24:
#     - Hourly radiation from Spitters (1986) distribution
#     - Hourly temperature: morning uses TMIN, afternoon declines to next day's TMIN
#     - Hourly VPD = (VP(TEMP1) - VP(TMIN)) × (VPDF / 0.75)
#     - When VPD1 > VPDcr: DDMP1 × (VPDcr/VPD1)² reduction (applied twice per VBA)
#   DDMP = Σ DDMP1_h;  TR = Σ TR1_h
#
# Args:
#   dmp_pars  - Named list from init_dm_production
#   state     - Named list: CBD, LAI, WSFG, TMIN, TMAX, VPDF, TMINA (next-day Tmin)
#   bd_thres  - Named list: bdEMR, bdTSG
#   weather   - Named list: TMIN, TMAX, SRAD, doy
#
# Returns:
#   Named list: DDMP, FINT, RUE, TCFRUE, TR, VPD
# =============================================================================
step_dm_production <- function(dmp_pars, state, bd_thres, weather) {
  TMP   <- (weather$TMAX + weather$TMIN) / 2
  SRAD  <- weather$SRAD
  TMIN  <- weather$TMIN
  TMAX  <- weather$TMAX
  VPDF  <- state$VPDF
  # Next day's Tmin drives the afternoon temperature decline (VBA: TMINA)
  TMINA <- if (!is.null(state$TMINA)) state$TMINA else TMIN

  CBD   <- state$CBD
  LAI   <- state$LAI
  WSFG  <- state$WSFG

  # --- Temperature function for RUE (trapezoidal, same form as phenology) ---
  TCFRUE <- temperature_function(TMP,
                                  dmp_pars$TBRUE, dmp_pars$TP1RUE,
                                  dmp_pars$TP2RUE, dmp_pars$TCRUE)

  # RUE adjusted for temperature and water stress
  RUE <- dmp_pars$IRUE * TCFRUE * WSFG

  # RUE is zero outside the growing period (pre-emergence or post-TSG)
  if (CBD < bd_thres$bdEMR || CBD > bd_thres$bdTSG) RUE <- 0

  # --- PAR interception via Beer's law ---
  FINT <- 1 - exp(-dmp_pars$KPAR * LAI)

  if (dmp_pars$vpdtp == 1) {

    # =========================================================================
    # HOURLY MODE — matches VBA hourly loop exactly
    # Implements the limited transpiration (LT) trait:
    #   when VPD1 > VPDcr, apply (VPDcr/VPD1)^2 reduction to DDMP1 and TR1
    # =========================================================================
    Pi    <- pi
    doy   <- weather$doy
    LAT   <- dmp_pars$LAT

    # Daylength and solar geometry (reuse calc_daylength from 02_phenology.R)
    dl    <- calc_daylength(doy, LAT)
    DAYL  <- dl$DAYL
    SINLD <- dl$SINLD
    COSLD <- dl$COSLD
    AOBs  <- max(-0.9999, min(0.9999, dl$AOB))  # clamped for sqrt stability

    # Daily integral of SINB*(1+0.4*SINB) — used to normalise hourly radiation
    DSINBE <- 3600 * (DAYL * (SINLD + 0.4*(SINLD^2 + COSLD^2*0.5)) +
                      12 * COSLD * (2 + 3*0.4*SINLD) * sqrt(1 - AOBs^2) / Pi)
    if (DSINBE <= 0) DSINBE <- 1e-6  # guard against polar-region edge case

    DTR    <- SRAD * 1e6         # total daily radiation, J m-2 d-1
    P      <- 1.5                # twilight correction parameter (h)
    SUNRIS <- 12 - 0.5 * DAYL
    SUNSET <- 12 + 0.5 * DAYL
    VPTMIN <- 0.6108 * exp(17.27 * TMIN / (237.3 + TMIN))  # VP at today's Tmin (kPa)

    # --- Vectorised hourly summation (matches VBA H=1..24 loop exactly) ---
    Hv       <- 1:24
    daylight <- Hv > SUNRIS & Hv < SUNSET

    angle   <- sin(Pi * (Hv - SUNRIS) / (DAYL + 2*P))
    TEMP1v  <- ifelse(Hv < 13.5,
                      TMIN  + (TMAX  - TMIN)  * angle,
                      TMINA + (TMAX  - TMINA) * angle)

    SINBv  <- pmax(SINLD + COSLD * cos(2 * Pi * (Hv + 12) / 24), 0)
    SRAD1v <- DTR * SINBv * (1 + 0.4*SINBv) / DSINBE * 3600 / 1e6

    DDMP1v   <- SRAD1v * 0.48 * FINT * RUE
    VPTEMP1v <- 0.6108 * exp(17.27 * TEMP1v / (237.3 + TEMP1v))
    VPD1v    <- pmax((VPTEMP1v - VPTMIN) * (VPDF / 0.75), 0)
    TR1v     <- DDMP1v * VPD1v / dmp_pars$TEC

    # Limited transpiration: (VPDcr/VPD1)^2 reduction (two-step, matching VBA)
    lt_idx <- daylight & VPD1v > dmp_pars$VPDcr
    if (any(lt_idx)) {
      VPD1_lt <- VPD1v[lt_idx];  D1 <- DDMP1v[lt_idx]
      VPDcr_  <- dmp_pars$VPDcr; TEC_ <- dmp_pars$TEC
      T1 <- D1 * VPDcr_ / TEC_;  D1 <- T1 * TEC_ / VPD1_lt
      T1 <- D1 * VPDcr_ / TEC_;  D1 <- T1 * TEC_ / VPD1_lt
      DDMP1v[lt_idx] <- D1;  TR1v[lt_idx] <- T1
    }

    DDMP <- sum(DDMP1v[daylight])
    TR   <- max(sum(TR1v[daylight]), 0)
    VPD  <- NA_real_  # not a single value in hourly mode

  } else {

    # =========================================================================
    # DAILY MODE (vpdtp = 0)
    # =========================================================================
    DDMP <- SRAD * 0.48 * FINT * RUE

    VPTMIN <- 0.6108 * exp(17.27 * TMIN / (TMIN + 237.3))
    VPTMAX <- 0.6108 * exp(17.27 * TMAX / (TMAX + 237.3))
    VPD    <- VPDF * (VPTMAX - VPTMIN)   # daily mean VPD estimate (kPa)
    TR     <- DDMP * VPD / dmp_pars$TEC
    if (TR < 0) TR <- 0

  }

  list(
    DDMP   = DDMP,
    FINT   = FINT,
    RUE    = RUE,
    TCFRUE = TCFRUE,
    TR     = TR,
    VPD    = VPD
  )
}
