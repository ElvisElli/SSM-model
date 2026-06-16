# =============================================================================
# SSM Soybean Model - Soil Water Balance Sub-Model
# =============================================================================
# Simulates the daily soil water balance using a 10-layer approach.
#
# Key processes:
#   1. Potential ET (Priestley-Taylor-based Penman method)
#   2. Soil evaporation (stage-1 and stage-2)
#   3. Plant transpiration with water stress
#   4. Runoff (SCS curve number method, water=2 only)
#   5. Drainage between layers (cascade model)
#   6. Root depth growth and layer-weighted root uptake
#   7. Water stress factors for phenology, DM, leaf expansion
#   8. Auto-irrigation (water=1) or rainfed+runoff (water=2)
#
# Reference: Soltani & Sinclair (2012), Chapter 8-9
# =============================================================================


# =============================================================================
# FUNCTION: init_soil_water
# Initializes soil water state for all layers at the start of simulation.
# Called once per simulation year.
#
# Args:
#   soil     - Named list from read_soil() with $meta and $layers
#   crop_pars- Named list of crop parameters
#   CO2      - Ambient CO2 (ppm)
#   CO2REF   - Reference CO2 (ppm)
#
# Returns:
#   Named list with all soil water state variables
# =============================================================================
init_soil_water <- function(soil, crop_pars, CO2 = 420, CO2REF = 385, init_ftswrz = 0) {
  # Crop water parameters
  DEPORT  <- as.numeric(crop_pars$DEPORT)   # Initial root depth (mm)
  MEED    <- as.numeric(crop_pars$MEED)     # Maximum effective root depth (mm)
  GRTDP   <- as.numeric(crop_pars$GRTDP)   # Root depth growth rate (mm/bd)
  TECREF  <- as.numeric(crop_pars$TECREF)   # Reference TEC (Pa)
  CO2RES  <- as.numeric(crop_pars$CO2RES)   # CO2 response for RUE/TEC
  WSSG    <- as.numeric(crop_pars$WSSG)     # FTSW threshold: grain growth
  WSSL    <- as.numeric(crop_pars$WSSL)     # FTSW threshold: leaf expansion
  WSSD    <- as.numeric(crop_pars$WSSD)     # Phenology acceleration factor
  WSSN    <- as.numeric(crop_pars$WSSN)     # FTSW threshold: N uptake
  FLDKIL  <- as.numeric(crop_pars$FLDKIL)  # Flood kill duration (d)
  vpdtp   <- as.numeric(crop_pars$vpdtp %||% 0)
  VPDcr   <- as.numeric(crop_pars$VPDcr  %||% 2)
  surviv  <- as.numeric(crop_pars$surviv  %||% 0)
  EPCOND  <- as.numeric(crop_pars$EPCOND  %||% 0.1)
  LTLRWC  <- as.numeric(crop_pars$LTLRWC  %||% 0.55)

  # CO2 adjustment for TEC
  RUEREF <- 1 + as.numeric(CO2RES) * log10(as.numeric(CO2REF) / 330)
  RUECO2 <- 1 + as.numeric(CO2RES) * log10(as.numeric(CO2) / 330)
  TEC    <- TECREF * (RUECO2 / RUEREF)

  # Soil profile metadata
  NLYER  <- as.integer(soil$meta$NLYER)
  LDRAIN <- as.integer(soil$meta$LDRAIN)  # index of drainage layer (0 = last layer)
  if (LDRAIN == 0) LDRAIN <- NLYER        # 0 means the bottom layer
  SALB   <- soil$meta$SALB
  CN     <- soil$meta$CN2

  # Layer initialization
  layers <- soil$layers
  DLYER  <- layers$DLYER       # Layer thickness (mm)
  SAT    <- layers$SAT         # Saturation vol/vol
  DUL    <- layers$DUL         # Drained upper limit (field capacity) vol/vol
  CLL    <- layers$LL          # Lower limit (wilting point) vol/vol
  ADRY   <- layers$ADRY        # Air-dry vol/vol
  iWL    <- layers$iWL         # Initial water content vol/vol
  DRAINF <- layers$DRAINF      # Drainage coefficient (fraction per day)

  # Convert vol/vol to mm water per layer
  WL    <- iWL  * DLYER   # current water per layer (mm) — layers always start at iWL (DUL)
  WLAD  <- ADRY * DLYER   # air-dry water
  WLLL  <- CLL  * DLYER   # lower limit water
  WLUL  <- DUL  * DLYER   # upper limit (field capacity)
  WLST  <- SAT  * DLYER   # saturation water

  # Available transpirable soil water
  ATSW <- pmax(WL - WLLL, 0)
  TTSW <- WLUL - WLLL           # total transpirable soil water per layer
  FTSW <- pmin(ATSW / pmax(TTSW, 1e-8), 1)

  # Root uptake potential per layer (0-1, based on FTSW vs threshold)
  RT <- ifelse(FTSW > WSSG, 1, FTSW / WSSG)
  RT <- pmax(RT, 0)

  # Profile totals at sowing
  SOLDEP <- sum(DLYER)     # total soil depth (mm)
  WSOL   <- sum(WL)        # total soil water
  ATSWSL <- sum(ATSW)      # total available soil water (profile)

  list(
    # Soil profile parameters
    NLYER = NLYER, LDRAIN = LDRAIN, SALB = SALB, CN = CN,
    DLYER = DLYER, WLAD = WLAD, WLLL = WLLL, WLUL = WLUL, WLST = WLST,
    DRAINF = DRAINF, SOLDEP = SOLDEP,
    # Crop water parameters
    DEPORT = DEPORT, MEED = MEED, GRTDP = GRTDP, TEC = TEC,
    WSSG = WSSG, WSSL = WSSL, WSSD = WSSD, WSSN = WSSN,
    FLDKIL = FLDKIL, vpdtp = vpdtp, VPDcr = VPDcr,
    surviv = surviv, EPCOND = EPCOND, LTLRWC = LTLRWC,
    # Constants
    EOSMIN = 1.5,   # minimum soil evaporation stage-2 (mm)
    WETWAT = 10,    # precipitation threshold for resetting evaporation stage (mm)
    KET    = 0.5,   # extinction coefficient for soil evaporation
    CALB   = 0.23,  # crop albedo
    # Layer state
    WL   = WL,      # current water content (mm/layer)
    ATSW = ATSW, TTSW = TTSW, FTSW = FTSW, RT = RT,
    FLOUT = rep(0, NLYER),  # drainage outflow per layer
    RLYER = rep(0, NLYER),  # root length in each layer (mm)
    # Saved initial values (at sowing)
    iATSW = ATSW, iFTSW = FTSW,
    # Cumulative balances
    CTR = 0, CE = 0, CRAIN = 0, CRUNOF = 0, CIRGW = 0, CDRAIN = 0, IRGNO = 0,
    # Daily outputs
    TR = 0, SEVP = 0, IRGW = 0, RUNOF = 0, DRAIN = 0, PET = 0,
    # Profile sums
    WSOL = WSOL, ATSWSL = ATSWSL, ISOLWAT = ATSWSL,
    # Root and water stress
    DEPORT = DEPORT, RTLN = 1L, AROOT = 1,
    WSFL = 1, WSFG = 1, WSFD = 1, WSFN = 1, WSXF = 1,
    # Root zone water: FTSWRZ initialized from the shallow root zone of the previous
    # year's final layer state (VBA carry-over: < 0.5 → stage-2; ≥ 0.5 → stage-1)
    ATSWRZ = 0, TTSWRZ = 0, FTSWRZ = init_ftswrz,
    WRZ = 0, WRZUL = 0, WRZST = 0,
    # Evaporation day counter: always reset to 1 at start of each season
    DYSE = 1,
    # Flood duration
    FLDUR = 0,
    # BLSLAI (needed for soil evaporation LAI logic)
    BLSLAI = 0
  )
}


# =============================================================================
# FUNCTION: step_soil_water
# Advances the soil water balance by one day.
#
# Args:
#   sw        - Soil water state (from init_soil_water or previous step)
#   state     - Named list: CBD, DAP, bd, DDMP, LAI, BLSLAI, VPDF
#   bd_thres  - Named list: bdEMR, bdBSG, bdTSG, bdBRG, bdTRG
#   weather   - Named list: RAIN, TMIN, TMAX, SRAD
#   water_mgmt- Integer: 1=auto irrigation, 2=rainfed+runoff, 3=scheduled
#   IRGLVL    - FTSW threshold for auto irrigation trigger (water=1)
#   manag_pars- Named list with irrigation schedule (for water=3)
#
# Returns:
#   Updated sw state list
# =============================================================================
step_soil_water <- function(sw, state, bd_thres, weather,
                             water_mgmt = 2, IRGLVL = 0.5, manag_pars = NULL) {
  RAIN  <- weather$RAIN
  TMIN  <- weather$TMIN
  TMAX  <- weather$TMAX
  SRAD  <- weather$SRAD
  CBD   <- state$CBD
  DAP   <- state$DAP
  bd    <- state$bd
  DDMP  <- state$DDMP
  LAI   <- state$LAI
  VPDF  <- state$VPDF
  BLSLAI<- state$BLSLAI

  NLYER  <- sw$NLYER
  LDRAIN <- sw$LDRAIN

  # ----------------------------------------------------------
  # IRRIGATION
  # ----------------------------------------------------------
  IRGW <- 0
  if (water_mgmt == 1) {
    # Auto-irrigation: refill to field capacity when FTSW drops below threshold
    if (sw$FTSWRZ <= IRGLVL && CBD > 0 && CBD < bd_thres$bdTSG) {
      IRGW   <- sw$TTSWRZ - sw$ATSWRZ
      if (IRGW < 0) IRGW <- 0
      sw$IRGNO <- sw$IRGNO + 1L
    }
  } else if (water_mgmt == 2) {
    IRGW <- 0   # rainfed
  } else if (water_mgmt == 3 && !is.null(manag_pars)) {
    # Scheduled irrigation (DAP, CBD, or DOY based) — simplified here
    IRGW <- 0   # extend as needed for scheduled runs
  }
  sw$IRGW   <- IRGW
  sw$CIRGW  <- sw$CIRGW + IRGW

  # ----------------------------------------------------------
  # DRAINAGE from previous day
  # ----------------------------------------------------------
  DRAIN     <- sw$FLOUT[LDRAIN]
  sw$DRAIN  <- DRAIN
  sw$CDRAIN <- sw$CDRAIN + DRAIN

  # ----------------------------------------------------------
  # ROOT DEPTH GROWTH
  # ----------------------------------------------------------
  GRTD <- sw$GRTDP * bd
  # Root growth only between bdBRG and bdTRG, when there is DM production,
  # depth hasn't reached soil depth or max effective depth,
  # and there's water available in the current rooting layer
  if (CBD < bd_thres$bdBRG || CBD > bd_thres$bdTRG) GRTD <- 0
  if (DDMP == 0) GRTD <- 0
  if (sw$DEPORT >= sw$SOLDEP) GRTD <- 0
  if (sw$DEPORT >= sw$MEED)   GRTD <- 0
  if (sw$ATSW[sw$RTLN] == 0)  GRTD <- 0
  sw$DEPORT <- sw$DEPORT + GRTD

  # Determine which layers have roots (proportionally by depth)
  DPTOP <- 0
  RTLN  <- 0L
  for (L in seq_len(NLYER)) {
    RLYER_L <- sw$DEPORT - DPTOP
    RLYER_L <- min(RLYER_L, sw$DLYER[L])
    RLYER_L <- max(RLYER_L, 0)
    sw$RLYER[L] <- RLYER_L
    if (RLYER_L > 0) RTLN <- L
    DPTOP <- DPTOP + sw$DLYER[L]
  }
  sw$RTLN <- RTLN

  # ----------------------------------------------------------
  # RUNOFF (SCS curve number, rainfed mode only)
  # ----------------------------------------------------------
  RUNOF <- 0
  if (water_mgmt == 2 && RAIN > 0.01) {
    S <- 254 * (100 / sw$CN - 1)   # maximum soil retention (mm)
    if (sw$DLYER[1] >= 250) {
      # Runoff based on top layer only
      SWER <- 0.15 * ((sw$WLST[1] - sw$WL[1]) / max(sw$WLST[1] - sw$WLLL[1], 1e-8))
    } else {
      # Runoff based on top 2 layers (each weighted 50%)
      SWER <- 0.15 * (0.5 * (sw$WLST[1] - sw$WL[1]) / max(sw$WLST[1] - sw$WLLL[1], 1e-8) +
                      0.5 * (sw$WLST[2] - sw$WL[2]) / max(sw$WLST[2] - sw$WLLL[2], 1e-8))
    }
    SWER <- max(0, SWER)
    if ((RAIN - SWER * S) > 0) {
      RUNOF <- (RAIN - SWER * S)^2 / (RAIN + (1 - SWER) * S)
    }
  }
  sw$RUNOF  <- RUNOF
  sw$CRAIN  <- sw$CRAIN + RAIN
  sw$CRUNOF <- sw$CRUNOF + RUNOF

  # ----------------------------------------------------------
  # POTENTIAL ET (Priestley-Taylor via BEACHELL-PENMAN)
  # ----------------------------------------------------------
  # LAI for soil evaporation: use canopy LAI before BSG, then BLSLAI after
  ETLAI  <- if (CBD <= bd_thres$bdBSG) LAI else BLSLAI
  TD     <- 0.6 * TMAX + 0.4 * TMIN   # degree-day temperature
  ALBEDO <- sw$CALB * (1 - exp(-sw$KET * ETLAI)) + sw$SALB * exp(-sw$KET * ETLAI)
  EEQ    <- SRAD * (0.004876 - 0.004374 * ALBEDO) * (TD + 29)   # equilibrium ET
  PET    <- EEQ * 1.1                                             # Penman PET
  if (TMAX > 34) PET <- EEQ * ((TMAX - 34) * 0.05 + 1.1)
  if (TMAX < 5)  PET <- EEQ * 0.01 * exp(0.18 * (TMAX + 20))
  sw$PET <- PET

  # ----------------------------------------------------------
  # SOIL EVAPORATION (2-stage model)
  # ----------------------------------------------------------
  EOS <- PET * exp(-sw$KET * ETLAI)
  if (PET > sw$EOSMIN && EOS < sw$EOSMIN) EOS <- sw$EOSMIN

  SEVP <- EOS
  # Reset to stage 1 after rain or irrigation wets the soil
  if ((RAIN + IRGW) > sw$WETWAT) sw$DYSE <- 1
  if (sw$DYSE > 1 || sw$FTSWRZ < 0.5 || sw$ATSW[1] <= 1) {
    # Stage-2 evaporation: declining rate with DYSE
    SEVP    <- EOS * (sqrt(sw$DYSE + 1) - sqrt(sw$DYSE))
    sw$DYSE <- sw$DYSE + 1
  }
  sw$SEVP <- SEVP
  sw$CE   <- sw$CE + SEVP

  # ----------------------------------------------------------
  # PLANT TRANSPIRATION
  # TR is computed in step_dm_production for both daily and hourly VPD modes.
  # Using it here ensures the LT trait (vpdtp=1) correctly reduces water uptake.
  # ----------------------------------------------------------
  TR <- if (!is.null(state$TR)) state$TR else 0
  if (TR < 0) TR <- 0
  sw$TR  <- TR
  sw$CTR <- sw$CTR + TR

  # ----------------------------------------------------------
  # WATER UPTAKE DISTRIBUTION ACROSS LAYERS
  # ----------------------------------------------------------
  AROOT <- sw$AROOT
  WUUR  <- TR / (AROOT + 1e-8)  # water uptake per unit root area

  WU <- rep(0, NLYER)
  SE <- rep(0, NLYER)
  TSE <- SEVP

  for (L in seq_len(NLYER)) {
    WU[L] <- sw$RLYER[L] * sw$RT[L] * WUUR
    # Soil evaporation distributed over layers starting from top
    SE[L] <- TSE
    max_se <- (sw$WL[L] - sw$WLAD[L]) * sw$DRAINF[L]
    if (SE[L] > max_se) SE[L] <- max_se
    if (sw$WL[L] <= sw$WLAD[L]) SE[L] <- 0
    TSE <- max(TSE - SE[L], 0)
  }

  # ----------------------------------------------------------
  # SOIL WATER BALANCE UPDATE (layer by layer)
  # ----------------------------------------------------------
  WRZ <- 0; WRZUL <- 0; WRZST <- 0; ATSWRZ <- 0; TTSWRZ <- 0
  WSOL <- 0; ATSWSL <- 0; TTSWSL <- 0; AROOT <- 0
  FLOUT <- rep(0, NLYER)

  for (L in seq_len(NLYER)) {
    # Inflow: rainfall+irrigation-runoff to top layer; drainage from layer above otherwise
    FLIN_L <- if (L == 1) max(RAIN + IRGW - RUNOF, 0) else max(FLOUT[L - 1], 0)

    # Water balance
    sw$WL[L] <- sw$WL[L] + FLIN_L - WU[L] - SE[L]

    # Drainage outflow (excess above field capacity)
    FLOUT_L  <- (sw$WL[L] - sw$WLUL[L]) * sw$DRAINF[L]
    if (FLOUT_L < 0) FLOUT_L <- 0
    sw$WL[L] <- sw$WL[L] - FLOUT_L
    FLOUT[L] <- FLOUT_L

    # Update ATSW, TTSW, FTSW
    sw$ATSW[L] <- max(sw$WL[L] - sw$WLLL[L], 0)
    sw$TTSW[L] <- sw$WLUL[L] - sw$WLLL[L]
    sw$FTSW[L] <- sw$ATSW[L] / max(sw$TTSW[L], 1e-8)

    # Root uptake stress factor per layer
    sw$RT[L] <- if (sw$FTSW[L] > sw$WSSG) 1 else sw$FTSW[L] / sw$WSSG
    sw$RT[L] <- max(sw$RT[L], 0)

    # Root-zone weighted sums (only where roots exist)
    root_frac <- sw$RLYER[L] / max(sw$DLYER[L], 1e-8)
    AROOT    <- AROOT   + sw$RLYER[L] * sw$RT[L]
    WRZ      <- WRZ     + sw$WL[L]    * root_frac
    WRZUL    <- WRZUL   + sw$WLUL[L]  * root_frac
    WRZST    <- WRZST   + sw$WLST[L]  * root_frac
    ATSWRZ   <- ATSWRZ  + sw$ATSW[L]  * root_frac
    TTSWRZ   <- TTSWRZ  + sw$TTSW[L]  * root_frac

    # Full profile sums
    WSOL   <- WSOL   + sw$WL[L]
    ATSWSL <- ATSWSL + sw$ATSW[L]
    TTSWSL <- TTSWSL + sw$TTSW[L]
  }

  sw$FLOUT  <- FLOUT
  sw$AROOT  <- if (AROOT > 0) AROOT else 1
  sw$WRZ    <- WRZ; sw$WRZUL <- WRZUL; sw$WRZST <- WRZST
  sw$ATSWRZ <- ATSWRZ; sw$TTSWRZ <- TTSWRZ
  sw$FTSWRZ <- if (TTSWRZ > 0) ATSWRZ / TTSWRZ else 0
  sw$WSOL   <- WSOL; sw$ATSWSL <- ATSWSL; sw$TTSWSL <- TTSWSL
  sw$FTSWSL <- if (TTSWSL > 0) ATSWSL / TTSWSL else 0

  # ----------------------------------------------------------
  # WATER STRESS FACTORS
  # ----------------------------------------------------------
  sw$WSFL <- if (sw$FTSWRZ > sw$WSSL) 1 else sw$FTSWRZ / sw$WSSL
  sw$WSFG <- if (sw$FTSWRZ > sw$WSSG) 1 else sw$FTSWRZ / sw$WSSG
  sw$WSFN <- if (sw$FTSWRZ > sw$WSSN) 1 else sw$FTSWRZ / (sw$WSSN + 1e-4)

  # Water-deficit phenology acceleration (WSFD > 1 means faster development under drought)
  sw$WSFD <- (1 - sw$WSFG) * sw$WSSD + 1

  # Excess water stress factor (flooding)
  sw$WSXF <- if (WRZ <= WRZUL) 1 else (WRZST - WRZ) / max(WRZST - WRZUL, 1e-8)
  if (sw$WSXF < 0) sw$WSXF <- 0

  # ----------------------------------------------------------
  # FLOOD KILL CHECK
  # ----------------------------------------------------------
  if (sw$WSXF <= 0.02) {
    sw$FLDUR <- sw$FLDUR + 1
  } else {
    sw$FLDUR <- 0
  }

  return(sw)
}
