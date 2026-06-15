# =============================================================================
# SSM Soybean Model - Dry Matter Distribution & Yield Formation
# =============================================================================
# Partitions daily DM production between vegetative (leaves + stems) and
# reproductive (grain) organs. Implements the Dynamic Harvest Index (DHI)
# approach for grain filling.
#
# Key processes:
#   1. Grain growth rate (SGR) based on dynamic HI during BSG–TSG period
#   2. DM translocation from stems when assimilation is insufficient
#   3. Partitioning of vegetative DM between leaves (FLF) and stems
#   4. Heat and frost effects on DHI (disabled for soybean in test runs)
#
# Reference: Soltani & Sinclair (2012), Chapter 7
# =============================================================================


# =============================================================================
# FUNCTION: init_dm_distribution
# Initializes DM distribution state and parameters.
# Called once at the start of each simulation year.
#
# Args:
#   crop_pars - Named list of crop parameters
#
# Returns:
#   Named list of DM distribution state and parameters
# =============================================================================
init_dm_distribution <- function(crop_pars) {
  FLF1A  <- as.numeric(crop_pars$FLF1A)   # Fraction of net DM → leaves (when WTOP < WTOPL)
  FLF1B  <- as.numeric(crop_pars$FLF1B)   # Fraction of net DM → leaves (when WTOP >= WTOPL)
  WTOPL  <- as.numeric(crop_pars$WTOPL)   # Switch threshold for leaf fraction (g/m²)
  FLF2   <- as.numeric(crop_pars$FLF2)    # Leaf fraction after bdTLM
  FRTRL  <- as.numeric(crop_pars$FRTRL)   # Fraction of BSGDM available for translocation
  GCC    <- as.numeric(crop_pars$GCC)     # Grain construction cost (g structural DM / g glucose)
  PDHI   <- as.numeric(crop_pars$PDHI)    # Potential daily HI increment (g/g per day)
  WDHI1  <- as.numeric(crop_pars$WDHI1)   # WTOP threshold 1 for DHI modifier
  WDHI2  <- as.numeric(crop_pars$WDHI2)   # WTOP threshold 2
  WDHI3  <- as.numeric(crop_pars$WDHI3)   # WTOP threshold 3
  WDHI4  <- as.numeric(crop_pars$WDHI4)   # WTOP threshold 4
  MC     <- as.numeric(crop_pars$MC)      # Grain moisture content at harvest (%)
  heat   <- as.numeric(crop_pars$heat %||% 0)  # Heat/frost effect flag (0 = off)

  list(
    FLF1A = FLF1A, FLF1B = FLF1B, WTOPL = WTOPL, FLF2 = FLF2,
    FRTRL = FRTRL, GCC = GCC, PDHI = PDHI,
    WDHI1 = WDHI1, WDHI2 = WDHI2, WDHI3 = WDHI3, WDHI4 = WDHI4,
    MC = MC, heat = heat,
    # Cumulative state
    WLF    = 0.5,  # Leaf dry mass (g/m²) — initialized at 0.5
    WST    = 0.5,  # Stem dry mass (g/m²) — initialized at 0.5
    WVEG   = 1.0,  # Total vegetative DM (leaves + stems)
    WGRN   = 0,    # Grain dry mass (g/m²)
    WTOP   = 1.0,  # Total above-ground DM
    HI     = 0,    # Harvest Index
    BSGDM  = 0,    # WTOP at beginning of seed growth
    TRLDM  = 0,    # Translocatable DM pool
    DHI    = 0,    # Dynamic harvest index rate
    DHIDMF = 0,    # DHI modifier based on WTOP at BSG
    FrHtDHI= 1,    # Frost/heat reduction factor on DHI
    TRANSL = 0,    # DM translocated from stems today
    SGR    = 0,    # Seed growth rate today (g/m²/d)
    GLF    = 0,    # Leaf DM gain today
    GST    = 0     # Stem DM gain today
  )
}


# =============================================================================
# FUNCTION: step_dm_distribution
# Partitions daily DM between grain and vegetative tissue for one day.
#
# Grain filling algorithm (BSG to TSG):
#   SGR = (DHI * FrHtDHI) * (WTOP + DDMP) + DDMP * HI
#   If SGR requires more DM than produced, translocation from stems occurs.
#
# Args:
#   dmd_state - Named list of DM distribution state
#   state     - Named list: CBD, DDMP, LAI, WSFL
#   bd_thres  - Named list: bdBSG, bdTSG, bdBLG, bdTLM, bdTLP
#
# Returns:
#   Updated dmd_state with WTOP, WGRN, WLF, WST, HI, SGR, GLF, GST, TRANSL
# =============================================================================
step_dm_distribution <- function(dmd_state, state, bd_thres) {
  CBD  <- state$CBD
  DDMP <- state$DDMP
  LAI  <- state$LAI

  WLF    <- dmd_state$WLF
  WST    <- dmd_state$WST
  WVEG   <- dmd_state$WVEG
  WGRN   <- dmd_state$WGRN
  WTOP   <- dmd_state$WTOP
  HI     <- dmd_state$HI
  BSGDM  <- dmd_state$BSGDM
  TRLDM  <- dmd_state$TRLDM
  DHI    <- dmd_state$DHI
  DHIDMF <- dmd_state$DHIDMF
  FrHtDHI<- dmd_state$FrHtDHI

  # -----------------------------------------------------------------------
  # GRAIN GROWTH (BSG to TSG)
  # -----------------------------------------------------------------------
  if (CBD <= bd_thres$bdBSG) {
    # Pre-grain-filling: no seed growth, track WTOP at BSG
    TRANSL <- 0
    SGR    <- 0
    BSGDM  <- WTOP   # continuously update until BSG is reached

    # DHI modifier based on WTOP at BSG (penalty for very low or high biomass)
    if (BSGDM <= dmd_state$WDHI1 || BSGDM >= dmd_state$WDHI4) {
      DHIDMF <- 0
    } else if (BSGDM > dmd_state$WDHI1 && BSGDM < dmd_state$WDHI2) {
      DHIDMF <- (BSGDM - dmd_state$WDHI1) / (dmd_state$WDHI2 - dmd_state$WDHI1)
    } else if (BSGDM > dmd_state$WDHI3 && BSGDM < dmd_state$WDHI4) {
      DHIDMF <- (dmd_state$WDHI4 - BSGDM) / (dmd_state$WDHI4 - dmd_state$WDHI3)
    } else {
      DHIDMF <- 1  # WDHI2 <= BSGDM <= WDHI3
    }

    DHI    <- dmd_state$PDHI * DHIDMF              # potential DHI rate
    TRLDM  <- BSGDM * dmd_state$FRTRL              # translocatable pool

  } else if (CBD > bd_thres$bdBSG && CBD <= bd_thres$bdTSG) {
    # Grain-filling period: daily seed growth rate
    SGR <- (DHI * FrHtDHI) * (WTOP + DDMP) + DDMP * HI
    if (LAI == 0 ) SGR <- 0   # no photosynthate without leaves
    if (SGR < 0) SGR <- 0

    # DM translocation if grain demand exceeds current assimilation
    if ((SGR / dmd_state$GCC) > DDMP) {
      TRANSL <- (SGR / dmd_state$GCC) - DDMP
      if (TRANSL > TRLDM) TRANSL <- TRLDM  # can't exceed translocatable pool
    } else {
      TRANSL <- 0
    }

    TRLDM <- TRLDM - TRANSL
    # Constrain SGR to available DM (assimilation + translocation) × GCC
    if (SGR > (DDMP + TRANSL) * dmd_state$GCC) {
      SGR <- (DDMP + TRANSL) * dmd_state$GCC
    }

  } else {
    # Post-TSG: no more grain growth
    TRANSL <- 0
    SGR    <- 0
  }

  # -----------------------------------------------------------------------
  # DM PARTITIONING TO LEAVES AND STEMS (vegetative pool)
  # -----------------------------------------------------------------------
  # Net DM available for vegetative growth (after seed demand)
  DDMP2 <- DDMP - SGR / dmd_state$GCC
  if (DDMP2 < 0) DDMP2 <- 0

  # Fraction of vegetative DM allocated to leaves
  GLF <- 0
  if (CBD <= bd_thres$bdBLG || CBD > bd_thres$bdTLP) {
    GLF <- 0
  } else if (CBD > bd_thres$bdBLG && CBD <= bd_thres$bdTLM) {
    FLF1 <- if (WTOP < dmd_state$WTOPL) dmd_state$FLF1A else dmd_state$FLF1B
    GLF  <- FLF1 * DDMP2
  } else if (CBD > bd_thres$bdTLM && CBD <= bd_thres$bdTLP) {
    GLF <- dmd_state$FLF2 * DDMP2
  }

  # Remaining vegetative DM goes to stem
  GST <- DDMP2 - GLF

  # -----------------------------------------------------------------------
  # UPDATE ORGAN MASSES
  # -----------------------------------------------------------------------
  WLF  <- WLF + GLF
  WST  <- WST + GST
  WGRN <- WGRN + SGR
  WVEG <- WVEG + DDMP - (SGR / dmd_state$GCC)
  WTOP <- WVEG + WGRN
  HI   <- if (WTOP > 0) WGRN / WTOP else 0

  # Update state
  dmd_state$WLF    <- WLF
  dmd_state$WST    <- WST
  dmd_state$WGRN   <- WGRN
  dmd_state$WVEG   <- WVEG
  dmd_state$WTOP   <- WTOP
  dmd_state$HI     <- HI
  dmd_state$BSGDM  <- BSGDM
  dmd_state$TRLDM  <- TRLDM
  dmd_state$DHI    <- DHI
  dmd_state$DHIDMF <- DHIDMF
  dmd_state$TRANSL <- TRANSL
  dmd_state$SGR    <- SGR
  dmd_state$GLF    <- GLF
  dmd_state$GST    <- GST

  return(dmd_state)
}
