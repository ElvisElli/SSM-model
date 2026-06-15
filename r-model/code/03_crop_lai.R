# =============================================================================
# SSM Soybean Model - Crop LAI Sub-Model (no nitrogen limitation)
# =============================================================================
# Calculates the daily gain (GLAI) and loss (DLAI) in Leaf Area Index (LAI).
# This version is for non-nitrogen-limited conditions (nitrogen = 0).
#
# LAI development is driven by:
#   1. Node production rate based on thermal time and phyllochron (PHYL)
#   2. Leaf area per plant as a power function of node number (PLACON, PLAPOW)
#   3. Plant density (PDEN, plants/m²)
#   4. Water stress on leaf expansion (WSFL)
#   5. Senescence during grain filling period (after bdBLS)
#   6. Frost and heat damage to leaves (if FRZTKIL and HtLTH are set)
#
# Reference: Soltani & Sinclair (2012), Chapter 5
# =============================================================================


# =============================================================================
# FUNCTION: init_crop_lai
# Initializes LAI state variables at the start of a simulation.
# Called once per simulation year before the main daily loop.
#
# Args:
#   crop_pars - Named list of crop parameters
#
# Returns:
#   Named list of initialized LAI state variables
# =============================================================================
init_crop_lai <- function(crop_pars) {
  PHYL    <- as.numeric(crop_pars$PHYL)     # Phyllochron (°C·d per leaf)
  PLACON  <- as.numeric(crop_pars$PLACON)   # Leaf area coefficient
  PLAPOW  <- as.numeric(crop_pars$PLAPOW)   # Leaf area exponent (at std. density)
  a_pla   <- as.numeric(crop_pars$a_pla_den) # Density modifier intercept
  b_pla   <- as.numeric(crop_pars$b_pla_den) # Density modifier slope
  SLA     <- as.numeric(crop_pars$SLA)       # Specific Leaf Area (m²/g)
  FRZTKIL <- as.numeric(crop_pars$FRZTKIL %||% -99)  # Frost kill temperature (°C)
  FRZLDR  <- as.numeric(crop_pars$FRZLDR  %||% 0)   # Frost leaf damage rate
  HtLTH   <- as.numeric(crop_pars$HtLTH   %||% 999) # Heat damage threshold (°C)
  HtLDR   <- as.numeric(crop_pars$HtLDR   %||% 0)   # Heat damage rate
  PDEN    <- as.numeric(crop_pars$pden)     # Plant density (plants/m²)

  # Adjust PLAPOW for plant density effect on leaf area per plant
  PLAPOW_eff <- PLAPOW * (b_pla * PDEN + a_pla)

  list(
    PHYL       = PHYL,
    PLACON     = PLACON,
    PLAPOW_eff = PLAPOW_eff,
    SLA        = SLA,
    FRZTKIL    = FRZTKIL,
    FRZLDR     = FRZLDR,
    HtLTH      = HtLTH,
    HtLDR      = HtLDR,
    # State variables
    MSNN       = 1,    # Main-stem node number (starts at 1)
    PLA1       = 0,    # Previous leaf area per plant (cm²/plant)
    PLA2       = 0,    # Current leaf area per plant (cm²/plant)
    LAI        = 0,    # Leaf Area Index (m²/m²)
    GLAI       = 0,    # Daily LAI gain
    DLAI       = 0,    # Daily LAI loss
    BLSLAI     = 0,    # LAI saved at begin-leaf-senescence stage
    MXLAI      = 0,    # Maximum LAI reached
    WSFL       = 1     # Water stress factor on leaf expansion (initialized to 1)
  )
}


# =============================================================================
# FUNCTION: step_crop_lai
# Advances LAI by one day for non-nitrogen-limited conditions.
#
# Algorithm:
#   1. Apply yesterday's GLAI and DLAI to update today's LAI
#   2. Calculate today's GLAI:
#      - During node production phase (bdBLG to bdTLM):
#          nodes gained = DTU / PHYL
#          leaf area per plant: PLA2 = PLACON * MSNN^PLAPOW
#          GLAI = (PLA2 - PLA1) * PDEN / 10000 * WSFL
#      - During late vegetative (bdTLM to bdTLP): GLAI = GLF * SLA
#      - After bdTLP: no more LAI gain
#   3. Calculate DLAI:
#      - Before bdBLS: no senescence (save LAI as BLSLAI)
#      - After bdBLS: linear decline toward 0 by bdMAT
#   4. Apply frost and heat damage (increase DLAI if needed)
#
# Args:
#   lai_state - Named list of LAI state (from init_crop_lai or previous step)
#   state     - Named list: CBD, DTU, bd, WSFL, TMIN, TMAX
#   GLF       - Dry matter allocated to leaves today (g/m²·d)
#   bd_thres  - Named list of bioday thresholds
#
# Returns:
#   Updated lai_state
# =============================================================================
step_crop_lai <- function(lai_state, state, GLF, bd_thres) {
  CBD    <- state$CBD
  DTU    <- state$DTU
  bd     <- state$bd
  WSFL   <- state$WSFL
  TMIN   <- state$TMIN
  TMAX   <- state$TMAX

  PHYL       <- lai_state$PHYL
  PLACON     <- lai_state$PLACON
  PLAPOW_eff <- lai_state$PLAPOW_eff
  SLA        <- lai_state$SLA
  PDEN       <- state$PDEN   # plant density (plants/m²) from management
  BLSLAI     <- lai_state$BLSLAI

  # --- Step 1: Update LAI from yesterday's gains and losses ---
  LAI <- lai_state$LAI + lai_state$GLAI - lai_state$DLAI
  if (LAI < 0) LAI <- 0

  # Pre-mature senescence check: if LAI drops too low during seed filling
  # (handled in main model loop via MAT flag, not here)
  if (LAI > lai_state$MXLAI) lai_state$MXLAI <- LAI

  # --- Step 2: Calculate GLAI for today ---
  GLAI <- 0
  PLA1 <- lai_state$PLA1
  PLA2 <- lai_state$PLA2
  MSNN <- lai_state$MSNN

  if (CBD <= bd_thres$bdEMR) {
    # Pre-emergence: no leaf expansion
    GLAI <- 0

  } else if (CBD > bd_thres$bdEMR && CBD <= bd_thres$bdTLM) {
    # Main-stem leaf production phase:
    # Thermal time drives node production at rate 1/PHYL per °C·d
    INODE <- DTU / PHYL           # nodes expanded today
    MSNN  <- MSNN + INODE         # cumulative nodes
    PLA2  <- PLACON * MSNN^PLAPOW_eff  # leaf area per plant (cm²/plant)

    # Convert to LAI (m²/m²): multiply by density, divide by 10000 (cm²→m²)
    GLAI  <- ((PLA2 - PLA1) * PDEN / 10000) * WSFL
    if (GLAI < 0) GLAI <- 0
    PLA1  <- PLA2

  } else if (CBD > bd_thres$bdTLM && CBD <= bd_thres$bdTLP) {
    # After main-stem leaf production but before total leaf production ends:
    # LAI gain from other branches, driven by leaf DM allocation
    GLAI <- GLF * SLA

  } else {
    # After bdTLP: no more leaf production
    GLAI <- 0
  }

  # --- Step 3: Calculate DLAI for today (senescence) ---
  DLAI <- 0

  if (CBD < bd_thres$bdBLS) {
    # Before senescence begins: save current LAI as BLSLAI
    DLAI   <- 0
    BLSLAI <- LAI   # update saved LAI at BLS stage

  } else {
    # After bdBLS: linear decline proportional to remaining bioday distance to maturity
    # bd is today's biological day increment; total senescence removes BLSLAI
    # in the interval (bdMAT - bdBLS)
    interval <- bd_thres$bdMAT - bd_thres$bdBLS
    if (interval > 0) {
      DLAI <- (bd / interval) * BLSLAI
    }
  }

  # --- Step 4: Frost damage ---
  DLAIF <- 0
  if (CBD > bd_thres$bdEMR && !is.na(lai_state$FRZTKIL) && TMIN < lai_state$FRZTKIL) {
    frstf <- abs(TMIN - lai_state$FRZTKIL) * lai_state$FRZLDR
    frstf <- max(0, min(1, frstf))
    DLAIF <- LAI * frstf
  }
  if (DLAI < DLAIF) DLAI <- DLAIF

  # --- Step 5: Heat damage (increases senescence rate) ---
  DLAIH <- 0
  if (CBD > bd_thres$bdEMR && !is.na(lai_state$HtLTH) && TMAX > lai_state$HtLTH) {
    # Semenov-Sirius formulation
    heatf <- 1 + (TMAX - lai_state$HtLTH) * lai_state$HtLDR
    if (heatf < 1) heatf <- 1
    DLAIH <- DLAI * heatf
  }
  if (DLAI < DLAIH) DLAI <- DLAIH

  # Update state
  lai_state$LAI    <- LAI
  lai_state$GLAI   <- GLAI
  lai_state$DLAI   <- DLAI
  lai_state$PLA1   <- PLA1
  lai_state$PLA2   <- PLA2
  lai_state$MSNN   <- MSNN
  lai_state$BLSLAI <- BLSLAI

  return(lai_state)
}
