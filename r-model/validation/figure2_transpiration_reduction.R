# =============================================================================
# Figure 2 — Transpiration Reduction from the Limited-Transpiration Trait
#
# Computes the daily transpiration reduction (mm/day) caused by the LT trait
# (VPDcr = 2.0 kPa) relative to the check cultivar, using:
#   - Hourly VPD from sinusoidal T interpolation (exact model formula)
#   - Hourly SRAD from solar geometry (exact model formula)
#   - FINT from actual model daily outputs (DOY-mean across years per location)
#   - CO2-adjusted RUE and TEC from scenario parameters
#
# Reduction per hour h (daylight, VPD1[h] > VPDcr):
#   TR_orig[h] = SRAD1[h] * 0.48 * FINT * RUE * VPD1[h] / TEC   [mm/h]
#   TR_lt[h]   = SRAD1[h] * 0.48 * FINT * RUE * VPDcr^2 /
#                (VPD1[h] * TEC)                                   [mm/h]
#   delta_TR[h] = TR_orig[h] - TR_lt[h]
#
# Daily reduction = sum_h(delta_TR[h])  [mm/day], averaged across 1985-2025.
#
# Output: r-model/outputs/plots/figure2_transpiration_reduction.tif
#
# Usage:
#   Rscript r-model/validation/figure2_transpiration_reduction.R
# =============================================================================

# --- packages ----------------------------------------------------------------
needed <- c("readxl", "ggplot2", "RColorBrewer")
miss   <- needed[!sapply(needed, requireNamespace, quietly = TRUE)]
if (length(miss) > 0) install.packages(miss, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(readxl); library(ggplot2); library(RColorBrewer)
})

# --- paths -------------------------------------------------------------------
if (!exists("BASE_DIR") || !dir.exists(BASE_DIR)) {
  script_path <- tryCatch({
    sp <- NULL
    for (i in seq_len(sys.nframe())) {
      of <- sys.frame(i)$ofile
      if (!is.null(of) && nchar(of) > 0) { sp <- normalizePath(of, mustWork=FALSE); break }
    }
    if (is.null(sp)) {
      args <- commandArgs(trailingOnly = FALSE)
      ff   <- grep("^--file=", args, value = TRUE)
      if (length(ff) > 0) sp <- normalizePath(sub("^--file=", "", ff[1]), mustWork=FALSE)
    }
    sp
  }, error = function(e) NULL)
  BASE_DIR <- if (!is.null(script_path)) {
    d <- dirname(script_path)
    if (basename(d) == "validation") dirname(d) else d
  } else {
    cwd <- getwd()
    if      (file.exists(file.path(cwd, "inputs/scenarios.csv")))         cwd
    else if (file.exists(file.path(cwd, "r-model/inputs/scenarios.csv"))) file.path(cwd, "r-model")
    else stop("Cannot locate r-model base directory. Set BASE_DIR before sourcing.")
  }
}

WTH_DIR   <- file.path(BASE_DIR, "inputs", "weather")
DAILY_DIR <- file.path(BASE_DIR, "outputs", "results", "daily")
OUT_DIR   <- file.path(BASE_DIR, "outputs", "plots")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
OUT_FILE  <- file.path(OUT_DIR, "figure2_transpiration_reduction.tif")

cat("Base directory:", BASE_DIR, "\n")
cat("Output:", OUT_FILE, "\n\n")

# --- model parameters (from scenarios.csv, check cultivar) ------------------
VPDF   <- 0.75
VPDcr  <- 2.0
RUE    <- 2.0    # g DM / MJ PAR  (IRUE)
KPAR   <- 0.65
TECREF <- 4.5    # g DM / mm / kPa (reference)
CO2    <- 420    # ambient CO2 (ppm)
CO2REF <- 385    # reference CO2 (ppm)
CO2RES <- 0.8    # CO2 responsiveness

# CO2-adjusted TEC
RUEREF <- 1 + CO2RES * log10(CO2REF / 330)
RUECO2 <- 1 + CO2RES * log10(CO2    / 330)
TEC    <- TECREF * (RUECO2 / RUEREF)
cat(sprintf("CO2-adjusted TEC: %.3f g/mm/kPa  (TECREF=%.1f, CO2=%d ppm)\n\n",
            TEC, TECREF, CO2))

DOY_MIN <- 91    # April 1
DOY_MAX <- 273   # September 30

# --- helper functions --------------------------------------------------------
es <- function(T) 0.6108 * exp(17.27 * T / (237.3 + T))

calc_dayl <- function(doy, LAT) {
  Pi <- pi; RDN <- Pi / 180
  DEC <- atan(sin(23.45*RDN) * cos(2*Pi*(doy+10)/365) /
              sqrt(1 - (sin(23.45*RDN)*cos(2*Pi*(doy+10)/365))^2)) * (-1)
  SINLD <- sin(RDN*LAT) * sin(DEC)
  COSLD <- cos(RDN*LAT) * cos(DEC)
  AOBs  <- pmax(-0.9999, pmin(0.9999, SINLD/COSLD))
  DAYL  <- 12 * (1 + 2 * atan(AOBs / sqrt(1 - AOBs^2)) / Pi)
  list(DAYL=DAYL, SINLD=SINLD, COSLD=COSLD, AOBs=AOBs)
}

tr_reduction_mm <- function(TMAX, TMIN, TMINA, SRAD, DAYL, FINT,
                             SINLD, COSLD, AOBs,
                             VPDcr, VPDF, RUE, TEC) {
  Pi <- pi; P <- 1.5
  SUNRIS <- 12 - 0.5*DAYL
  SUNSET <- 12 + 0.5*DAYL
  Hv     <- 1:24
  dl     <- Hv > SUNRIS & Hv < SUNSET

  # Hourly temperature (exact model formula)
  angle <- sin(Pi * (Hv - SUNRIS) / (DAYL + 2*P))
  TEMP  <- ifelse(Hv < 13.5,
                  TMIN  + (TMAX  - TMIN)  * angle,
                  TMINA + (TMAX  - TMINA) * angle)

  # Hourly VPD
  VPD1 <- pmax((es(TEMP) - es(TMIN)) * (VPDF / 0.75), 0)

  # Hourly SRAD (exact model formula)
  DSINBE <- 3600 * (DAYL * (SINLD + 0.4*(SINLD^2 + COSLD^2*0.5)) +
                    12 * COSLD * (2 + 3*0.4*SINLD) * sqrt(1 - AOBs^2) / Pi)
  if (DSINBE <= 0) DSINBE <- 1e-6
  DTR   <- SRAD * 1e6
  SINBv <- pmax(SINLD + COSLD * cos(2*Pi*(Hv+12)/24), 0)
  SRAD1 <- DTR * SINBv * (1 + 0.4*SINBv) / DSINBE * 3600 / 1e6

  # Hourly transpiration without and with LT trait
  TR_orig <- SRAD1 * 0.48 * FINT * RUE * VPD1 / TEC
  # For hours with VPD1 > VPDcr: TR is reduced by (VPDcr/VPD1)^2
  TR_lt   <- ifelse(VPD1 > VPDcr,
                    SRAD1 * 0.48 * FINT * RUE * VPDcr^2 / (VPD1 * TEC),
                    TR_orig)

  # Sum only over daylight hours
  sum((TR_orig - TR_lt)[dl], na.rm = TRUE)
}

# --- location table ----------------------------------------------------------
locs <- data.frame(
  wth_file   = c("SSM_Rowher_AR.xlsx",      "SSM_Marianna_AR.xlsx",
                 "SSM_Keiser_AR.xlsx",       "SSM_Jonesboro_AR.xlsx",
                 "SSM_MountVernon_MO.xlsx",  "SSM_Novelty_MO.xlsx",
                 "SSM_Albany_MO.xlsx",       "SSM_Eustis_NE.xlsx",
                 "SSM_Lincoln_NE.xlsx",      "SSM_NorthPlatte_NE.xlsx"),
  daily_file = c("RW-RFD-MID-check_daily.csv", "MA-RFD-MID-check_daily.csv",
                 "KS-RFD-MID-check_daily.csv", "JB-RFD-MID-check_daily.csv",
                 "MV-RFD-MID-check_daily.csv", "NV-RFD-MID-check_daily.csv",
                 "AL-RFD-MID-check_daily.csv", "EU-RFD-MID-check_daily.csv",
                 "LN-RFD-MID-check_daily.csv", "NP-RFD-MID-check_daily.csv"),
  label      = c("Rohwer, AR",       "Marianna, AR",
                 "Keiser, AR",       "Jonesboro, AR",
                 "Mount Vernon, MO", "Novelty, MO",
                 "Albany, MO",       "Eustis, NE",
                 "Lincoln, NE",      "North Platte, NE"),
  lat        = c(33.8, 34.7, 35.7, 35.8, 37.1, 40.0, 40.2, 40.5, 40.8, 41.0),
  stringsAsFactors = FALSE
)

# --- compute TR reduction per DOY per location -------------------------------
cat("Computing TR reduction (mm/day)...\n")
all_rows <- list()

for (i in seq_len(nrow(locs))) {
  loc <- locs[i, ]
  cat(sprintf("  %s\n", loc$label))

  # Weather data
  wth <- read_xlsx(file.path(WTH_DIR, loc$wth_file), skip = 10,
                   col_names  = c("YEAR","DOY","SRAD","TMAX","TMIN","RAIN"),
                   col_types  = rep("numeric", 6))
  wth <- wth[!is.na(wth$YEAR) & wth$DOY >= DOY_MIN & wth$DOY <= DOY_MAX, ]
  wth$TMINA <- c(wth$TMIN[-1], NA)
  wth$TMINA[is.na(wth$TMINA)] <- wth$TMIN[is.na(wth$TMINA)]

  # Mean FINT by DOY from model daily output
  dout      <- read.csv(file.path(DAILY_DIR, loc$daily_file))
  dout      <- dout[dout$doy >= DOY_MIN & dout$doy <= DOY_MAX, ]
  fint_mean <- aggregate(FINT ~ doy, data = dout, FUN = mean)
  names(fint_mean) <- c("DOY", "FINT_mean")

  # Pre-compute solar geometry per unique DOY
  doys_u  <- sort(unique(wth$DOY))
  dayl_lu <- lapply(doys_u, function(d) calc_dayl(d, loc$lat))
  names(dayl_lu) <- doys_u

  # Merge FINT into weather; DOYs outside crop season get FINT=0
  wth <- merge(wth, fint_mean, by = "DOY", all.x = TRUE)
  wth$FINT_mean[is.na(wth$FINT_mean)] <- 0

  # Compute TR reduction for every row
  wth$red_mm <- mapply(function(TMAX, TMIN, TMINA, SRAD, DOY, FINT) {
    dl <- dayl_lu[[as.character(DOY)]]
    if (is.null(dl) || FINT <= 0) return(0)
    tr_reduction_mm(TMAX, TMIN, TMINA, SRAD, dl$DAYL, FINT,
                    dl$SINLD, dl$COSLD, dl$AOBs,
                    VPDcr, VPDF, RUE, TEC)
  }, wth$TMAX, wth$TMIN, wth$TMINA, wth$SRAD, wth$DOY, wth$FINT_mean)

  # Mean across years per DOY
  agg          <- aggregate(red_mm ~ DOY, data = wth, FUN = mean)
  agg$location <- loc$label
  all_rows[[i]] <- agg
}

df          <- do.call(rbind, all_rows)
df$location <- factor(df$location, levels = rev(locs$label))

# Inspect range to set bins
cat(sprintf("\nReduction range: %.3f – %.3f mm/day\n",
            min(df$red_mm), max(df$red_mm)))
cat("Quantiles (50, 75, 90, 95%):",
    round(quantile(df$red_mm[df$red_mm > 0.005],
                   c(.50,.75,.90,.95)), 3), "\n\n")

# --- 4 categories ------------------------------------------------------------
breaks   <- c(-Inf, 0.10, 0.75, 1.75, Inf)
cat_labs <- c("< 0.1 mm", "0.1 to 0.75 mm", "0.75 to 1.75 mm", "> 1.75 mm")
df$red_cat <- cut(df$red_mm, breaks = breaks, labels = cat_labs, right = TRUE)

# BuGn palette — same family as Figure 1 for visual consistency
bugu_cols  <- RColorBrewer::brewer.pal(4, "BuGn")
cat_colors <- c(
  "< 0.1 mm"       = "#D0D0D0",    # grey — negligible / pre-canopy
  "0.1 to 0.75 mm" = bugu_cols[2], # light blue-green
  "0.75 to 1.75 mm" = bugu_cols[3], # teal
  "> 1.75 mm"      = bugu_cols[4]  # dark green
)

# --- plot elements -----------------------------------------------------------
month_mid <- c(105, 135, 166, 196, 227, 258)
month_lab <- c("Apr","May","Jun","Jul","Aug","Sep")
month_sep <- c(121, 152, 182, 213, 244)

p <- ggplot(df, aes(x = DOY, y = location, fill = red_cat)) +
  annotate("rect", xmin=DOY_MIN, xmax=DOY_MAX,
           ymin=0.5, ymax=10.5, fill="#fafafa", alpha=1) +
  geom_tile(height = 0.88) +
  geom_vline(xintercept = month_sep, color = "white",
             linewidth = 0.4, alpha = 0.9) +
  geom_hline(yintercept = c(4.5, 7.5), color = "white", linewidth = 1.5) +
  scale_fill_manual(
    values = cat_colors,
    name   = "TR reduction\n(mm/day)",
    drop   = FALSE
  ) +
  scale_x_continuous(
    breaks = month_mid, labels = month_lab,
    expand = c(0.005, 0)
  ) +
  scale_y_discrete(expand = expansion(add = c(0.5, 0.5))) +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 10) +
  theme(
    panel.grid        = element_blank(),
    panel.border      = element_rect(color = "grey60", linewidth = 0.5),
    legend.position   = "right",
    legend.key.size   = unit(0.55, "cm"),
    legend.key        = element_rect(color = "grey70", linewidth = 0.3),
    legend.title      = element_text(size = 8.5),
    legend.text       = element_text(size = 8.5),
    axis.text.y       = element_text(size = 8.5, color = "grey15"),
    axis.text.x       = element_text(size = 8.5, color = "grey15"),
    axis.ticks        = element_line(color = "grey60"),
    plot.margin       = margin(4, 4, 4, 4)
  )

# --- save (TIFF, 19 cm wide, 600 dpi) ---------------------------------------
ggsave(OUT_FILE, p, width = 7.5, height = 3.8, units = "in",
       dpi = 600, device = "tiff", compression = "lzw")
cat(sprintf("Figure saved: %s  (%.1f cm wide, 600 dpi TIFF)\n",
            OUT_FILE, 7.5 * 2.54))
