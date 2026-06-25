# =============================================================================
# Figure 1 — Environmental Characterization
# Limited-Transpiration Trait Activation across Locations and Season
#
# Shows the mean daily hours when the model's estimated hourly VPD exceeds
# the critical threshold (VPDcr = 2.0 kPa) that triggers the limited-
# transpiration (LT) trait in soybean. Values are averaged across 1985-2025
# for each day of year (DOY 60-304, March-October).
#
# Hourly VPD formula (Soltani & Sinclair 2012):
#   VPD_h = max( [es(T_h) - es(T_min)] * (VPDF / 0.75), 0 )
#   es(T)  = 0.6108 * exp(17.27 * T / (237.3 + T))   [kPa]
#   T_h    = sinusoidal interpolation between Tmin and Tmax
#   VPDF   = 0.75 (VPD scaling factor)
#
# Output: r-model/outputs/plots/figure1_environmental_characterization.png
#
# Usage:
#   Rscript r-model/validation/figure1_environmental_characterization.R
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
    # 1. call-stack ofile (source())
    for (i in seq_len(sys.nframe())) {
      of <- sys.frame(i)$ofile
      if (!is.null(of) && nchar(of) > 0) { sp <- normalizePath(of, mustWork=FALSE); break }
    }
    # 2. Rscript --file= argument
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

WTH_DIR <- file.path(BASE_DIR, "inputs", "weather")
OUT_DIR <- file.path(BASE_DIR, "outputs", "plots")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
OUT_FILE <- file.path(OUT_DIR, "figure1_environmental_characterization.png")

cat("Base directory:", BASE_DIR, "\n")
cat("Output:", OUT_FILE, "\n\n")

# --- parameters --------------------------------------------------------------
VPDF    <- 0.75   # VPD scaling factor
VPDcr   <- 2.0    # critical VPD threshold for LT trait (kPa)
DOY_MIN <- 60     # March 1
DOY_MAX <- 304    # October 31

# --- helper functions --------------------------------------------------------
es <- function(T) 0.6108 * exp(17.27 * T / (237.3 + T))

calc_dayl <- function(doy, LAT) {
  Pi <- pi; RDN <- Pi / 180
  DEC <- atan(sin(23.45*RDN) * cos(2*Pi*(doy+10)/365) /
              sqrt(1 - (sin(23.45*RDN)*cos(2*Pi*(doy+10)/365))^2)) * (-1)
  SINLD <- sin(RDN*LAT) * sin(DEC)
  COSLD <- cos(RDN*LAT) * cos(DEC)
  AOBs  <- pmax(-0.9999, pmin(0.9999, SINLD/COSLD))
  12 * (1 + 2 * atan(AOBs / sqrt(1 - AOBs^2)) / Pi)
}

hours_above_vpd <- function(TMAX, TMIN, TMINA, DAYL, thr, VPDF) {
  P      <- 1.5
  SUNRIS <- 12 - 0.5*DAYL
  SUNSET <- 12 + 0.5*DAYL
  Hv     <- 1:24
  dl     <- Hv > SUNRIS & Hv < SUNSET
  angle  <- sin(pi * (Hv - SUNRIS) / (DAYL + 2*P))
  TEMP   <- ifelse(Hv < 13.5,
                   TMIN  + (TMAX  - TMIN)  * angle,
                   TMINA + (TMAX  - TMINA) * angle)
  VPD1   <- pmax((es(TEMP) - es(TMIN)) * (VPDF / 0.75), 0)
  sum(dl & VPD1 > thr, na.rm = TRUE)
}

# --- location table (south to north by latitude) -----------------------------
locs <- data.frame(
  file  = c("SSM_Rowher_AR.xlsx",      "SSM_Marianna_AR.xlsx",
            "SSM_Keiser_AR.xlsx",       "SSM_Jonesboro_AR.xlsx",
            "SSM_MountVernon_MO.xlsx",  "SSM_Novelty_MO.xlsx",
            "SSM_Albany_MO.xlsx",       "SSM_Eustis_NE.xlsx",
            "SSM_Lincoln_NE.xlsx",      "SSM_NorthPlatte_NE.xlsx"),
  label = c("Rohwer, AR",      "Marianna, AR",
            "Keiser, AR",      "Jonesboro, AR",
            "Mount Vernon, MO","Novelty, MO",
            "Albany, MO",      "Eustis, NE",
            "Lincoln, NE",     "North Platte, NE"),
  lat   = c(33.8, 34.7, 35.7, 35.8, 37.1, 40.0, 40.2, 40.5, 40.8, 41.0),
  stringsAsFactors = FALSE
)

# --- compute mean daily hours above VPDcr per DOY × location ----------------
cat("Computing mean daily hours above", VPDcr, "kPa (1985-2025)...\n")
all_rows <- list()

for (i in seq_len(nrow(locs))) {
  loc <- locs[i, ]
  cat(sprintf("  %s\n", loc$label))

  wth_path <- file.path(WTH_DIR, loc$file)
  if (!file.exists(wth_path))
    stop("Weather file not found: ", wth_path)

  d <- read_xlsx(wth_path, skip = 10,
                 col_names  = c("YEAR","DOY","SRAD","TMAX","TMIN","RAIN"),
                 col_types  = rep("numeric", 6))
  d <- d[!is.na(d$YEAR) & d$DOY >= DOY_MIN & d$DOY <= DOY_MAX, ]

  # next-day Tmin for afternoon cooling (carry last value at year boundary)
  d$TMINA <- c(d$TMIN[-1], NA)
  d$TMINA[is.na(d$TMINA)] <- d$TMIN[is.na(d$TMINA)]

  # daylength look-up (computed once per unique DOY)
  doys_u  <- sort(unique(d$DOY))
  dayl_lu <- setNames(calc_dayl(doys_u, loc$lat), doys_u)
  d$DAYL  <- dayl_lu[as.character(d$DOY)]

  # count daylight hours above threshold for every row
  d$hrs <- mapply(hours_above_vpd,
                  d$TMAX, d$TMIN, d$TMINA, d$DAYL,
                  MoreArgs = list(thr = VPDcr, VPDF = VPDF))

  # mean across years per DOY
  agg          <- aggregate(hrs ~ DOY, data = d, FUN = mean)
  agg$location <- loc$label
  all_rows[[i]] <- agg
}

df          <- do.call(rbind, all_rows)
df$location <- factor(df$location, levels = rev(locs$label))  # S at bottom

# --- 4 ordered categories ----------------------------------------------------
breaks   <- c(-Inf, 0.5, 2.5, 4.5, Inf)
cat_labs <- c("< 1 h", "1–2 h", "3–4 h", "> 4 h")
df$hrs_cat <- cut(df$hrs, breaks = breaks, labels = cat_labs, right = TRUE)

# BuGn palette (n=4) from ColorBrewer; lightest replaced by grey for "none"
bugu_cols <- brewer.pal(4, "BuGn")   # #EDF8FB  #B2E2E2  #66C2A4  #238B45
cat_colors <- c(
  "< 1 h" = "#D0D0D0",        # neutral grey — not triggered
  "1–2 h" = bugu_cols[2],     # #B2E2E2 — light blue-green
  "3–4 h" = bugu_cols[3],     # #66C2A4 — teal
  "> 4 h" = bugu_cols[4]      # #238B45 — dark green
)

# --- axis helpers ------------------------------------------------------------
month_mid <- c(74, 105, 135, 166, 196, 227, 258, 288)
month_lab <- c("Mar","Apr","May","Jun","Jul","Aug","Sep","Oct")
month_sep <- c(91, 121, 152, 182, 213, 244, 274)   # month boundaries
gs_start  <- 120   # ~May 1
gs_end    <- 300   # ~Oct 27

# --- build plot --------------------------------------------------------------
p <- ggplot(df, aes(x = DOY, y = location, fill = hrs_cat)) +

  # shading: muted background outside growing season
  annotate("rect", xmin=DOY_MIN, xmax=gs_start,
           ymin=0.5, ymax=10.5, fill="#f0f0f0", alpha=1) +
  annotate("rect", xmin=gs_end,  xmax=DOY_MAX,
           ymin=0.5, ymax=10.5, fill="#f0f0f0", alpha=1) +
  annotate("rect", xmin=gs_start, xmax=gs_end,
           ymin=0.5, ymax=10.5, fill="#fafafa", alpha=1) +

  geom_tile(height = 0.88) +

  # faint month separators
  geom_vline(xintercept = month_sep, color = "white",
             linewidth = 0.4, alpha = 0.9) +

  # bold state group dividers
  geom_hline(yintercept = c(4.5, 7.5), color = "white", linewidth = 1.5) +

  scale_fill_manual(
    values = cat_colors,
    name   = "Hours / day\nabove 2 kPa",
    drop   = FALSE
  ) +
  scale_x_continuous(
    breaks = month_mid, labels = month_lab,
    expand = c(0.005, 0)
  ) +
  scale_y_discrete(expand = expansion(add = c(0.5, 1.2))) +

  # growing season bracket
  annotate("segment", x=gs_start, xend=gs_end,
           y=10.78, yend=10.78, color="grey40", linewidth=0.8) +
  annotate("segment", x=gs_start, xend=gs_start,
           y=10.65, yend=10.78, color="grey40", linewidth=0.8) +
  annotate("segment", x=gs_end,   xend=gs_end,
           y=10.65, yend=10.78, color="grey40", linewidth=0.8) +
  annotate("text", x=(gs_start+gs_end)/2, y=11.12,
           label="Soybean growing season", size=3.0,
           color="grey30", fontface="italic", hjust=0.5) +

  # state labels (right margin)
  annotate("text", x=DOY_MAX+5, y=2.5, label="Arkansas",
           angle=90, size=3.3, fontface="bold", color="#333333", hjust=0.5) +
  annotate("text", x=DOY_MAX+5, y=5.5, label="Missouri",
           angle=90, size=3.3, fontface="bold", color="#333333", hjust=0.5) +
  annotate("text", x=DOY_MAX+5, y=8.5, label="Nebraska",
           angle=90, size=3.3, fontface="bold", color="#333333", hjust=0.5) +

  coord_cartesian(clip = "off") +

  labs(
    title = "Activation of the Limited-Transpiration Trait Across Locations and Season",
    x     = NULL,
    y     = NULL
  ) +

  theme_bw(base_size = 12) +
  theme(
    panel.grid        = element_blank(),
    panel.border      = element_rect(color="grey60", linewidth=0.6),
    legend.position   = "right",
    legend.key.size   = unit(0.65, "cm"),
    legend.key        = element_rect(color="grey70", linewidth=0.3),
    legend.title      = element_text(size=9.5, face="bold"),
    legend.text       = element_text(size=9.5),
    axis.text.y       = element_text(size=10, color="grey15"),
    axis.text.x       = element_text(size=10, color="grey15"),
    axis.ticks        = element_line(color="grey60"),
    plot.title        = element_text(face="bold", size=12.5, color="grey10",
                                     margin=margin(b=6)),
    plot.margin       = margin(6, 42, 6, 6)
  )

# --- save --------------------------------------------------------------------
ggsave(OUT_FILE, p, width=11, height=5.0, dpi=300)
cat(sprintf("\nFigure saved: %s\n", OUT_FILE))
