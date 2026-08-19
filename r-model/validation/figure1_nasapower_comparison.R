# =============================================================================
# Figure 1 Comparison — Model-Estimated vs NASA POWER Observed Hourly VPD
#
# Replicates Figure 1 (hours/day above VPDcr = 2.0 kPa) using two methods:
#
#  (A) Model estimate  — same sinusoidal Tmin/Tmax interpolation as Figure 1
#                        (Soltani & Sinclair 2012), using weather file data
#                        1985–2025.
#
#  (B) NASA POWER observed — actual hourly T2M + RH2M from the NASA POWER
#                        MERRA-2 reanalysis product.
#                        VPD_obs[h] = es(T2M[h]) × (1 − RH2M[h]/100)
#                        Period: 2001–2025 (earliest available for hourly data).
#
# Both panels use the same:
#  • DOY range: 91–273 (April 1 – September 30)
#  • Threshold: VPDcr = 2.0 kPa
#  • Daylight-hour filter: astronomical sunrise/sunset
#  • 4-category BuGn colour scale  (< 1 h / 1–2 h / 3–4 h / > 4 h)
#  • 10 locations (south → north)
#
# Output: r-model/outputs/plots/figure1_nasapower_comparison.tif
#
# Usage:
#   Rscript r-model/validation/figure1_nasapower_comparison.R
# =============================================================================

# --- packages ----------------------------------------------------------------
needed <- c("readxl", "ggplot2", "RColorBrewer", "httr", "jsonlite", "patchwork")
miss   <- needed[!sapply(needed, requireNamespace, quietly = TRUE)]
if (length(miss) > 0) install.packages(miss, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(readxl); library(ggplot2); library(RColorBrewer)
  library(httr);   library(jsonlite); library(patchwork)
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
OUT_DIR   <- file.path(BASE_DIR, "outputs", "plots")
CACHE_DIR <- file.path(BASE_DIR, "outputs", "nasapower_cache")
dir.create(OUT_DIR,   recursive = TRUE, showWarnings = FALSE)
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
OUT_FILE  <- file.path(OUT_DIR, "figure1_nasapower_comparison.tif")

cat("Base directory:", BASE_DIR, "\n")
cat("Output:        ", OUT_FILE, "\n\n")

# --- parameters --------------------------------------------------------------
VPDF    <- 0.75
VPDcr   <- 2.0
DOY_MIN <- 91     # April 1
DOY_MAX <- 273    # September 30

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

# Model-estimated hours above VPDcr (same formula as Figure 1)
hours_above_vpd_model <- function(TMAX, TMIN, TMINA, DAYL, thr, VPDF) {
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

# --- location table ----------------------------------------------------------
# Longitudes from standard city coordinates; latitudes match weather files
locs <- data.frame(
  wth_file = c("SSM_Rowher_AR.xlsx",     "SSM_Marianna_AR.xlsx",
               "SSM_Keiser_AR.xlsx",      "SSM_Jonesboro_AR.xlsx",
               "SSM_MountVernon_MO.xlsx", "SSM_Novelty_MO.xlsx",
               "SSM_Albany_MO.xlsx",      "SSM_Eustis_NE.xlsx",
               "SSM_Lincoln_NE.xlsx",     "SSM_NorthPlatte_NE.xlsx"),
  label    = c("Rohwer, AR",       "Marianna, AR",
               "Keiser, AR",       "Jonesboro, AR",
               "Mount Vernon, MO", "Novelty, MO",
               "Albany, MO",       "Eustis, NE",
               "Lincoln, NE",      "North Platte, NE"),
  lat      = c(33.8,  34.7,  35.7,  35.8,  37.1,
               40.0,  40.2,  40.5,  40.8,  41.0),
  lon      = c(-91.82, -90.77, -89.95, -90.70, -93.82,
               -92.20, -94.33, -99.92, -96.68, -100.77),
  stringsAsFactors = FALSE
)

# =============================================================================
# PANEL A — Model estimate (1985–2025, same as Figure 1)
# =============================================================================
cat("=== Panel A: Model-estimated VPD (1985–2025) ===\n")
rows_model <- list()

for (i in seq_len(nrow(locs))) {
  loc <- locs[i, ]
  cat(sprintf("  %s\n", loc$label))

  d <- read_xlsx(file.path(WTH_DIR, loc$wth_file), skip = 10,
                 col_names  = c("YEAR","DOY","SRAD","TMAX","TMIN","RAIN"),
                 col_types  = rep("numeric", 6))
  d <- d[!is.na(d$YEAR) & d$DOY >= DOY_MIN & d$DOY <= DOY_MAX, ]
  d$TMINA <- c(d$TMIN[-1], NA)
  d$TMINA[is.na(d$TMINA)] <- d$TMIN[is.na(d$TMINA)]

  doys_u  <- sort(unique(d$DOY))
  dayl_lu <- setNames(calc_dayl(doys_u, loc$lat), doys_u)
  d$DAYL  <- dayl_lu[as.character(d$DOY)]

  d$hrs <- mapply(hours_above_vpd_model,
                  d$TMAX, d$TMIN, d$TMINA, d$DAYL,
                  MoreArgs = list(thr = VPDcr, VPDF = VPDF))

  agg          <- aggregate(hrs ~ DOY, data = d, FUN = mean)
  agg$location <- loc$label
  rows_model[[i]] <- agg
}

df_model          <- do.call(rbind, rows_model)
df_model$location <- factor(df_model$location, levels = rev(locs$label))
df_model$panel    <- "A — Model estimate (1985–2025)"

# =============================================================================
# PANEL B — NASA POWER observed hourly T2M + RH2M (2001–2025)
# =============================================================================
cat("\n=== Panel B: NASA POWER observed VPD (2001–2025) ===\n")

POWER_URL <- "https://power.larc.nasa.gov/api/temporal/hourly/point"

fetch_power <- function(lat, lon, start, end, cache_file) {
  if (file.exists(cache_file)) {
    cat("    [cached]\n")
    return(read.csv(cache_file))
  }
  cat(sprintf("    Downloading %s–%s ...\n", start, end))
  r <- GET(POWER_URL,
           query = list(parameters = "T2M,RH2M", community = "AG",
                        longitude  = as.character(lon),
                        latitude   = as.character(lat),
                        start = start, end = end, format = "JSON"),
           timeout(120))
  if (status_code(r) != 200)
    stop("NASA POWER API error: ", status_code(r), " - ",
         substr(rawToChar(content(r, "raw")), 1, 200))
  body <- fromJSON(rawToChar(content(r, "raw")))
  t2m  <- unlist(body$properties$parameter$T2M)
  rh2m <- unlist(body$properties$parameter$RH2M)
  stamp <- names(t2m)
  df <- data.frame(
    stamp = stamp,
    YEAR  = as.integer(substr(stamp, 1, 4)),
    MONTH = as.integer(substr(stamp, 5, 6)),
    DAY   = as.integer(substr(stamp, 7, 8)),
    HOUR  = as.integer(substr(stamp, 9, 10)),
    T2M   = as.numeric(t2m),
    RH2M  = as.numeric(rh2m),
    stringsAsFactors = FALSE
  )
  write.csv(df, cache_file, row.names = FALSE)
  df
}

doy_from_ymd <- function(year, month, day) {
  as.integer(format(as.Date(paste(year, month, day, sep="-")), "%j"))
}

rows_power <- list()

for (i in seq_len(nrow(locs))) {
  loc <- locs[i, ]
  cat(sprintf("  %s\n", loc$label))

  slug <- gsub("[^A-Za-z0-9]", "_", loc$label)

  cf1 <- file.path(CACHE_DIR, paste0(slug, "_2001_2015.csv"))
  cf2 <- file.path(CACHE_DIR, paste0(slug, "_2016_2025.csv"))

  d1 <- fetch_power(loc$lat, loc$lon, "20010101", "20151231", cf1)
  d2 <- fetch_power(loc$lat, loc$lon, "20160101", "20251231", cf2)
  d  <- rbind(d1, d2)

  d <- d[d$T2M > -900 & d$RH2M >= 0 & d$RH2M <= 100, ]

  d$DOY <- doy_from_ymd(d$YEAR, d$MONTH, d$DAY)
  d     <- d[d$DOY >= DOY_MIN & d$DOY <= DOY_MAX, ]

  d$VPD_obs <- pmax(es(d$T2M) * (1 - d$RH2M / 100), 0)

  doys_u  <- sort(unique(d$DOY))
  dayl_lu <- setNames(calc_dayl(doys_u, loc$lat), doys_u)

  dayl_v   <- dayl_lu[as.character(d$DOY)]
  sunris_v <- 12 - 0.5 * dayl_v
  sunset_v <- 12 + 0.5 * dayl_v
  hour_mid <- d$HOUR + 0.5
  d$daylight <- hour_mid > sunris_v & hour_mid < sunset_v

  day_key   <- paste(d$YEAR, d$DOY)
  d$key     <- day_key
  day_above <- tapply(d$daylight & d$VPD_obs > VPDcr, day_key, sum, na.rm = TRUE)
  day_doy   <- tapply(d$DOY,  day_key, `[`, 1)
  day_year  <- tapply(d$YEAR, day_key, `[`, 1)

  daily_df <- data.frame(
    YEAR = as.integer(day_year),
    DOY  = as.integer(day_doy),
    hrs  = as.numeric(day_above)
  )

  agg          <- aggregate(hrs ~ DOY, data = daily_df, FUN = mean)
  agg$location <- loc$label
  rows_power[[i]] <- agg
}

df_power          <- do.call(rbind, rows_power)
df_power$location <- factor(df_power$location, levels = rev(locs$label))
df_power$panel    <- "B — NASA POWER observed (2001–2025)"

cat("\nModel    range: ", round(min(df_model$hrs),2), "–", round(max(df_model$hrs),2), "h\n")
cat("POWER    range: ", round(min(df_power$hrs),2), "–", round(max(df_power$hrs),2), "h\n\n")

# =============================================================================
# Shared plot settings
# =============================================================================
breaks   <- c(-Inf, 0.5, 2.5, 4.5, Inf)
cat_labs <- c("< 1 h", "1 to 2 h", "3 to 4 h", "> 4 h")

bugu_cols  <- brewer.pal(4, "BuGn")
cat_colors <- c(
  "< 1 h"     = "#D0D0D0",
  "1 to 2 h"  = bugu_cols[2],
  "3 to 4 h"  = bugu_cols[3],
  "> 4 h"     = bugu_cols[4]
)

df_model$hrs_cat <- cut(df_model$hrs, breaks = breaks, labels = cat_labs, right = TRUE)
df_power$hrs_cat <- cut(df_power$hrs, breaks = breaks, labels = cat_labs, right = TRUE)

month_mid <- c(105, 135, 166, 196, 227, 258)
month_lab <- c("Apr","May","Jun","Jul","Aug","Sep")
month_sep <- c(121, 152, 182, 213, 244)

make_panel <- function(df, subtitle) {
  ggplot(df, aes(x = DOY, y = location, fill = hrs_cat)) +
    annotate("rect", xmin = DOY_MIN, xmax = DOY_MAX,
             ymin = 0.5, ymax = 10.5, fill = "#fafafa", alpha = 1) +
    geom_tile(height = 0.88) +
    geom_vline(xintercept = month_sep, color = "white",
               linewidth = 0.4, alpha = 0.9) +
    geom_hline(yintercept = c(4.5, 7.5), color = "white", linewidth = 1.5) +
    scale_fill_manual(
      values = cat_colors,
      name   = "Hours/day\nabove 2 kPa",
      drop   = FALSE
    ) +
    scale_x_continuous(
      breaks = month_mid, labels = month_lab,
      expand = c(0.005, 0)
    ) +
    scale_y_discrete(expand = expansion(add = c(0.5, 0.5))) +
    labs(x = NULL, y = NULL, subtitle = subtitle) +
    theme_bw(base_size = 10) +
    theme(
      panel.grid      = element_blank(),
      panel.border    = element_rect(color = "grey60", linewidth = 0.5),
      legend.position = "right",
      legend.key.size = unit(0.55, "cm"),
      legend.key      = element_rect(color = "grey70", linewidth = 0.3),
      legend.title    = element_text(size = 8.5),
      legend.text     = element_text(size = 8.5),
      axis.text.y     = element_text(size = 8.5, color = "grey15"),
      axis.text.x     = element_text(size = 8.5, color = "grey15"),
      axis.ticks      = element_line(color = "grey60"),
      plot.subtitle   = element_text(size = 8.5, color = "grey30", margin = margin(b=2)),
      plot.margin     = margin(4, 4, 4, 4)
    )
}

pA <- make_panel(df_model, "Model estimate  (sinusoidal Tmin/Tmax, 1985–2025)")
pB <- make_panel(df_power, "NASA POWER observed  (T2M + RH2M, 2001–2025)")

combined <- (pA / pB) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

ggsave(OUT_FILE, combined, width = 7.5, height = 7.6, units = "in",
       dpi = 600, device = "tiff", compression = "lzw")
cat(sprintf("Figure saved: %s  (%.1f cm wide, 600 dpi TIFF)\n",
            OUT_FILE, 7.5 * 2.54))
