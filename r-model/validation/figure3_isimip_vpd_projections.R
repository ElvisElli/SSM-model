# =============================================================================
# Figure 3 – ISIMIP3b Future VPD Projections (Bias-Corrected)
#
# Prerequisites:
#   Run  r-model/validation/download_isimip_data.py  first to populate
#   r-model/outputs/isimip_cache/ with the daily point CSVs.
#
# Workflow:
#   1. Read ISIMIP3b daily point CSVs (all months, full years)
#   2. Read SSM observed baseline weather files (xlsx, 1985-2025)
#   3. Bias-correct ISIMIP temperatures against baseline using the additive
#      delta method (monthly, per GCM × location)
#   4. Generate bias-inspection plots (before and after correction)
#   5. Compute daily VPD from bias-corrected temperatures + raw HURS
#   6. Filter July → annual means → projection plot
#
# Bias-correction method (delta method, after Correia-McKay et al.):
#   Overlap period: 1985–2014 (historical ISIMIP + SSM baseline)
#   bias[gcm, location, month] = mean(GCM_hist) − mean(baseline)
#   correction (temperature, additive):
#       corrected_future = raw_future_°C − bias
#   Note: hurs is not corrected – no baseline humidity in SSM xlsx files.
#         VPD = (es(Tmax_corr) + es(Tmin_corr)) / 2 × (1 − hurs/100)
#
# Outputs:
#   outputs/plots/figure3_inspect_raw.tif      bias before correction
#   outputs/plots/figure3_inspect_corrected.tif  bias after correction
#   outputs/plots/figure3_isimip_vpd_projections.tif  final figure
#   outputs/plots/figure3_isimip_vpd_projections.pdf
#
# Usage:
#   Rscript r-model/validation/figure3_isimip_vpd_projections.R
# =============================================================================


# =============================================================================
# 1.  PACKAGES
# =============================================================================

needed <- c("readxl", "dplyr", "tidyr", "ggplot2", "patchwork", "scales",
            "RColorBrewer")
miss <- needed[!sapply(needed, requireNamespace, quietly = TRUE)]
if (length(miss) > 0) install.packages(miss, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr)
  library(ggplot2); library(patchwork); library(scales); library(RColorBrewer)
})


# =============================================================================
# 2.  PATHS
# =============================================================================

if (!exists("BASE_DIR") || !dir.exists(BASE_DIR)) {
  script_path <- tryCatch({
    sp <- NULL
    for (i in seq_len(sys.nframe())) {
      of <- sys.frame(i)$ofile
      if (!is.null(of) && nchar(of) > 0) { sp <- normalizePath(of, mustWork = FALSE); break }
    }
    if (is.null(sp)) {
      args <- commandArgs(trailingOnly = FALSE)
      ff <- grep("^--file=", args, value = TRUE)
      if (length(ff) > 0) sp <- normalizePath(sub("^--file=", "", ff[1]), mustWork = FALSE)
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

CACHE_DIR <- file.path(BASE_DIR, "outputs", "isimip_cache")
WTH_DIR   <- file.path(BASE_DIR, "inputs",  "weather")
OUT_DIR   <- file.path(BASE_DIR, "outputs", "plots")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(CACHE_DIR) || length(list.files(CACHE_DIR)) == 0)
  stop("No ISIMIP CSVs found in: ", CACHE_DIR,
       "\nRun download_isimip_data.py first.")

cat("Base dir  :", BASE_DIR,  "\n")
cat("Cache dir :", CACHE_DIR, "\n")
cat("Weather   :", WTH_DIR,   "\n\n")


# =============================================================================
# 3.  STUDY DESIGN
# =============================================================================

LOCATIONS <- data.frame(
  name    = c("Rohwer, AR",         "Lincoln, NE"),
  wth_file= c("SSM_Rowher_AR.xlsx", "SSM_Lincoln_NE.xlsx"),   # note: "Rowher" typo is in the actual filename
  lat     = c(33.8,                  40.8),
  lon     = c(-91.82,               -96.68),
  stringsAsFactors = FALSE
)

GCMS <- data.frame(
  label   = c("GFDL-ESM4",     "IPSL-CM6A-LR",  "MPI-ESM1-2-HR",
              "MRI-ESM2-0",    "UKESM1-0-LL"),
  slug    = c("gfdl-esm4",     "ipsl-cm6a-lr",   "mpi-esm1-2-hr",
              "mri-esm2-0",    "ukesm1-0-ll"),
  variant = c("r1i1p1f1",      "r1i1p1f1",       "r1i1p1f1",
              "r1i1p1f1",      "r1i1p1f2"),
  stringsAsFactors = FALSE
)

SCENARIOS <- data.frame(
  id    = c("historical", "ssp126",    "ssp370",    "ssp585"),
  label = c("Historical", "SSP1-2.6", "SSP3-7.0",  "SSP5-8.5"),
  stringsAsFactors = FALSE
)

# Decade periods available in cache (must match download_isimip_data.py)
# Historical: decade-aligned; last file covers 2011-2014 only.
PERIODS <- list(
  historical = list(c(1981, 1990), c(1991, 2000), c(2001, 2010), c(2011, 2014)),
  ssp126     = list(c(2015, 2020), c(2021, 2030), c(2031, 2040), c(2041, 2050),
                    c(2051, 2060), c(2061, 2070), c(2071, 2080), c(2081, 2090),
                    c(2091, 2100)),
  ssp370     = list(c(2015, 2020), c(2021, 2030), c(2031, 2040), c(2041, 2050),
                    c(2051, 2060), c(2061, 2070), c(2071, 2080), c(2081, 2090),
                    c(2091, 2100)),
  ssp585     = list(c(2015, 2020), c(2021, 2030), c(2031, 2040), c(2041, 2050),
                    c(2051, 2060), c(2061, 2070), c(2071, 2080), c(2081, 2090),
                    c(2091, 2100))
)

VARIABLES <- c("tasmax", "tasmin", "hurs")

# Bias-correction overlap period
BIAS_YR_MIN <- 1985L
BIAS_YR_MAX <- 2014L

JULY <- 7L


# =============================================================================
# 4.  HELPER FUNCTIONS
# =============================================================================

# Saturation vapour pressure (kPa), T in °C
es <- function(T_C) 0.6108 * exp(17.27 * T_C / (237.3 + T_C))

# Daily VPD (kPa): bias-corrected temps in °C, hurs in %
calc_vpd <- function(tmax_C, tmin_C, hurs_pct) {
  pmax((es(tmax_C) + es(tmin_C)) / 2 * (1 - hurs_pct / 100), 0)
}

# Read one ISIMIP point CSV (YYYY-MM-DD, value  — no header)
read_isimip_csv <- function(path) {
  df <- tryCatch(
    read.csv(path, header = FALSE,
             col.names = c("date_chr", "value"),
             colClasses = c("character", "numeric")),
    error = function(e) NULL
  )
  if (is.null(df)) return(NULL)
  df$date  <- as.Date(df$date_chr)
  df$year  <- as.integer(format(df$date, "%Y"))
  df$month <- as.integer(format(df$date, "%m"))
  df[!is.na(df$date) & !is.na(df$value), c("date", "year", "month", "value")]
}

# Build expected CSV filename (matches Python downloader convention)
# Format verified empirically: lon BEFORE lat, slug WITH hyphens
csv_name <- function(gcm_slug, variant, scenario, variable, lat, lon,
                     yr_start, yr_end) {
  sprintf("%s_%s_w5e5_%s_%s_lon%glat%g_daily_%d_%d.csv",
          gcm_slug, variant, scenario, variable,
          lon, lat, yr_start, yr_end)
}


# =============================================================================
# 5.  READ ALL ISIMIP CSVs  (full year, all months)
# =============================================================================

cat("=== Reading ISIMIP CSVs ===\n")
raw_list <- list()

for (loc_i in seq_len(nrow(LOCATIONS))) {
  loc <- LOCATIONS[loc_i, ]

  for (gcm_i in seq_len(nrow(GCMS))) {
    gcm <- GCMS[gcm_i, ]

    for (scen_i in seq_len(nrow(SCENARIOS))) {
      scenario  <- SCENARIOS$id[scen_i]
      periods   <- PERIODS[[scenario]]
      var_data  <- list()   # one element per variable

      for (v in VARIABLES) {
        chunks <- list()
        for (per in periods) {
          fname <- csv_name(gcm$slug, gcm$variant, scenario, v,
                            loc$lat, loc$lon, per[1], per[2])
          fpath <- file.path(CACHE_DIR, fname)
          if (!file.exists(fpath)) next
          chunk <- read_isimip_csv(fpath)
          if (!is.null(chunk)) chunks[[length(chunks) + 1]] <- chunk
        }
        if (length(chunks) > 0) {
          d <- bind_rows(chunks)
          colnames(d)[colnames(d) == "value"] <- v
          var_data[[v]] <- d
        }
      }

      # Skip if any variable is missing for this combination
      if (!all(VARIABLES %in% names(var_data))) next

      # Merge the three variables on date
      merged <- Reduce(function(a, b) merge(a, b, by = c("date", "year", "month")),
                       var_data)
      if (nrow(merged) == 0) next

      raw_list[[length(raw_list) + 1]] <- data.frame(
        location = loc$name,
        gcm      = gcm$label,
        scenario = scenario,
        merged,
        stringsAsFactors = FALSE
      )
      cat(sprintf("  %-20s %-16s %-12s %5d rows\n",
                  loc$name, gcm$label, scenario, nrow(merged)))
    }
  }
}

if (length(raw_list) == 0)
  stop("No ISIMIP data loaded.  Run download_isimip_data.py first.")

raw_df <- bind_rows(raw_list)

# Convert K → °C for temperature variables (ISIMIP3b W5E5 convention)
raw_df <- raw_df %>%
  mutate(
    tasmax_C = tasmax - 273.15,
    tasmin_C = tasmin - 273.15
  )

cat(sprintf("\nTotal daily rows loaded: %d\n\n", nrow(raw_df)))


# =============================================================================
# 6.  READ SSM BASELINE WEATHER  (observed, 1985–2025)
# =============================================================================

cat("=== Reading SSM baseline weather ===\n")
baseline_list <- list()

for (loc_i in seq_len(nrow(LOCATIONS))) {
  loc  <- LOCATIONS[loc_i, ]
  wpath <- file.path(WTH_DIR, loc$wth_file)
  if (!file.exists(wpath)) {
    warning("SSM weather file not found: ", wpath)
    next
  }

  wth <- suppressMessages(
    read_excel(wpath, skip = 9,
               col_names = c("YEAR", "DOY", "SRAD", "TMAX", "TMIN", "RAIN"),
               col_types  = rep("numeric", 6))
  )
  wth <- wth[!is.na(wth$YEAR), ]
  wth$date  <- as.Date(paste0(wth$YEAR, "-01-01")) + wth$DOY - 1
  wth$year  <- as.integer(wth$YEAR)
  wth$month <- as.integer(format(wth$date, "%m"))

  baseline_list[[loc_i]] <- data.frame(
    location = loc$name,
    date     = wth$date,
    year     = wth$year,
    month    = wth$month,
    TMAX     = wth$TMAX,    # °C
    TMIN     = wth$TMIN,    # °C
    stringsAsFactors = FALSE
  )
  cat(sprintf("  %-20s  %d rows (%d–%d)\n",
              loc$name, nrow(wth), min(wth$year), max(wth$year)))
}

baseline_df <- bind_rows(baseline_list)
cat("\n")


# =============================================================================
# 7.  BIAS CORRECTION
# =============================================================================
# Delta method (additive for temperature):
#   bias[gcm, location, month] = mean(GCM_hist, overlap) − mean(baseline, overlap)
#   corrected = raw_°C − bias
# Note: hurs is NOT corrected (no baseline humidity in SSM files).
# =============================================================================

cat(sprintf("=== Bias correction (overlap: %d–%d) ===\n", BIAS_YR_MIN, BIAS_YR_MAX))

# --- 7a.  Baseline monthly means (observed) ----------------------------------
baseline_monthly <- baseline_df %>%
  filter(year >= BIAS_YR_MIN, year <= BIAS_YR_MAX) %>%
  group_by(location, month) %>%
  summarise(TMAX_base = mean(TMAX, na.rm = TRUE),
            TMIN_base = mean(TMIN, na.rm = TRUE),
            .groups   = "drop")

# --- 7b.  ISIMIP historical monthly means (per GCM) --------------------------
isimip_hist_monthly <- raw_df %>%
  filter(scenario == "historical",
         year >= BIAS_YR_MIN, year <= BIAS_YR_MAX) %>%
  group_by(location, gcm, month) %>%
  summarise(TMAX_gcm = mean(tasmax_C, na.rm = TRUE),
            TMIN_gcm = mean(tasmin_C, na.rm = TRUE),
            .groups  = "drop")

# --- 7c.  Calculate bias per GCM × location × month -------------------------
bias_df <- isimip_hist_monthly %>%
  left_join(baseline_monthly, by = c("location", "month")) %>%
  mutate(
    bias_tmax = TMAX_gcm - TMAX_base,   # positive = GCM warmer than observed
    bias_tmin = TMIN_gcm - TMIN_base
  )

cat("  Bias summary (mean absolute, all GCMs × locations):\n")
cat(sprintf("    Tmax: %.2f °C | Tmin: %.2f °C\n",
            mean(abs(bias_df$bias_tmax), na.rm = TRUE),
            mean(abs(bias_df$bias_tmin), na.rm = TRUE)))


# =============================================================================
# 7d.  INSPECTION PLOTS — before correction
# =============================================================================

# --- Plot A: Monthly mean Tmax — baseline vs each GCM (historical period) ---
monthly_compare_raw <- isimip_hist_monthly %>%
  left_join(baseline_monthly, by = c("location", "month")) %>%
  pivot_longer(cols = c(TMAX_gcm, TMAX_base),
               names_to = "source", values_to = "Tmax") %>%
  mutate(source = recode(source,
                         TMAX_gcm  = "GCM historical",
                         TMAX_base = "SSM observed"))

gcm_palette <- setNames(
  c("#636363",
    brewer.pal(max(nrow(GCMS), 3), "Set1")[seq_len(nrow(GCMS))]),
  c("SSM observed", GCMS$label)
)

p_raw_tmax <- ggplot(monthly_compare_raw,
                     aes(x = factor(month), y = Tmax,
                         colour = ifelse(source == "SSM observed", "SSM observed", gcm),
                         group  = interaction(gcm, source),
                         linetype = source)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  scale_colour_manual(name = NULL, values = gcm_palette) +
  scale_linetype_manual(name = NULL, values = c("GCM historical" = "dashed",
                                                  "SSM observed"   = "solid")) +
  scale_x_discrete(labels = month.abb) +
  labs(x = NULL, y = "Monthly mean Tmax (°C)",
       title = "Monthly Tmax — Raw ISIMIP vs SSM baseline",
       subtitle = sprintf("Overlap period %d–%d", BIAS_YR_MIN, BIAS_YR_MAX)) +
  facet_wrap(~ location, scales = "free_y") +
  theme_classic(base_size = 9) +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_line(colour = "#E8E8E8", linewidth = 0.3))

# --- Plot B: Tmax bias by month before correction ----------------------------
bias_long <- bias_df %>%
  pivot_longer(cols = c(bias_tmax, bias_tmin),
               names_to = "variable", values_to = "bias") %>%
  mutate(variable = recode(variable,
                           bias_tmax = "Tmax bias (°C)",
                           bias_tmin = "Tmin bias (°C)"))

p_bias_raw <- ggplot(bias_long,
                     aes(x = factor(month), y = bias,
                         colour = gcm, group = gcm)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red",
             linewidth = 0.5) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.5) +
  scale_colour_manual(name = "GCM",
                      values = brewer.pal(max(nrow(GCMS), 3), "Set1")[seq_len(nrow(GCMS))]) +
  scale_x_discrete(labels = month.abb) +
  labs(x = "Month", y = "Bias (GCM − observed, °C)",
       title = "Temperature bias — before correction") +
  facet_grid(variable ~ location) +
  theme_classic(base_size = 9) +
  theme(legend.position  = "bottom",
        strip.background = element_rect(fill = "#F5F5F5", colour = NA),
        panel.grid.major.y = element_line(colour = "#E8E8E8", linewidth = 0.3))

inspect_raw <- p_raw_tmax / p_bias_raw + plot_layout(heights = c(1, 1.2))

tiff(file.path(OUT_DIR, "figure3_inspect_raw.tif"),
     width = 9, height = 8, units = "in", res = 300, compression = "lzw")
print(inspect_raw)
dev.off()
cat("  Saved: figure3_inspect_raw.tif\n")


# --- 7e.  Apply bias correction to ALL ISIMIP data (historical + future) ----
corrected_df <- raw_df %>%
  left_join(bias_df[, c("location", "gcm", "month",
                         "bias_tmax", "bias_tmin")],
            by = c("location", "gcm", "month")) %>%
  mutate(
    tmax_corr = tasmax_C - bias_tmax,   # °C, bias-corrected
    tmin_corr = tasmin_C - bias_tmin    # °C, bias-corrected
    # hurs is used as-is (no baseline humidity available)
  )


# =============================================================================
# 7f.  INSPECTION PLOTS — after correction
# =============================================================================

isimip_hist_corr_monthly <- corrected_df %>%
  filter(scenario == "historical",
         year >= BIAS_YR_MIN, year <= BIAS_YR_MAX) %>%
  group_by(location, gcm, month) %>%
  summarise(TMAX_gcm = mean(tmax_corr, na.rm = TRUE),
            TMIN_gcm = mean(tmin_corr, na.rm = TRUE),
            .groups  = "drop")

monthly_compare_corr <- isimip_hist_corr_monthly %>%
  left_join(baseline_monthly, by = c("location", "month")) %>%
  pivot_longer(cols = c(TMAX_gcm, TMAX_base),
               names_to = "source", values_to = "Tmax") %>%
  mutate(source = recode(source,
                         TMAX_gcm  = "GCM corrected",
                         TMAX_base = "SSM observed"))

p_corr_tmax <- ggplot(monthly_compare_corr,
                      aes(x = factor(month), y = Tmax,
                          colour = ifelse(source == "SSM observed",
                                          "SSM observed", gcm),
                          group  = interaction(gcm, source),
                          linetype = source)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  scale_colour_manual(name = NULL, values = gcm_palette) +
  scale_linetype_manual(name = NULL, values = c("GCM corrected" = "dashed",
                                                  "SSM observed"  = "solid")) +
  scale_x_discrete(labels = month.abb) +
  labs(x = NULL, y = "Monthly mean Tmax (°C)",
       title = "Monthly Tmax — Bias-corrected ISIMIP vs SSM baseline",
       subtitle = sprintf("Overlap period %d–%d", BIAS_YR_MIN, BIAS_YR_MAX)) +
  facet_wrap(~ location, scales = "free_y") +
  theme_classic(base_size = 9) +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_line(colour = "#E8E8E8", linewidth = 0.3))

# Bias after correction (should be near zero)
bias_after_df <- isimip_hist_corr_monthly %>%
  left_join(baseline_monthly, by = c("location", "month")) %>%
  mutate(bias_tmax = TMAX_gcm - TMAX_base,
         bias_tmin = TMIN_gcm - TMIN_base) %>%
  pivot_longer(cols = c(bias_tmax, bias_tmin),
               names_to = "variable", values_to = "bias") %>%
  mutate(variable = recode(variable,
                           bias_tmax = "Tmax bias (°C)",
                           bias_tmin = "Tmin bias (°C)"))

p_bias_corr <- ggplot(bias_after_df,
                      aes(x = factor(month), y = bias,
                          colour = gcm, group = gcm)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red",
             linewidth = 0.5) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.5) +
  scale_colour_manual(name = "GCM",
                      values = brewer.pal(max(nrow(GCMS), 3), "Set1")[seq_len(nrow(GCMS))]) +
  scale_x_discrete(labels = month.abb) +
  scale_y_continuous(limits = c(-0.5, 0.5)) +
  labs(x = "Month", y = "Residual bias (°C)",
       title = "Temperature bias — after correction (expect ≈ 0)") +
  facet_grid(variable ~ location) +
  theme_classic(base_size = 9) +
  theme(legend.position  = "bottom",
        strip.background = element_rect(fill = "#F5F5F5", colour = NA),
        panel.grid.major.y = element_line(colour = "#E8E8E8", linewidth = 0.3))

inspect_corr <- p_corr_tmax / p_bias_corr + plot_layout(heights = c(1, 1.2))

tiff(file.path(OUT_DIR, "figure3_inspect_corrected.tif"),
     width = 9, height = 8, units = "in", res = 300, compression = "lzw")
print(inspect_corr)
dev.off()
cat("  Saved: figure3_inspect_corrected.tif\n")

cat(sprintf("  Residual bias after correction — Tmax: %.3f °C | Tmin: %.3f °C\n",
            mean(abs(bias_after_df$bias[bias_after_df$variable == "Tmax bias (°C)"]),
                 na.rm = TRUE),
            mean(abs(bias_after_df$bias[bias_after_df$variable == "Tmin bias (°C)"]),
                 na.rm = TRUE)))
cat("\n")


# =============================================================================
# 7g.  INSPECTION PLOTS — annual time-series (mirrors "Mint over time" style)
#
# Baseline (SSM observed) in red; GCM data (all scenarios pooled, multi-model
# mean) in teal.  Shape encodes bias status: circle = Bias Corrected,
# triangle = Not Bias Corrected.  The overlap period (~1985-2014) reveals
# whether the correction brings GCM temperatures in line with observations.
# =============================================================================

# Baseline: annual SSM observed means (all available years)
baseline_annual <- baseline_df %>%
  group_by(location, year) %>%
  summarise(Tmax = mean(TMAX, na.rm = TRUE),
            Tmin = mean(TMIN, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(climate = "Baseline",
         correction = "Observed")

# GCM — NOT bias corrected: average across all GCMs × scenarios per year
raw_gcm_annual <- raw_df %>%
  group_by(location, scenario, gcm, year) %>%
  summarise(Tmax = mean(tasmax_C, na.rm = TRUE),
            Tmin = mean(tasmin_C, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(location, year) %>%
  summarise(Tmax = mean(Tmax, na.rm = TRUE),
            Tmin = mean(Tmin, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(climate = "Future Climate",
         correction = "Not Bias Corrected")

# GCM — bias corrected: average across all GCMs × scenarios per year
corr_gcm_annual <- corrected_df %>%
  group_by(location, scenario, gcm, year) %>%
  summarise(Tmax = mean(tmax_corr, na.rm = TRUE),
            Tmin = mean(tmin_corr, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(location, year) %>%
  summarise(Tmax = mean(Tmax, na.rm = TRUE),
            Tmin = mean(Tmin, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(climate = "Future Climate",
         correction = "Bias Corrected")

annual_ts <- bind_rows(baseline_annual, raw_gcm_annual, corr_gcm_annual)
annual_ts$year <- as.numeric(annual_ts$year)

CLIM_COLS   <- c("Baseline"       = "#E8634E",
                 "Future Climate" = "#3ABFC4")
CORR_SHAPES <- c("Observed"             = 16,
                 "Bias Corrected"       = 16,
                 "Not Bias Corrected"   = 17)

make_ts_panel <- function(var, ylab_text) {
  ggplot(annual_ts,
         aes(x      = year,
             y      = .data[[var]],
             colour = climate,
             shape  = correction,
             group  = interaction(climate, correction))) +
    geom_line(linewidth = 0.35) +
    geom_point(size = 1.1) +
    scale_colour_manual(name = NULL, values = CLIM_COLS) +
    scale_shape_manual (name = NULL, values = CORR_SHAPES) +
    facet_wrap(~ location) +
    labs(x = "Year", y = ylab_text) +
    theme_classic(base_size = 9) +
    theme(
      legend.position  = "right",
      panel.grid.major = element_line(colour = "#E8E8E8", linewidth = 0.3),
      strip.background = element_rect(fill = "#F5F5F5", colour = NA)
    )
}

p_ts_tmax <- make_ts_panel("Tmax", "Annual mean Tmax (°C)")
p_ts_tmin <- make_ts_panel("Tmin", "Annual mean Tmin (°C)")

inspect_ts <- (p_ts_tmax / p_ts_tmin) +
  plot_annotation(
    title    = "Annual temperature over time — baseline vs GCM (bias corrected / not)",
    subtitle = "Multi-model × scenario mean shown for GCM series; SSM observed for baseline",
    theme    = theme(plot.title    = element_text(size = 10),
                     plot.subtitle = element_text(size = 8, colour = "#555555"))
  )

tiff(file.path(OUT_DIR, "figure3_inspect_timeseries.tif"),
     width = 9, height = 7, units = "in", res = 300, compression = "lzw")
print(inspect_ts)
dev.off()
cat("  Saved: figure3_inspect_timeseries.tif\n\n")


# =============================================================================
# 8.  COMPUTE DAILY VPD  (bias-corrected temperatures, raw hurs)
# =============================================================================

corrected_df <- corrected_df %>%
  mutate(vpd = calc_vpd(tmax_corr, tmin_corr, hurs))


# =============================================================================
# 9.  FILTER JULY → ANNUAL MEAN VPD
# =============================================================================

july_ann <- corrected_df %>%
  filter(month == JULY) %>%
  group_by(location, gcm, scenario, year) %>%
  summarise(vpd_mean = mean(vpd, na.rm = TRUE), .groups = "drop") %>%
  left_join(SCENARIOS, by = c("scenario" = "id"))

# Historical reference: multi-model spread per year × location
hist_band <- july_ann %>%
  filter(scenario == "historical") %>%
  group_by(location, year) %>%
  summarise(hist_med = median(vpd_mean),
            hist_lo  = min(vpd_mean),
            hist_hi  = max(vpd_mean),
            .groups  = "drop")

# Future scenario ribbons (multi-model spread)
fut_band <- july_ann %>%
  filter(scenario != "historical") %>%
  group_by(location, label, year) %>%
  summarise(vpd_med = median(vpd_mean),
            vpd_lo  = min(vpd_mean),
            vpd_hi  = max(vpd_mean),
            .groups = "drop")

fut_lines <- july_ann %>%
  filter(scenario != "historical")


# =============================================================================
# 10.  VPD PROJECTION PLOT
# =============================================================================

SCN_COLS <- c("SSP1-2.6" = "#2166AC",
              "SSP3-7.0" = "#F4A432",
              "SSP5-8.5" = "#D6372C")

make_panel <- function(loc_name, show_xlab = FALSE, show_legend = FALSE) {

  hb <- hist_band[hist_band$location == loc_name, ]
  fb <- fut_band [fut_band$location  == loc_name, ]
  gl <- fut_lines[fut_lines$location == loc_name, ]

  gg <- ggplot() +
    # Historical: grey ribbon + median line
    geom_ribbon(data = hb,
                aes(x = year, ymin = hist_lo, ymax = hist_hi),
                fill = "#BDBDBD", alpha = 0.45) +
    geom_line(data = hb,
              aes(x = year, y = hist_med),
              colour = "#636363", linewidth = 0.75) +
    # Individual GCM lines (thin, semi-transparent)
    geom_line(data = gl,
              aes(x = year, y = vpd_mean, colour = label,
                  group = interaction(gcm, label)),
              linewidth = 0.25, alpha = 0.30, show.legend = FALSE) +
    # Scenario uncertainty ribbon
    geom_ribbon(data = fb,
                aes(x = year, ymin = vpd_lo, ymax = vpd_hi, fill = label),
                alpha = 0.20) +
    # Multi-model median line
    geom_line(data = fb,
              aes(x = year, y = vpd_med, colour = label),
              linewidth = 0.90) +
    # Vertical rule at historical/future boundary
    geom_vline(xintercept = 2015, linetype = "dashed",
               colour = "#525252", linewidth = 0.4) +
    scale_colour_manual(name = "Scenario", values = SCN_COLS) +
    scale_fill_manual  (name = "Scenario", values = SCN_COLS) +
    scale_x_continuous(breaks = seq(1990, 2100, 20),
                       expand = expansion(mult = 0.01)) +
    scale_y_continuous(name = "July mean daily VPD (kPa)",
                       expand = expansion(mult = c(0.03, 0.06))) +
    labs(title = loc_name,
         x     = if (show_xlab) "Year" else NULL) +
    theme_classic(base_size = 9) +
    theme(
      plot.title         = element_text(size = 9, hjust = 0),
      axis.title.x       = element_text(size = 8),
      panel.grid.major.y = element_line(colour = "#E0E0E0", linewidth = 0.3),
      legend.position    = if (show_legend) "bottom" else "none",
      legend.key.size    = unit(0.45, "cm"),
      legend.text        = element_text(size = 7.5),
      legend.title       = element_text(size = 8)
    )
  gg
}

locs_plot <- unique(july_ann$location)
n_locs    <- length(locs_plot)

panels <- lapply(seq_along(locs_plot), function(i) {
  make_panel(locs_plot[i],
             show_xlab   = (i == n_locs),
             show_legend = (i == n_locs))
})

final_plot <- wrap_plots(panels, ncol = 1) +
  plot_annotation(
    caption = paste0(
      "ISIMIP3b W5E5 | GCMs: GFDL-ESM4, IPSL-CM6A-LR, MPI-ESM1-2-HR,",
      " MRI-ESM2-0, UKESM1-0-LL\n",
      "Bias-corrected (additive delta, monthly, per GCM × location, ",
      sprintf("overlap %d–%d) | ", BIAS_YR_MIN, BIAS_YR_MAX),
      "Ribbon = model spread (min–max); line = multi-model median"
    ),
    theme = theme(
      plot.caption = element_text(size = 6.5, colour = "#666666", hjust = 0)
    )
  )


# =============================================================================
# 11.  SAVE OUTPUTS
# =============================================================================

fig_height <- 3.8 * n_locs + 0.5

tiff(file.path(OUT_DIR, "figure3_isimip_vpd_projections.tif"),
     width = 7.5, height = fig_height, units = "in", res = 600,
     compression = "lzw")
print(final_plot)
dev.off()

pdf(file.path(OUT_DIR, "figure3_isimip_vpd_projections.pdf"),
    width = 7.5, height = fig_height)
print(final_plot)
dev.off()

cat("=== Done ===\n")
cat("  Inspection — monthly bias (raw):       figure3_inspect_raw.tif\n")
cat("  Inspection — monthly bias (corrected): figure3_inspect_corrected.tif\n")
cat("  Inspection — annual time series:       figure3_inspect_timeseries.tif\n")
cat("  Final figure (TIFF):                   figure3_isimip_vpd_projections.tif\n")
cat("  Final figure (PDF):                    figure3_isimip_vpd_projections.pdf\n")
