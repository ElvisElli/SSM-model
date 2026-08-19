# =============================================================================
# Figure 3 – ISIMIP3b Future VPD Projections
#
# Downloads ISIMIP3b bias-corrected climate data (W5E5) for two study
# locations and projects July mean daily VPD from 1985 through 2100
# across five GCMs and three SSP scenarios.
#
# Study locations (from SSM soybean project):
#   Rohwer, AR    lat = 33.8,  lon = -91.82
#   Lincoln, NE   lat = 40.8,  lon = -96.68
#
# GCMs (ISIMIP3b protocol):
#   GFDL-ESM4, IPSL-CM6A-LR, MPI-ESM1-2-HR, MRI-ESM2-0, UKESM1-0-LL
#
# Scenarios:
#   historical (1985-2014) | SSP1-2.6 | SSP3-7.0 | SSP5-8.5 (2015-2100)
#
# Variables used: tasmax [K], tasmin [K], hurs [%]
# Daily VPD (kPa): (es(Tmax) + es(Tmin)) / 2 × (1 − hurs/100)
#   where es(T °C) = 0.6108 × exp(17.27T / (237.3 + T))
#
# Download strategy
#   1. Check local CSV cache (ISIMIP portal naming convention).
#   2. If missing, call the ISIMIP data-portal API to submit a point-
#      extraction job; download the resulting CSV when the job completes.
#   3. If API extraction fails, download the full global NetCDF (~2.5 GB),
#      extract the grid-cell closest to the target lat/lon, save as CSV,
#      then delete the NetCDF to recover disk space.
#   Set SKIP_DOWNLOAD = TRUE below to work only with pre-cached CSVs.
#
# Output: r-model/outputs/plots/figure3_isimip_vpd_projections.tif
#         r-model/outputs/plots/figure3_isimip_vpd_projections.pdf
#
# Usage:
#   Rscript r-model/validation/figure3_isimip_vpd_projections.R
# =============================================================================


# =============================================================================
# 0.  USER CONFIGURATION
# =============================================================================

# Set to TRUE to skip all downloads and work only with cached CSVs.
# Recommended workflow: run download_isimip_data.py first (Python), then
# set SKIP_DOWNLOAD = TRUE here and run this R script for the analysis.
SKIP_DOWNLOAD <- TRUE

# ISIMIP files API endpoint (public, no API key required).
# API docs: https://files.isimip.org/api/v2/
ISIMIP_API_BASE <- "https://files.isimip.org/api/v2"

# Maximum seconds to wait for a portal extraction job to complete.
JOB_TIMEOUT_SEC <- 3600


# =============================================================================
# 1.  PACKAGES
# =============================================================================

needed <- c("httr", "jsonlite", "ggplot2", "patchwork", "dplyr", "tidyr",
            "RColorBrewer", "scales")
miss <- needed[!sapply(needed, requireNamespace, quietly = TRUE)]
if (length(miss) > 0) {
  install.packages(miss, repos = "https://cloud.r-project.org")
}
# ncdf4 is optional — needed only for the full-NetCDF fallback download
if (!requireNamespace("ncdf4", quietly = TRUE)) {
  message("ncdf4 not installed.  Fallback download of full NetCDF files will be disabled.")
  HAS_NCDF4 <- FALSE
} else {
  HAS_NCDF4 <- TRUE
}
suppressPackageStartupMessages({
  library(httr); library(jsonlite)
  library(ggplot2); library(patchwork); library(dplyr); library(tidyr)
  library(RColorBrewer); library(scales)
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
OUT_DIR   <- file.path(BASE_DIR, "outputs", "plots")
dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR,   recursive = TRUE, showWarnings = FALSE)

OUT_TIF <- file.path(OUT_DIR, "figure3_isimip_vpd_projections.tif")
OUT_PDF <- file.path(OUT_DIR, "figure3_isimip_vpd_projections.pdf")

cat("Base directory:", BASE_DIR, "\n")
cat("Cache directory:", CACHE_DIR, "\n")
cat("Output:", OUT_TIF, "\n\n")


# =============================================================================
# 3.  STUDY DESIGN
# =============================================================================

LOCATIONS <- data.frame(
  name    = c("Rohwer, AR",  "Lincoln, NE"),
  lat     = c(33.8,           40.8),
  lon     = c(-91.82,        -96.68),
  stringsAsFactors = FALSE
)

# GCM identifiers ---------------------------------------------------------
# col 'folder'  : directory name on files.isimip.org (CAPS)
# col 'slug'    : lowercase slug used in NetCDF filenames (with hyphens)
# col 'csv_slug': slug in ISIMIP portal CSV filenames (no hyphens)
# col 'variant' : CMIP6 ensemble member used for ISIMIP3b W5E5
GCMS <- data.frame(
  label    = c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR",
               "MRI-ESM2-0", "UKESM1-0-LL"),
  folder   = c("GFDL-ESM4", "IPSL-CM6A-LR", "MPI-ESM1-2-HR",
               "MRI-ESM2-0", "UKESM1-0-LL"),
  slug     = c("gfdl-esm4",   "ipsl-cm6a-lr",  "mpi-esm1-2-hr",
               "mri-esm2-0",  "ukesm1-0-ll"),
  csv_slug = c("gfdlesm4",    "ipslcm6alr",    "mpiesm12hr",
               "mriesm20",    "ukesm10ll"),
  variant  = c("r1i1p1f1",   "r1i1p1f1",      "r1i1p1f1",
               "r1i1p1f1",   "r1i1p1f2"),
  stringsAsFactors = FALSE
)

SCENARIOS <- data.frame(
  id    = c("historical", "ssp126",     "ssp370",     "ssp585"),
  label = c("Historical", "SSP1-2.6",  "SSP3-7.0",   "SSP5-8.5"),
  stringsAsFactors = FALSE
)

# Decade file periods available on ISIMIP3b servers -------------------------
HIST_PERIODS <- list(c(1985, 1994), c(1995, 2004), c(2005, 2014))

FUTURE_PERIODS <- list(
  c(2015, 2020), c(2021, 2030), c(2031, 2040), c(2041, 2050),
  c(2051, 2060), c(2061, 2070), c(2071, 2080), c(2081, 2090), c(2091, 2100)
)

VARIABLES <- c("tasmax", "tasmin", "hurs")

# Filter month for aggregation
JULY <- 7L


# =============================================================================
# 4.  HELPER FUNCTIONS
# =============================================================================

# Saturation vapour pressure (kPa)
es <- function(T_C) 0.6108 * exp(17.27 * T_C / (237.3 + T_C))

# Daily VPD from ISIMIP3b variables
#   tasmax / tasmin in Kelvin → convert to °C first
#   hurs in % (mean daily relative humidity)
calc_vpd_isimip <- function(tasmax_K, tasmin_K, hurs_pct) {
  Tmax <- tasmax_K - 273.15
  Tmin <- tasmin_K - 273.15
  es_mean <- (es(Tmax) + es(Tmin)) / 2
  pmax(es_mean * (1 - hurs_pct / 100), 0)
}

# Expected CSV cache filename (matches ISIMIP portal extraction convention)
isimip_csv_name <- function(gcm_csv_slug, variant, scenario, variable,
                             lat, lon, yr_start, yr_end) {
  sprintf("%s_%s_w5e5_%s_%s_lat%.4glon%.4g_daily_%d_%d.csv",
          gcm_csv_slug, variant, scenario, variable,
          lat, lon, yr_start, yr_end)
}

# Direct NetCDF download URL on files.isimip.org
isimip_nc_url <- function(gcm_slug, gcm_folder, variant, scenario, variable,
                           yr_start, yr_end) {
  base <- "https://files.isimip.org/ISIMIP3b/InputData/climate/atmosphere/bias-adjusted/global/daily"
  sprintf("%s/%s/%s/%s_%s_w5e5_%s_%s_global_daily_%d_%d.nc",
          base, scenario, gcm_folder,
          gcm_slug, variant, scenario, variable,
          yr_start, yr_end)
}


# =============================================================================
# 5.  DOWNLOAD FUNCTIONS
# =============================================================================

# ---------------------------------------------------------------------------
# 5a.  ISIMIP data-portal API: job-based point CSV extraction
#
#  The ISIMIP data portal (https://data.isimip.org) provides a REST API that
#  can cut out a single grid cell and return a daily CSV.  The workflow:
#    POST /api/v2/jobs/  →  receive {job_id, status}
#    GET  /api/v2/jobs/{id}/  →  poll until status == "finished"
#    GET  {download_url}  →  CSV in ISIMIP naming convention
#
#  Returns path to the downloaded CSV, or NULL on failure.
# ---------------------------------------------------------------------------
download_isimip_api <- function(gcm_slug, gcm_folder, variant, scenario,
                                variable, yr_start, yr_end,
                                lat, lon, out_path) {

  nc_path <- sprintf(
    "ISIMIP3b/InputData/climate/atmosphere/bias-adjusted/global/daily/%s/%s/%s_%s_w5e5_%s_%s_global_daily_%d_%d.nc",
    scenario, gcm_folder, gcm_slug, variant, scenario, variable, yr_start, yr_end)

  # POST body: paths + operations array (ISIMIP files API format)
  body <- list(
    paths      = list(nc_path),
    operations = list(list(
      operation  = "select_point",
      point      = list(lat, lon),   # [lat, lon]
      output_csv = TRUE
    ))
  )

  resp <- tryCatch(
    POST(paste0(ISIMIP_API_BASE, "/"),
         body = toJSON(body, auto_unbox = TRUE),
         content_type_json(),
         timeout(30)),
    error = function(e) { message("  API POST error: ", e$message); NULL }
  )

  if (is.null(resp) || http_error(resp)) {
    if (!is.null(resp))
      message("  API POST returned HTTP ", status_code(resp))
    return(NULL)
  }

  job <- content(resp, as = "parsed", type = "application/json")
  job_id <- job$id
  if (is.null(job_id)) {
    message("  API response contained no job id")
    return(NULL)
  }

  # Poll for completion
  t0 <- proc.time()["elapsed"]
  repeat {
    Sys.sleep(10)
    elapsed <- proc.time()["elapsed"] - t0
    if (elapsed > JOB_TIMEOUT_SEC) { message("  Job timed out"); return(NULL) }

    status_resp <- tryCatch(
      GET(paste0(ISIMIP_API_BASE, "/", job_id, "/"), timeout(30)),
      error = function(e) NULL
    )
    if (is.null(status_resp) || http_error(status_resp)) next

    status_obj <- content(status_resp, as = "parsed", type = "application/json")
    st <- status_obj$status
    if (st == "finished") {
      dl_url <- status_obj$file_url %||% status_obj$output_url
      # Some API versions nest URL in a 'files' list
      if (is.null(dl_url) && !is.null(status_obj$files))
        dl_url <- status_obj$files[[1]]$file_url %||% status_obj$files[[1]]$url
      if (is.null(dl_url)) { message("  No download URL in finished job"); return(NULL) }
      dl_resp <- tryCatch(GET(dl_url, write_disk(out_path, overwrite = TRUE), timeout(300)), error = function(e) NULL)
      if (is.null(dl_resp) || http_error(dl_resp)) { message("  CSV download failed"); return(NULL) }
      return(out_path)
    } else if (st %in% c("error", "failed")) {
      message("  Job failed on server: ", status_obj$error %||% "unknown")
      return(NULL)
    }
    # still pending / running → keep polling
    cat(sprintf("    [job %s] %.0f s elapsed, status = %s\r", job_id, elapsed, st))
  }
}

# Null-coalescing operator (like Python's `or`)
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------------------------------------------------------------------------
# 5b.  Fallback: download full global NetCDF, extract point, save CSV
#
#  Requires ncdf4 package and adequate disk space (~2.5 GB per file).
#  The NetCDF file is deleted after point extraction.
#  Returns path to the extracted CSV, or NULL on failure.
# ---------------------------------------------------------------------------
download_isimip_nc_extract <- function(nc_url, lat, lon, variable, out_path) {

  if (!HAS_NCDF4) {
    message("  ncdf4 not available – cannot use NetCDF fallback")
    return(NULL)
  }

  tmp_nc <- tempfile(fileext = ".nc")
  cat(sprintf("  Downloading full NetCDF (~2.5 GB): %s\n", basename(nc_url)))
  dl <- tryCatch(
    GET(nc_url,
        write_disk(tmp_nc, overwrite = TRUE),
        progress(),
        timeout(14400)),   # 4-hour timeout for large files
    error = function(e) { message("  Download error: ", e$message); NULL }
  )

  if (is.null(dl) || http_error(dl)) {
    unlink(tmp_nc)
    message("  NetCDF download failed (HTTP ", if (!is.null(dl)) status_code(dl) else "?", ")")
    return(NULL)
  }

  tryCatch({
    nc <- ncdf4::nc_open(tmp_nc)
    on.exit({ ncdf4::nc_close(nc); unlink(tmp_nc) }, add = TRUE)

    nc_lons <- ncdf4::ncvar_get(nc, "lon")
    nc_lats <- ncdf4::ncvar_get(nc, "lat")
    nc_time <- ncdf4::ncvar_get(nc, "time")

    # Time origin from units attribute
    time_units <- ncdf4::ncatt_get(nc, "time", "units")$value
    origin_str <- sub("days since ", "", time_units)
    origin_date <- as.Date(origin_str)
    dates <- origin_date + nc_time

    # Nearest grid-cell indices
    i_lon <- which.min(abs(nc_lons - lon))
    i_lat <- which.min(abs(nc_lats - lat))

    vals <- ncdf4::ncvar_get(nc, variable,
                             start = c(i_lon, i_lat, 1),
                             count = c(1, 1, -1))
    df <- data.frame(date = dates, value = as.numeric(vals))

    write.table(df, out_path, sep = ",", row.names = FALSE, col.names = FALSE,
                quote = FALSE)
    return(out_path)

  }, error = function(e) {
    message("  ncdf4 extraction error: ", e$message)
    return(NULL)
  })
}

# ---------------------------------------------------------------------------
# 5c.  Master download function: cache → API → NC fallback
# ---------------------------------------------------------------------------
get_isimip_csv <- function(gcm_row, scenario, variable, yr_start, yr_end,
                            lat, lon) {

  csv_name <- isimip_csv_name(gcm_row$csv_slug, gcm_row$variant,
                               scenario, variable, lat, lon, yr_start, yr_end)
  csv_path <- file.path(CACHE_DIR, csv_name)

  if (file.exists(csv_path)) {
    return(csv_path)
  }

  if (SKIP_DOWNLOAD) {
    return(NULL)   # user opted out of downloading
  }

  cat(sprintf("  Fetching: %s %s %s %d-%d\n",
              gcm_row$label, scenario, variable, yr_start, yr_end))

  # --- Attempt 1: ISIMIP data-portal API -----------------------------------
  result <- download_isimip_api(gcm_row$slug, gcm_row$folder, gcm_row$variant,
                                 scenario, variable, yr_start, yr_end,
                                 lat, lon, csv_path)

  # --- Attempt 2: full NetCDF download and extraction ----------------------
  if (is.null(result)) {
    nc_url <- isimip_nc_url(gcm_row$slug, gcm_row$folder, gcm_row$variant,
                             scenario, variable, yr_start, yr_end)
    result <- download_isimip_nc_extract(nc_url, lat, lon, variable, csv_path)
  }

  if (is.null(result)) {
    message(sprintf("  [SKIPPED] Could not obtain: %s", csv_name))
    message("  Manual option: download from https://data.isimip.org, select")
    message("  the file above, use 'Subset' for lat/lon, download as CSV,")
    message(sprintf("  and save to: %s", csv_path))
  }

  result
}


# =============================================================================
# 6.  READ CACHED CSV (ISIMIP portal format: YYYY-MM-DD,value, no header)
# =============================================================================

read_isimip_csv <- function(csv_path) {
  df <- tryCatch(
    read.csv(csv_path, header = FALSE, col.names = c("date_chr", "value"),
             colClasses = c("character", "numeric")),
    error = function(e) { message("  Read error: ", csv_path, " — ", e$message); NULL }
  )
  if (is.null(df)) return(NULL)
  df$date <- as.Date(df$date_chr)
  df[!is.na(df$date) & !is.na(df$value), c("date", "value")]
}


# =============================================================================
# 7.  MAIN DATA-COLLECTION LOOP
# =============================================================================

cat("=== Collecting ISIMIP3b data ===\n")

all_data <- list()   # will become a flat data.frame at the end

for (loc_i in seq_len(nrow(LOCATIONS))) {
  loc <- LOCATIONS[loc_i, ]
  cat(sprintf("\nLocation: %s (lat=%.2f, lon=%.2f)\n", loc$name, loc$lat, loc$lon))

  for (gcm_i in seq_len(nrow(GCMS))) {
    gcm <- GCMS[gcm_i, ]

    for (scen_i in seq_len(nrow(SCENARIOS))) {
      scenario <- SCENARIOS$id[scen_i]

      periods  <- if (scenario == "historical") HIST_PERIODS else FUTURE_PERIODS
      cat(sprintf("  %s / %s (%d periods)\n", gcm$label, scenario, length(periods)))

      # Containers for the three variables over all periods in this combo
      daily_tmax <- list()
      daily_tmin <- list()
      daily_hurs <- list()

      for (per in periods) {
        yr0 <- per[1]; yr1 <- per[2]

        paths <- lapply(VARIABLES, function(v) {
          get_isimip_csv(gcm, scenario, v, yr0, yr1, loc$lat, loc$lon)
        })
        names(paths) <- VARIABLES

        if (any(sapply(paths, is.null))) next   # skip if any variable missing

        raw <- lapply(VARIABLES, function(v) {
          d <- read_isimip_csv(paths[[v]])
          if (!is.null(d)) colnames(d)[2] <- v
          d
        })
        if (any(sapply(raw, is.null))) next

        # Merge the three variables on date
        mrg <- Reduce(function(a, b) merge(a, b, by = "date", all = FALSE), raw)
        if (nrow(mrg) == 0) next

        # Compute daily VPD
        mrg$vpd <- calc_vpd_isimip(mrg$tasmax, mrg$tasmin, mrg$hurs)
        mrg$year <- as.integer(format(mrg$date, "%Y"))
        mrg$month <- as.integer(format(mrg$date, "%m"))

        # Keep only July
        july_df <- mrg[mrg$month == JULY, c("date", "year", "vpd")]
        if (nrow(july_df) == 0) next

        all_data[[length(all_data) + 1]] <- data.frame(
          location = loc$name,
          gcm      = gcm$label,
          scenario = scenario,
          july_df,
          stringsAsFactors = FALSE
        )
      }   # end period loop
    }   # end scenario loop
  }   # end GCM loop
}   # end location loop

if (length(all_data) == 0) {
  stop(
    "No data were loaded.\n",
    "If SKIP_DOWNLOAD = TRUE, ensure pre-downloaded CSVs are in:\n  ", CACHE_DIR, "\n",
    "CSV naming pattern: {gcm_slug}_{variant}_w5e5_{scenario}_{variable}",
    "_lat{lat}lon{lon}_daily_{yr_start}_{yr_end}.csv\n",
    "Example: gfdlesm4_r1i1p1f1_w5e5_ssp370_tasmax_lat33.8lon-91.82_daily_2041_2050.csv"
  )
}

full_df <- bind_rows(all_data)
cat(sprintf("\nTotal daily July records: %d\n", nrow(full_df)))


# =============================================================================
# 8.  ANNUAL AGGREGATION
# =============================================================================

july_ann <- full_df %>%
  group_by(location, gcm, scenario, year) %>%
  summarise(vpd_mean = mean(vpd, na.rm = TRUE), .groups = "drop")

# Historical reference band: multi-model mean/min/max per year × location
hist_band <- july_ann %>%
  filter(scenario == "historical") %>%
  group_by(location, year) %>%
  summarise(
    hist_med = median(vpd_mean),
    hist_lo  = min(vpd_mean),
    hist_hi  = max(vpd_mean),
    .groups  = "drop"
  )

# Future scenario ribbons: multi-model spread
fut_band <- july_ann %>%
  filter(scenario != "historical") %>%
  group_by(location, scenario, year) %>%
  summarise(
    vpd_med = median(vpd_mean),
    vpd_lo  = min(vpd_mean),
    vpd_hi  = max(vpd_mean),
    .groups = "drop"
  ) %>%
  left_join(SCENARIOS, by = c("scenario" = "id"))

fut_lines <- july_ann %>%
  filter(scenario != "historical") %>%
  left_join(SCENARIOS, by = c("scenario" = "id"))


# =============================================================================
# 9.  PLOT
# =============================================================================

# Scenario colours
SCN_COLS <- c("SSP1-2.6" = "#2166AC",
              "SSP3-7.0" = "#F4A432",
              "SSP5-8.5" = "#D6372C")

# Build a panel for one location
make_panel <- function(loc_name, show_legend = FALSE) {

  hb  <- hist_band [hist_band$location  == loc_name, ]
  fb  <- fut_band  [fut_band$location   == loc_name, ]
  fl  <- fut_lines [fut_lines$location  == loc_name, ]

  # Individual GCM lines for one scenario layer — subtle
  gcm_lines <- july_ann %>%
    filter(location == loc_name, scenario != "historical") %>%
    left_join(SCENARIOS, by = c("scenario" = "id"))

  gg <- ggplot() +

    # Historical ribbon (grey) + median line
    geom_ribbon(data = hb,
                aes(x = year, ymin = hist_lo, ymax = hist_hi),
                fill = "#BDBDBD", alpha = 0.45) +
    geom_line(data = hb,
              aes(x = year, y = hist_med),
              colour = "#636363", linewidth = 0.7) +

    # Thin individual GCM lines
    geom_line(data = gcm_lines,
              aes(x = year, y = vpd_mean, colour = label, group = interaction(gcm, label)),
              linewidth = 0.25, alpha = 0.35, show.legend = FALSE) +

    # Scenario ribbons (model spread)
    geom_ribbon(data = fb,
                aes(x = year, ymin = vpd_lo, ymax = vpd_hi, fill = label),
                alpha = 0.20) +

    # Scenario median lines
    geom_line(data = fb,
              aes(x = year, y = vpd_med, colour = label),
              linewidth = 0.9) +

    # Vertical rule at 2015
    geom_vline(xintercept = 2015, linetype = "dashed", colour = "#525252",
               linewidth = 0.4) +

    scale_colour_manual(name = "Scenario", values = SCN_COLS) +
    scale_fill_manual  (name = "Scenario", values = SCN_COLS) +
    scale_x_continuous(breaks = seq(1990, 2100, 20), expand = expansion(mult = 0.01)) +
    scale_y_continuous(name = "July mean daily VPD (kPa)",
                       expand = expansion(mult = c(0.03, 0.06))) +
    labs(title = loc_name) +
    theme_classic(base_size = 9) +
    theme(
      plot.title     = element_text(size = 9, hjust = 0),
      axis.title.x   = element_blank(),
      panel.grid.major.y = element_line(colour = "#E0E0E0", linewidth = 0.3),
      legend.position = if (show_legend) "bottom" else "none",
      legend.key.size = unit(0.45, "cm"),
      legend.text    = element_text(size = 7.5),
      legend.title   = element_text(size = 8)
    )

  gg
}

locs_plot <- unique(july_ann$location)   # use only locations that have data

panels <- lapply(seq_along(locs_plot), function(i) {
  make_panel(locs_plot[i], show_legend = (i == length(locs_plot)))
})

combined <- wrap_plots(panels, ncol = 1) +
  plot_annotation(
    caption = paste0(
      "ISIMIP3b W5E5 bias-corrected data | GCMs: GFDL-ESM4, IPSL-CM6A-LR, ",
      "MPI-ESM1-2-HR, MRI-ESM2-0, UKESM1-0-LL\n",
      "Ribbon = model spread (min–max); bold line = multi-model median | ",
      "VPD = (es(Tmax)+es(Tmin))/2 × (1−RH/100)"
    ),
    theme = theme(plot.caption = element_text(size = 6.5, colour = "#666666",
                                               hjust = 0))
  ) &
  theme(axis.title.x = element_blank())

x_lab <- ggplot(data.frame(x = 0, y = 0), aes(x, y)) +
  labs(x = "Year") +
  theme_void() +
  theme(axis.title.x = element_text(size = 9))

final_plot <- combined / x_lab + plot_layout(heights = c(30, 1))


# =============================================================================
# 10.  SAVE OUTPUT
# =============================================================================

cat("\nSaving output...\n")

# TIFF (publication)
if (requireNamespace("ragg", quietly = TRUE)) {
  ragg::agg_tiff(OUT_TIF, width = 7.5, height = 3.8 * length(panels) + 0.6,
                 units = "in", res = 600, compression = "lzw")
} else {
  tiff(OUT_TIF, width = 7.5, height = 3.8 * length(panels) + 0.6,
       units = "in", res = 600, compression = "lzw")
}
print(final_plot)
dev.off()

# PDF (vector)
pdf(OUT_PDF, width = 7.5, height = 3.8 * length(panels) + 0.6)
print(final_plot)
dev.off()

cat("Saved:", OUT_TIF, "\n")
cat("Saved:", OUT_PDF, "\n")
cat("\nDone.\n")
