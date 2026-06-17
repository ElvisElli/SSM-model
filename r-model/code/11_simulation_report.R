# =============================================================================
# SSM Soybean Model - Simulation Summary Report
# =============================================================================
# Reads all_results.csv (and weather files) and writes a self-contained HTML
# report with environmental characterisation figures, diurnal VPD analysis,
# VPD threshold summary tables, and simulation summary statistics.
#
# Usage:
#   Rscript 11_simulation_report.R
#   source("11_simulation_report.R")
#   generate_report(out_html = "my_report.html")
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
})

# Install base64enc quietly if not present
if (!requireNamespace("base64enc", quietly = TRUE))
  install.packages("base64enc", repos = "https://cloud.r-project.org", quiet = TRUE)
library(base64enc)

# ── Locate r-model base directory ─────────────────────────────────────────────
if (!exists("BASE_DIR") || !dir.exists(BASE_DIR)) {
  BASE_DIR <- tryCatch({
    script_path <- NULL
    for (i in seq_len(sys.nframe())) {
      ofile <- sys.frame(i)$ofile
      if (!is.null(ofile) && nchar(ofile) > 0) {
        script_path <- normalizePath(ofile, mustWork = FALSE); break
      }
    }
    if (!is.null(script_path)) {
      d <- dirname(script_path)
      if (basename(d) == "code") dirname(d) else d
    } else {
      cwd <- getwd()
      if      (file.exists(file.path(cwd, "inputs/scenarios.csv")))         cwd
      else if (file.exists(file.path(cwd, "r-model/inputs/scenarios.csv"))) file.path(cwd, "r-model")
      else if (file.exists(file.path(cwd, "../inputs/scenarios.csv")))       normalizePath(file.path(cwd, ".."), mustWork = FALSE)
      else stop("Cannot find r-model base directory.")
    }
  }, error = function(e) stop(e$message))
}

RESULTS_DIR <- file.path(BASE_DIR, "outputs", "results")
DAILY_DIR   <- file.path(RESULTS_DIR, "daily")
WEATHER_DIR <- file.path(BASE_DIR, "inputs", "weather")
INPUT_DIR   <- file.path(BASE_DIR, "inputs")


# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

fmt1 <- function(x) formatC(round(as.numeric(x), 1), format = "f", digits = 1)
fmt0 <- function(x) formatC(round(as.numeric(x), 0), format = "f", digits = 0, big.mark = ",")
fmtp <- function(x) paste0(formatC(round(as.numeric(x) * 100, 1), format = "f", digits = 1), " %")

fnum <- function(vals, digits = 1) {
  clean <- as.numeric(vals[!is.na(vals) & vals != "" & vals != "NA"])
  if (!length(clean)) return("—")
  m <- mean(clean); s <- sd(clean)
  sprintf(paste0("%.", digits, "f ± %.", digits, "f"), m, s)
}
fmean <- function(vals, digits = 1) {
  clean <- as.numeric(vals[!is.na(vals) & vals != "" & vals != "NA"])
  if (!length(clean)) return("—")
  sprintf(paste0("%.", digits, "f"), mean(clean))
}

df_to_html <- function(d, id = "", cls = "tbl") {
  id_attr <- if (nzchar(id)) sprintf(' id="%s"', id) else ""
  header  <- paste(sprintf("<th>%s</th>", names(d)), collapse = "")
  rows    <- apply(d, 1, function(r) {
    cells <- paste(sprintf("<td>%s</td>", r), collapse = "")
    sprintf("<tr>%s</tr>", cells)
  })
  sprintf('<table%s class="%s"><thead><tr>%s</tr></thead><tbody>%s</tbody></table>',
          id_attr, cls, header, paste(rows, collapse = "\n"))
}

# Embed a PNG file as a base64 data URI
png_to_b64 <- function(path) {
  b64 <- base64encode(path)
  sprintf("data:image/png;base64,%s", b64)
}

# Wrapper: run plotting code, capture to temp PNG, return base64 URI
make_plot_b64 <- function(plot_fun, width = 1200, height = 700, res = 130) {
  tmp <- tempfile(fileext = ".png")
  png(tmp, width = width, height = height, res = res)
  tryCatch(plot_fun(), finally = dev.off())
  uri <- png_to_b64(tmp)
  unlink(tmp)
  uri
}


# =============================================================================
# DAYLENGTH & HOURLY VPD
# =============================================================================

calc_dayl <- function(doy, lat) {
  Pi <- pi; RDN <- Pi / 180
  DEC <- sin(23.45 * RDN) * cos(2 * Pi * (doy + 10) / 365)
  DEC <- atan(DEC / sqrt(pmax(1e-9, 1 - DEC^2))) * (-1)
  SINLD <- sin(RDN * lat) * sin(DEC)
  COSLD <- cos(RDN * lat) * cos(DEC)
  AOBs  <- pmax(-0.9999, pmin(0.9999, SINLD / COSLD))
  DAYL  <- 12 * (1 + 2 * atan(AOBs / sqrt(pmax(1e-9, 1 - AOBs^2))) / Pi)
  list(DAYL = DAYL, SUNRIS = 12 - DAYL / 2, SUNSET = 12 + DAYL / 2)
}

# Compute hourly VPD matrix: rows = days, cols = hours 1..24.
# Night-time hours are set to 0. Uses next-day Tmin for afternoon limb.
compute_vpd_matrix <- function(wth, lat, vpdf = 0.75) {
  n    <- nrow(wth)
  Pi   <- pi
  P    <- 1.5  # twilight correction
  TMIN <- as.numeric(wth$TMIN)
  TMAX <- as.numeric(wth$TMAX)
  DOY  <- as.integer(wth$DOY)
  TMINA <- c(TMIN[-1], TMIN[n])          # next day's Tmin

  dl     <- calc_dayl(DOY, lat)
  DAYL   <- dl$DAYL; SUNRIS <- dl$SUNRIS; SUNSET <- dl$SUNSET
  VPTMIN <- 0.6108 * exp(17.27 * TMIN / (237.3 + TMIN))

  vpd_mat <- matrix(0.0, nrow = n, ncol = 24)
  for (H in 1:24) {
    angle  <- sin(Pi * (H - SUNRIS) / (DAYL + 2 * P))
    if (H < 13.5) {
      TEMP1 <- TMIN  + (TMAX  - TMIN)  * angle
    } else {
      TEMP1 <- TMINA + (TMAX  - TMINA) * angle
    }
    VPTEMP <- 0.6108 * exp(17.27 * TEMP1 / (237.3 + TEMP1))
    VPD1   <- pmax((VPTEMP - VPTMIN) * (vpdf / 0.75), 0)
    day_h  <- H > SUNRIS & H < SUNSET
    VPD1[!day_h] <- 0
    vpd_mat[, H] <- VPD1
  }
  vpd_mat
}

# Read weather file (matches read_weather from 01_read_inputs.R)
read_wth <- function(filepath) {
  wth <- suppressMessages(read_excel(filepath, skip = 9, col_names = TRUE))
  colnames(wth)[1:6] <- c("YEAR", "DOY", "SRAD", "TMAX", "TMIN", "RAIN")
  wth <- wth[, 1:6]
  wth <- wth[!is.na(wth$YEAR), ]
  wth$YEAR <- as.integer(wth$YEAR)
  wth$DOY  <- as.integer(wth$DOY)
  wth
}


# =============================================================================
# ENVIRONMENTAL FIGURE A: Growing-season climate by location (from results)
# =============================================================================

plot_env_season <- function(df) {
  locs   <- sort(unique(df$Location))
  n_loc  <- length(locs)
  cols   <- c("#1a5276","#2e86c1","#1e8449","#d35400","#c0392b",
              "#7d3c98","#148f77","#d4ac0d","#5d6d7e","#873600")
  names(cols) <- locs

  # Use non-LT (check) cultivar + rainfed only for cleaner comparison
  sub <- df[!grepl("LT",  df$Crop,  ignore.case = FALSE) &
             grepl("RFD", df$Manag, ignore.case = TRUE), ]

  par(mfrow = c(2, 3), mar = c(4, 4, 2.5, 1.5), oma = c(0, 0, 3, 0),
      bg = "white", fg = "#333333", col.axis = "#333333", col.lab = "#333333")

  # Panel 1: Mean Tmin during season
  boxplot(MTMINT ~ Location, data = sub, las = 2, col = cols[locs],
          main = "Mean seasonal Tmin (°C)",
          ylab = "Mean Tmin (°C)", xlab = "", names = abbreviate(locs, 6),
          border = "grey40", outline = FALSE, cex.axis = 0.75)
  abline(h = mean(sub$MTMINT, na.rm = TRUE), lty = 2, col = "#2e86c1", lwd = 1)

  # Panel 2: Mean Tmax during season
  boxplot(MTMAXT ~ Location, data = sub, las = 2, col = cols[locs],
          main = "Mean seasonal Tmax (°C)",
          ylab = "Mean Tmax (°C)", xlab = "", names = abbreviate(locs, 6),
          border = "grey40", outline = FALSE, cex.axis = 0.75)
  abline(h = mean(sub$MTMAXT, na.rm = TRUE), lty = 2, col = "#c0392b", lwd = 1)

  # Panel 3: Total SRAD during season
  boxplot(SSRADT ~ Location, data = sub, las = 2, col = cols[locs],
          main = "Seasonal total solar radiation (MJ/m²)",
          ylab = "Total SRAD (MJ/m²)", xlab = "", names = abbreviate(locs, 6),
          border = "grey40", outline = FALSE, cex.axis = 0.75)

  # Panel 4: Seasonal rainfall
  boxplot(SRAINT ~ Location, data = sub, las = 2, col = cols[locs],
          main = "Seasonal total rainfall (mm)",
          ylab = "CRAIN (mm)", xlab = "", names = abbreviate(locs, 6),
          border = "grey40", outline = FALSE, cex.axis = 0.75)

  # Panel 5: Season length (R8 DAP)
  boxplot(R8 ~ Location, data = sub, las = 2, col = cols[locs],
          main = "Season length — days to R8 (DAP)",
          ylab = "Days to R8 (DAP)", xlab = "", names = abbreviate(locs, 6),
          border = "grey40", outline = FALSE, cex.axis = 0.75)

  # Panel 6: Yield by location
  boxplot(Ywet ~ Location, data = sub, las = 2, col = cols[locs],
          main = "Grain yield — rainfed check (kg/ha)",
          ylab = "Ywet (kg/ha)", xlab = "", names = abbreviate(locs, 6),
          border = "grey40", outline = FALSE, cex.axis = 0.75)

  mtext("Growing-Season Climate and Yield by Location  (rainfed check cultivar, all years)",
        outer = TRUE, cex = 1.0, font = 2, col = "#1a5276")
}


# =============================================================================
# ENVIRONMENTAL FIGURE B: Monthly climatology from weather files
# =============================================================================

plot_monthly_clim <- function(loc_info, weather_dir) {
  n_loc <- nrow(loc_info)
  nc    <- 5; nr <- ceiling(n_loc / nc)

  par(mfrow = c(nr, nc), mar = c(3, 3.5, 2, 3), oma = c(1, 1, 3.5, 1),
      bg = "white", fg = "#333333", col.axis = "#333333", col.lab = "#333333")

  month_lbl <- c("J","F","M","A","M","J","J","A","S","O","N","D")

  for (i in seq_len(n_loc)) {
    info     <- loc_info[i, ]
    wth_path <- file.path(weather_dir, info$wth_file)
    if (!file.exists(wth_path)) { plot.new(); title(info$loc_name); next }

    wth <- read_wth(wth_path)
    wth$month <- as.integer(format(as.Date(paste(wth$YEAR, wth$DOY), "%Y %j"), "%m"))

    n_years  <- length(unique(wth$YEAR))
    monthly  <- wth %>%
      group_by(month) %>%
      summarise(
        tmin = mean(TMIN,          na.rm = TRUE),
        tmax = mean(TMAX,          na.rm = TRUE),
        rain = sum(RAIN, na.rm = TRUE) / n_years,
        .groups = "drop"
      ) %>% arrange(month)

    m   <- monthly$month
    rng <- range(c(monthly$tmin, monthly$tmax), na.rm = TRUE)

    # Bar chart: rainfall
    bp <- barplot(monthly$rain, col = "#aed6f1", border = "white",
                  axes = FALSE, axisnames = FALSE,
                  ylim = c(0, max(monthly$rain, na.rm = TRUE) * 1.3))
    axis(4, las = 1, col.axis = "#2e86c1", cex.axis = 0.68, col = "#2e86c1")
    mtext("Rain (mm)", side = 4, line = 2, cex = 0.55, col = "#2e86c1")

    # Overlay temperature lines
    par(new = TRUE)
    plot(bp, monthly$tmax, type = "l", lwd = 2, col = "#c0392b",
         ylim = rng + c(-3, 5), axes = FALSE, xlab = "", ylab = "")
    lines(bp, monthly$tmin, lwd = 2, col = "#1a5276")
    axis(2, las = 1, cex.axis = 0.68, col.axis = "#333333")
    axis(1, at = bp, labels = month_lbl, cex.axis = 0.65, col.axis = "#333333")
    mtext("Temp (°C)", side = 2, line = 2.2, cex = 0.55)

    box(col = "#dee2e6")
    title(main = info$loc_name, cex.main = 0.88, font.main = 2, col.main = "#1a5276")

    if (i == 1)
      legend("topright", legend = c("Tmax","Tmin","Rain"),
             lty = c(1,1,NA), pch = c(NA,NA,15),
             col = c("#c0392b","#1a5276","#aed6f1"), lwd = 2,
             cex = 0.62, bty = "n")
  }

  mtext("Monthly Climatology by Location  (mean over all simulation years)",
        outer = TRUE, cex = 1.0, font = 2, col = "#1a5276")
}


# =============================================================================
# VPD ANALYSIS: compute mean diurnal VPD by location × planting window × stage
# =============================================================================

compute_vpd_profiles <- function(df, loc_info, weather_dir) {
  # df: all_results.csv data frame; loc_info: location metadata
  # Returns a list: by_loc[[loc]] = data.frame with cols:
  #   plant_win, stage, H (hour 1:24), vpd_mean

  # Work with check cultivar only (phenology same across cultivars)
  sub <- df[!grepl("LT", df$Crop, ignore.case = FALSE), ]

  # Identify planting window from Manag
  win_label <- function(m) {
    m <- toupper(m)
    if (grepl("ELY", m)) "Early"
    else if (grepl("MID", m)) "Mid"
    else if (grepl("LTE", m)) "Late"
    else "Other"
  }
  sub$PlantWin <- sapply(sub$Manag, win_label)

  all_profiles <- list()

  for (i in seq_len(nrow(loc_info))) {
    info  <- loc_info[i, ]
    loc   <- info$loc_name
    lat   <- as.numeric(info$lat)
    vpdf  <- as.numeric(info$vpdf)
    wpath <- file.path(weather_dir, info$wth_file)

    cat(sprintf("  VPD profiles: %s\n", loc))
    if (!file.exists(wpath)) next

    wth     <- read_wth(wpath)
    vpd_mat <- compute_vpd_matrix(wth, lat, vpdf)   # n_days × 24

    loc_sub <- sub[sub$Location == loc, ]

    for (win in c("Early","Mid","Late")) {
      win_sub <- loc_sub[loc_sub$PlantWin == win, ]
      if (nrow(win_sub) == 0) next

      veg_acc   <- numeric(24); veg_n   <- 0L
      repro_acc <- numeric(24); repro_n <- 0L

      for (k in seq_len(nrow(win_sub))) {
        row    <- win_sub[k, ]
        yr     <- as.integer(row$Pyear)
        Pdoy   <- as.integer(row$Pdoy)
        EMR_dap <- as.integer(row$dtEMR)  # days to emergence (from planting)
        R3_dap  <- as.integer(row$R3)     # DAP to R3 (beginning pod)
        R7_dap  <- as.integer(row$R7)     # DAP to R7 (physiological maturity)
        if (is.na(EMR_dap) || EMR_dap <= 0) EMR_dap <- 0L
        if (is.na(R3_dap)  || R3_dap  <= 0) next
        if (is.na(R7_dap)  || R7_dap  <= 0) next

        # DOY ranges: veg = emergence to R3, repro = R3 to R7
        emr_doy    <- Pdoy + EMR_dap
        veg_doys   <- seq(emr_doy, Pdoy + R3_dap - 1)
        repro_doys <- seq(Pdoy + R3_dap, Pdoy + R7_dap - 1)

        idx_v <- which(as.integer(wth$YEAR) == yr & as.integer(wth$DOY) %in% veg_doys)
        idx_r <- which(as.integer(wth$YEAR) == yr & as.integer(wth$DOY) %in% repro_doys)

        if (length(idx_v) > 0) {
          veg_acc <- veg_acc + colSums(vpd_mat[idx_v, , drop = FALSE])
          veg_n   <- veg_n   + length(idx_v)
        }
        if (length(idx_r) > 0) {
          repro_acc <- repro_acc + colSums(vpd_mat[idx_r, , drop = FALSE])
          repro_n   <- repro_n   + length(idx_r)
        }
      }

      if (veg_n > 0)
        all_profiles[[length(all_profiles) + 1]] <-
          data.frame(Location = loc, PlantWin = win, Stage = "Vegetative",
                     Hour = 1:24, VPD = veg_acc / veg_n)
      if (repro_n > 0)
        all_profiles[[length(all_profiles) + 1]] <-
          data.frame(Location = loc, PlantWin = win, Stage = "Reproductive",
                     Hour = 1:24, VPD = repro_acc / repro_n)
    }
  }

  do.call(rbind, all_profiles)
}


# =============================================================================
# VPD FIGURE: one PNG per location, 3 panels (sowing dates) × 2 lines (stages)
# phen_info: data.frame with cols Location, PlantWin, emr, r3, r7 (mean DAP)
# Saves to daily_dir; returns named vector of file paths.
# =============================================================================

plot_vpd_diurnal <- function(vpd_prof, daily_dir, phen_info, threshold = 2.0) {
  locs <- sort(unique(vpd_prof$Location))

  stg_cols <- c(Vegetative = "#2e86c1", Reproductive = "#e74c3c")
  stg_lty  <- c(Vegetative = 1,         Reproductive = 2)
  stg_pch  <- c(Vegetative = 19,        Reproductive = 17)
  wins     <- c("Early", "Mid", "Late")
  win_labs <- c(Early = "Early planting", Mid = "Mid planting", Late = "Late planting")

  y_max <- max(vpd_prof$VPD, na.rm = TRUE) * 1.30  # extra room for legend

  dir.create(daily_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- character(length(locs))
  names(paths) <- locs

  for (loc in locs) {
    sub  <- vpd_prof[vpd_prof$Location == loc, ]
    safe <- gsub("[^A-Za-z0-9_]", "_", loc)
    fpath <- file.path(daily_dir, paste0("VPD_diurnal_", safe, ".png"))

    png(fpath, width = 3000, height = 1100, res = 200)
    par(mfrow = c(1, 3),
        mar   = c(5, 5, 4, 2),
        oma   = c(0, 0, 4.5, 0),
        bg    = "white", fg = "#333333",
        col.axis = "#333333", col.lab = "#333333")

    for (win in wins) {
      wsub <- sub[sub$PlantWin == win, ]

      # Get mean DAP for this location × planting window
      ph <- phen_info[phen_info$Location == loc & phen_info$PlantWin == win, ]
      if (nrow(ph) > 0) {
        emr_d <- ph$emr[1]; r3_d <- ph$r3[1]; r7_d <- ph$r7[1]
        leg_veg   <- sprintf("Vegetative  (emergence to R3, avg DAP %d to %d)",
                             emr_d, r3_d)
        leg_repro <- sprintf("Reproductive  (R3 to R7, avg DAP %d to %d)",
                             r3_d, r7_d)
      } else {
        leg_veg   <- "Vegetative (emergence to R3)"
        leg_repro <- "Reproductive (R3 to R7)"
      }

      plot(NA, xlim = c(1, 24), ylim = c(0, y_max),
           xlab = "Hour of day", ylab = "Mean VPD (kPa)",
           xaxt = "n", cex.axis = 0.85, cex.lab = 0.90,
           main = win_labs[win], cex.main = 1.0, font.main = 2,
           col.main = "#1a5276")
      axis(1, at = c(6, 9, 12, 15, 18, 21), cex.axis = 0.82)
      abline(v = c(6, 9, 12, 15, 18), col = "#e0e0e0", lty = 1, lwd = 0.5)
      abline(h = seq(0, ceiling(y_max * 2) / 2, by = 0.5),
             col = "#e0e0e0", lty = 1, lwd = 0.4)
      abline(h = threshold, lty = 3, col = "grey40", lwd = 1.8)
      text(23.5, threshold + y_max * 0.025, paste0(threshold, " kPa"),
           col = "grey40", cex = 0.72, adj = 1)

      for (stg in c("Vegetative", "Reproductive")) {
        d <- wsub[wsub$Stage == stg, ]
        if (nrow(d) == 0) next
        d <- d[order(d$Hour), ]
        lines(d$Hour, d$VPD, col = stg_cols[stg], lty = stg_lty[stg], lwd = 2.2)
        pts <- d$Hour %in% c(8, 13, 18)
        points(d$Hour[pts], d$VPD[pts],
               col = stg_cols[stg], pch = stg_pch[stg], cex = 1.0)
      }

      # Full legend in every panel (uses the per-window day ranges)
      legend("topleft",
             legend = c(leg_veg, leg_repro,
                        paste0(threshold, " kPa threshold (VPDcr LT2)")),
             col    = c(stg_cols["Vegetative"], stg_cols["Reproductive"], "grey40"),
             lty    = c(stg_lty["Vegetative"],  stg_lty["Reproductive"],  3),
             pch    = c(stg_pch["Vegetative"],  stg_pch["Reproductive"],  NA),
             lwd    = c(2.2, 2.2, 1.8), cex = 0.72, bty = "n", pt.cex = 1.0)
    }

    mtext(paste0("Mean Diurnal VPD Profile — ", loc),
          outer = TRUE, cex = 1.15, font = 2, col = "#1a5276", line = 2.0)
    dev.off()
    paths[loc] <- fpath
  }

  invisible(paths)
}


# =============================================================================
# VPD THRESHOLD TABLES — cumulative hours/days per stage × planting window
# Uses actual phenological windows (emerg→R3 veg, R3→R7 repro, emerg→R7 total)
# Columns: Location | Early-Veg | Early-Repro | Early-Total | Mid-... | Late-...
# =============================================================================

compute_vpd_tables <- function(df, loc_info, weather_dir, threshold = 2.0) {
  sub <- df[!grepl("LT", df$Crop, ignore.case = FALSE), ]

  win_label <- function(m) {
    m <- toupper(m)
    if (grepl("ELY", m)) "Early"
    else if (grepl("MID", m)) "Mid"
    else if (grepl("LTE", m)) "Late"
    else "Other"
  }
  sub$PlantWin <- sapply(sub$Manag, win_label)

  rows_hours <- list()
  rows_days  <- list()

  for (i in seq_len(nrow(loc_info))) {
    info  <- loc_info[i, ]
    loc   <- info$loc_name
    lat   <- as.numeric(info$lat)
    vpdf  <- as.numeric(info$vpdf)
    wpath <- file.path(weather_dir, info$wth_file)
    if (!file.exists(wpath)) next

    wth     <- read_wth(wpath)
    vpd_mat <- compute_vpd_matrix(wth, lat, vpdf)
    loc_sub <- sub[sub$Location == loc, ]

    row_h <- list(Location = loc)
    row_d <- list(Location = loc)

    for (win in c("Early","Mid","Late")) {
      win_sub <- loc_sub[loc_sub$PlantWin == win, ]
      if (nrow(win_sub) == 0) {
        for (stg in c("Veg","Repro","Total")) {
          row_h[[paste0(win,".",stg)]] <- "—"
          row_d[[paste0(win,".",stg)]] <- "—"
        }
        next
      }

      h_veg <- numeric(0); h_rep <- numeric(0); h_tot <- numeric(0)
      d_veg <- numeric(0); d_rep <- numeric(0); d_tot <- numeric(0)

      for (k in seq_len(nrow(win_sub))) {
        r      <- win_sub[k, ]
        yr     <- as.integer(r$Pyear)
        Pdoy   <- as.integer(r$Pdoy)
        EMR    <- as.integer(r$dtEMR); if (is.na(EMR) || EMR < 0) EMR <- 0L
        R3_dap <- as.integer(r$R3)
        R7_dap <- as.integer(r$R7)
        if (is.na(R3_dap) || R3_dap <= 0 || is.na(R7_dap) || R7_dap <= 0) next

        emr_doy <- Pdoy + EMR
        r3_doy  <- Pdoy + R3_dap
        r7_doy  <- Pdoy + R7_dap - 1L

        get_idx <- function(d1, d2) {
          which(as.integer(wth$YEAR) == yr &
                as.integer(wth$DOY) >= d1 & as.integer(wth$DOY) <= d2)
        }
        vpd_sum_h <- function(idx) if (length(idx)) sum(vpd_mat[idx,,drop=FALSE] > threshold) else 0
        vpd_sum_d <- function(idx) if (length(idx)) sum(rowSums(vpd_mat[idx,,drop=FALSE] > threshold) > 0) else 0

        iv <- get_idx(emr_doy, r3_doy - 1L)
        ir <- get_idx(r3_doy,  r7_doy)
        it <- get_idx(emr_doy, r7_doy)

        h_veg <- c(h_veg, vpd_sum_h(iv))
        h_rep <- c(h_rep, vpd_sum_h(ir))
        h_tot <- c(h_tot, vpd_sum_h(it))
        d_veg <- c(d_veg, vpd_sum_d(iv))
        d_rep <- c(d_rep, vpd_sum_d(ir))
        d_tot <- c(d_tot, vpd_sum_d(it))
      }

      fmt_h <- function(v) if (length(v)) sprintf("%.0f ± %.0f", mean(v), sd(v)) else "—"
      fmt_d <- function(v) if (length(v)) sprintf("%.0f ± %.0f", mean(v), sd(v)) else "—"

      row_h[[paste0(win,".Veg")]]   <- fmt_h(h_veg)
      row_h[[paste0(win,".Repro")]] <- fmt_h(h_rep)
      row_h[[paste0(win,".Total")]] <- fmt_h(h_tot)
      row_d[[paste0(win,".Veg")]]   <- fmt_d(d_veg)
      row_d[[paste0(win,".Repro")]] <- fmt_d(d_rep)
      row_d[[paste0(win,".Total")]] <- fmt_d(d_tot)
    }

    rows_hours[[length(rows_hours) + 1]] <- as.data.frame(row_h, stringsAsFactors = FALSE)
    rows_days[[length(rows_days)   + 1]] <- as.data.frame(row_d, stringsAsFactors = FALSE)
  }

  col_names <- c("Location",
                 "Early — Veg.", "Early — Repro.", "Early — Total",
                 "Mid — Veg.",   "Mid — Repro.",   "Mid — Total",
                 "Late — Veg.",  "Late — Repro.",  "Late — Total")

  hours_df <- do.call(rbind, rows_hours)
  days_df  <- do.call(rbind, rows_days)
  names(hours_df) <- col_names
  names(days_df)  <- col_names

  list(hours = hours_df, days = days_df)
}


# =============================================================================
# MAIN REPORT FUNCTION
# =============================================================================

generate_report <- function(results_file = NULL, out_html = NULL) {

  if (is.null(results_file))
    results_file <- file.path(RESULTS_DIR, "all_results.csv")
  if (is.null(out_html))
    out_html <- file.path(RESULTS_DIR, "simulation_report.html")

  if (!file.exists(results_file))
    stop("Yearly results file not found: ", results_file,
         "\nRun 08_run_model.R first.")

  cat("Reading results:", results_file, "\n")
  df <- read.csv(results_file, stringsAsFactors = FALSE)
  cat(sprintf("  %d rows × %d columns\n", nrow(df), ncol(df)))

  # Location metadata from scenarios.csv
  scn_file <- file.path(INPUT_DIR, "scenarios.csv")
  loc_info <- data.frame()
  if (file.exists(scn_file)) {
    scn      <- read.csv(scn_file, stringsAsFactors = FALSE)
    loc_info <- unique(scn[, c("loc_name","lat","vpdf","wth_file")])
    loc_info <- loc_info[!duplicated(loc_info$loc_name), ]
    loc_info <- loc_info[order(loc_info$loc_name), ]
  }

  has_weather <- nrow(loc_info) > 0 && dir.exists(WEATHER_DIR)

  # Daily data
  daily_file <- file.path(DAILY_DIR, "all_daily.csv")
  has_daily  <- file.exists(daily_file)

  # ── Summary statistics ────────────────────────────────────────────────────
  n_rows   <- nrow(df)
  scenarios <- sort(unique(df$sName))
  locations <- sort(unique(df$Location))
  managements <- sort(unique(df$Manag))
  crops       <- sort(unique(df$Crop))
  years       <- sort(unique(df$Pyear))
  year_range  <- range(df$Pyear, na.rm = TRUE)

  n_normal <- sum(df$MATYP == 1, na.rm = TRUE)
  n_prem   <- sum(df$MATYP == 2, na.rm = TRUE)
  n_flood  <- sum(df$MATYP == 5, na.rm = TRUE)

  by_loc <- split(df, df$Location)

  # Helper tables (same as before)
  loc_rows <- lapply(locations, function(loc) {
    r <- by_loc[[loc]]
    c(loc, length(r[[1]]),
      fnum(r$Ywet, 0), fnum(r$WTOP, 1), fnum(r$MXLAI, 2),
      fnum(r$HI, 3),   fnum(r$R8, 1),   fnum(r$CTR, 1),
      fnum(r$CE, 1),   fnum(r$CRAIN, 1))
  })
  loc_tbl <- as.data.frame(do.call(rbind, loc_rows), stringsAsFactors = FALSE)
  names(loc_tbl) <- c("Location","N","Yield (kg/ha)","WTOP (g/m²)","MXLAI","HI",
                       "Season days","CTR (mm)","CE (mm)","CRAIN (mm)")

  water_type <- function(m) if (grepl("IRR", toupper(m))) "Irrigated" else "Rainfed"
  plant_win  <- function(m) {
    m <- toupper(m)
    if (grepl("ELY", m)) "Early" else if (grepl("MID", m)) "Mid"
    else if (grepl("LTE", m)) "Late" else "Other"
  }
  df$WaterType <- sapply(df$Manag, water_type)
  df$PlantWin  <- sapply(df$Manag, plant_win)

  crop_rows <- lapply(sort(unique(df$Crop)), function(cr) {
    r <- df[df$Crop == cr, ]
    c(cr, nrow(r), fnum(r$Ywet, 0), fnum(r$WTOP, 1), fnum(r$CTR, 1), fnum(r$HI, 3))
  })
  crop_tbl <- as.data.frame(do.call(rbind, crop_rows), stringsAsFactors = FALSE)
  names(crop_tbl) <- c("Cultivar","N","Yield (kg/ha)","WTOP (g/m²)","CTR (mm)","HI")

  manag_keys <- unique(df[, c("WaterType","PlantWin")])
  manag_keys <- manag_keys[order(manag_keys$WaterType, manag_keys$PlantWin), ]
  manag_rows <- apply(manag_keys, 1, function(k) {
    r <- df[df$WaterType == k[1] & df$PlantWin == k[2], ]
    c(k[1], k[2], nrow(r), fnum(r$Ywet, 0), fnum(r$CTR, 1),
      fmean(r$CIRGW, 1), fmean(r$IRGNO, 1))
  })
  manag_tbl <- as.data.frame(t(manag_rows), stringsAsFactors = FALSE)
  names(manag_tbl) <- c("Water","Planting","N","Yield (kg/ha)","CTR (mm)","Irrig. (mm)","Irrig. events")

  pheno_rows <- lapply(locations, function(loc) {
    r <- by_loc[[loc]]
    c(loc, fnum(r$dtEMR,1), fnum(r$R1,1), fnum(r$R5,1), fnum(r$R7,1), fnum(r$R8,1))
  })
  pheno_tbl <- as.data.frame(do.call(rbind, pheno_rows), stringsAsFactors = FALSE)
  names(pheno_tbl) <- c("Location","EMR (DAP)","R1 (DAP)","R5 (DAP)","R7 (DAP)","R8 (DAP)")

  water_rows <- lapply(locations, function(loc) {
    r <- by_loc[[loc]]
    c(loc, fmean(r$CRAIN,1), fmean(r$CTR,1), fmean(r$CE,1),
      fmean(r$CDRAIN,1), fmean(r$CRUNOF,1), fmean(r$ET,1),
      paste0(fmean(as.numeric(r$EoverET)*100, 1)," %"))
  })
  water_tbl <- as.data.frame(do.call(rbind, water_rows), stringsAsFactors = FALSE)
  names(water_tbl) <- c("Location","CRAIN (mm)","CTR (mm)","CE (mm)",
                         "CDRAIN (mm)","CRUNOF (mm)","ET (mm)","E/ET (%)")

  # Maturity status: phenological durations and maturity type counts
  mat_rows <- lapply(locations, function(loc) {
    r   <- by_loc[[loc]]
    emr <- as.numeric(r$dtEMR)
    r3  <- as.numeric(r$R3) - emr        # emergence → R3 (vegetative)
    r7  <- as.numeric(r$R7) - as.numeric(r$R3)  # R3 → R7 (reproductive)
    tot <- as.numeric(r$R7) - emr        # emergence → R7 total
    c(loc,
      fnum(emr, 0),
      fnum(r3,  0),
      fnum(r7,  0),
      fnum(tot, 0),
      sum(r$MATYP==1,na.rm=TRUE),
      sum(r$MATYP==2,na.rm=TRUE),
      sum(r$MATYP==5,na.rm=TRUE))
  })
  mat_tbl <- as.data.frame(do.call(rbind, mat_rows), stringsAsFactors = FALSE)
  names(mat_tbl) <- c("Location",
                       "Emerg. (days)",
                       "Veg. Emerg→R3 (days)",
                       "Repro. R3→R7 (days)",
                       "Total Emerg→R7 (days)",
                       "Normal mat.",
                       "Premature",
                       "Flood kill")

  env_rows <- lapply(locations, function(loc) {
    r <- by_loc[[loc]]
    c(loc, fmean(r$MTMINT,1), fmean(r$MTMAXT,1), fmean(r$SSRADT,0),
      fmean(r$SRAINT,0), fmean(r$SUMETT,0))
  })
  env_tbl <- as.data.frame(do.call(rbind, env_rows), stringsAsFactors = FALSE)
  names(env_tbl) <- c("Location","Tmin (°C)","Tmax (°C)","SRAD (MJ/m²)","Rain (mm)","ET (mm)")

  # ── Generate figures ──────────────────────────────────────────────────────
  cat("Generating figures...\n")

  # Figure A: growing season climate (from results — fast)
  cat("  Figure A: Growing-season climate\n")
  fig_a <- make_plot_b64(function() plot_env_season(df), width = 1500, height = 900, res = 130)

  # Figures B & VPD: require weather files
  fig_b  <- NULL; vpd_fig_paths <- NULL
  tbl_vpd_h <- NULL; tbl_vpd_d <- NULL

  if (has_weather) {
    cat("  Figure B: Monthly climatology\n")
    fig_b <- make_plot_b64(function() plot_monthly_clim(loc_info, WEATHER_DIR),
                           width = 1800, height = 700, res = 130)

    cat("  Computing hourly VPD profiles (this may take a moment)...\n")
    vpd_prof <- compute_vpd_profiles(df, loc_info, WEATHER_DIR)

    # Mean phenology per location × planting window for legend labels
    sub_chk <- df[!grepl("LT", df$Crop), ]
    sub_chk$PlantWin <- sapply(sub_chk$Manag, function(m) {
      m <- toupper(m)
      if (grepl("ELY",m)) "Early" else if (grepl("MID",m)) "Mid"
      else if (grepl("LTE",m)) "Late" else "Other"
    })
    phen_info <- do.call(rbind, lapply(
      split(sub_chk, list(sub_chk$Location, sub_chk$PlantWin), drop = TRUE),
      function(g) data.frame(
        Location = g$Location[1], PlantWin = g$PlantWin[1],
        emr = round(mean(as.numeric(g$dtEMR), na.rm = TRUE)),
        r3  = round(mean(as.numeric(g$R3),    na.rm = TRUE)),
        r7  = round(mean(as.numeric(g$R7),    na.rm = TRUE)),
        stringsAsFactors = FALSE)
    ))

    cat("  Figure VPD: Diurnal VPD curves (one PNG per location)\n")
    vpd_fig_paths <- plot_vpd_diurnal(vpd_prof, DAILY_DIR, phen_info, threshold = 2.0)

    cat("  Computing VPD threshold tables...\n")
    vpd_tbls  <- compute_vpd_tables(df, loc_info, WEATHER_DIR, threshold = 2.0)
    tbl_vpd_h <- vpd_tbls$hours
    tbl_vpd_d <- vpd_tbls$days
  }

  # ── Build HTML sections ───────────────────────────────────────────────────
  daily_sec <- ""
  if (has_daily) {
    dd <- read.csv(daily_file, nrows = 1)
    daily_sec <- sprintf('
<h2 id="daily">9. Daily Output</h2>
<div class="card">
  Daily time-step data in <code>outputs/results/daily/</code> (generated with
  <code>save_daily=TRUE</code>). Key columns: doy, DAP, CBD, LAI, DDMP, WTOP, WGRN,
  TR, FTSWRZ, WSFL, WSFG, WSFD  (46 columns total).
</div>')
  }

  fig_b_sec <- if (!is.null(fig_b)) sprintf('
<h3>Monthly climatology</h3>
<p>Mean monthly Tmin (blue), Tmax (red) and total monthly rainfall (bars) computed from
%d-year historical weather files. Growing season broadly spans DOY 115–300 across locations.</p>
<figure><img src="%s" style="max-width:100%%;border-radius:6px;border:1px solid #dee2e6">
<figcaption>Figure 2 — Monthly climatology (mean over all simulation years).</figcaption></figure>',
length(years), fig_b) else ""

  vpd_sec <- if (!is.null(vpd_fig_paths)) {
    # Build one <figure> per location (base64-embedded)
    figs_html <- paste(sapply(names(vpd_fig_paths), function(loc) {
      b64 <- png_to_b64(vpd_fig_paths[loc])
      sprintf('<figure><img src="%s" style="max-width:100%%;border-radius:6px;border:1px solid #dee2e6">
<figcaption>Figure 3 (%s) — Mean diurnal VPD profile by sowing date (panels) and
growth stage (blue solid = vegetative emergence→R3; red dashed = reproductive R3→R7).
Dotted line = VPDcr 2.0 kPa.</figcaption></figure>', b64, loc)
    }), collapse = "\n")

    sprintf('
<h2 id="vpd">8. Hourly VPD Analysis</h2>
<p>
  Hourly VPD was computed for each simulation day using the same model equations
  (Spitters solar geometry, asymmetric sinusoidal temperature curve, Magnus–Tetens VP formula).
  Days were classified as <strong>vegetative</strong> (emergence → R3) or
  <strong>reproductive</strong> (R3 → R7) using the actual phenology of each simulation year.
  Mean profiles are averaged over all 30 simulation years (check cultivar).
</p>
<div class="note">
  The <strong>2.0 kPa threshold</strong> (dotted line) corresponds to the VPDcr of the LT2
  cultivar. Hours or days above this threshold drive stomatal restriction and DM production
  penalties in LT cultivars. Individual PNG files are saved to
  <code>outputs/results/daily/</code>.
</div>
%s

<h3>Table: Cumulative hours with VPD &gt; 2 kPa per season</h3>
<p>Total daylight hours above 2 kPa accumulated over the vegetative (emergence → R3),
reproductive (R3 → R7), and total (emergence → R7) phases, for each planting window.
Values: mean ± SD across the 30 simulation years (check cultivar).</p>
%s

<h3>Table: Cumulative days with VPD &gt; 2 kPa per season</h3>
<p>Total days with at least one daylight hour above 2 kPa, split by growth phase and
planting window. Values: mean ± SD across 30 years.</p>
%s',
    figs_html,
    df_to_html(tbl_vpd_h, cls = "tbl"),
    df_to_html(tbl_vpd_d, cls = "tbl"))
  } else ""

  loc_list  <- paste(sprintf("<li>%s</li>", locations), collapse = "")
  scen_list <- paste(sprintf('<span class="badge bg">%s</span>', managements), collapse = " ")
  crop_list <- paste(sprintf('<span class="badge bo">%s</span>', crops), collapse = " ")

  toc_vpd   <- if (!is.null(vpd_fig_paths)) '<a href="#vpd">8. VPD analysis</a>' else ""
  toc_daily <- if (has_daily) '<a href="#daily">9. Daily output</a>' else ""

  CSS <- "
  :root{--primary:#1a5276;--accent:#2e86c1;--green:#1e8449;
        --orange:#d35400;--bg:#f8f9fa;--card:#ffffff;
        --border:#dee2e6;--text:#212529;--muted:#6c757d;}
  *{box-sizing:border-box;margin:0;padding:0;}
  body{font-family:'Segoe UI',Arial,sans-serif;font-size:14px;
       color:var(--text);background:var(--bg);line-height:1.65;}
  #toc{position:fixed;top:0;left:0;width:210px;height:100vh;overflow-y:auto;
       background:var(--primary);color:#cde;padding:12px 0;z-index:100;}
  #toc h3{padding:10px 16px 6px;font-size:11px;text-transform:uppercase;letter-spacing:1px;color:#acc;}
  #toc a{display:block;padding:5px 16px;font-size:12px;color:#cde;text-decoration:none;border-left:3px solid transparent;}
  #toc a:hover{background:rgba(255,255,255,.1);border-left-color:#5bc0de;color:#fff;}
  main{margin-left:210px;padding:28px 44px 64px;max-width:1100px;}
  h1{font-size:1.9rem;color:var(--primary);border-bottom:3px solid var(--accent);
     padding-bottom:8px;margin-bottom:4px;}
  .subtitle{color:var(--muted);font-size:13px;margin-bottom:28px;}
  h2{font-size:1.3rem;color:var(--primary);margin:36px 0 12px;
     border-left:5px solid var(--accent);padding-left:10px;}
  h3{font-size:1.05rem;color:var(--accent);margin:20px 0 8px;}
  p{margin-bottom:8px;}
  ul,ol{margin:6px 0 10px 22px;}li{margin-bottom:3px;}
  .card{background:var(--card);border:1px solid var(--border);border-radius:6px;padding:16px 20px;margin:14px 0;}
  .stat-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(155px,1fr));gap:12px;margin:14px 0;}
  .stat-box{background:var(--card);border:1px solid var(--border);border-radius:6px;padding:14px 16px;text-align:center;}
  .stat-val{font-size:2rem;font-weight:700;color:var(--primary);}
  .stat-lbl{font-size:11px;color:var(--muted);text-transform:uppercase;letter-spacing:.5px;}
  table.tbl{border-collapse:collapse;width:100%;font-size:12.5px;margin:12px 0;}
  table.tbl th{background:var(--primary);color:white;padding:7px 10px;text-align:left;font-weight:600;}
  table.tbl td{padding:6px 10px;border-bottom:1px solid var(--border);}
  table.tbl tr:nth-child(even) td{background:#f2f6fb;}
  table.tbl tr:hover td{background:#e8f4fd;}
  code{background:#f1f3f4;padding:1px 4px;border-radius:3px;font-size:92%;}
  .note{background:#eaf7fb;border-left:4px solid var(--accent);padding:10px 14px;margin:10px 0;border-radius:0 4px 4px 0;}
  .warn{background:#fef9e7;border-left:4px solid #f39c12;padding:10px 14px;margin:10px 0;border-radius:0 4px 4px 0;}
  .badge{display:inline-block;padding:2px 8px;border-radius:12px;font-size:11px;font-weight:600;margin:2px;}
  .bg{background:#d5f5e3;color:var(--green);}
  .bo{background:#fdebd0;color:var(--orange);}
  .br{background:#fadbd8;color:#c0392b;}
  figure{margin:16px 0;text-align:center;}
  figcaption{font-size:12px;color:var(--muted);margin-top:6px;font-style:italic;}
  "

  html <- sprintf('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>SSM Soybean — Simulation Report</title>
<style>%s</style>
</head>
<body>
<nav id="toc">
  <h3>Report Contents</h3>
  <a href="#overview">1. Overview</a>
  <a href="#locations">2. Locations &amp; Climate</a>
  <a href="#scenarios">3. Scenarios</a>
  <a href="#yield">4. Yield &amp; Biomass</a>
  <a href="#phenology">5. Phenology</a>
  <a href="#water">6. Water Balance</a>
  <a href="#maturity">7. Maturity Status</a>
  %s
  %s
</nav>
<main>
<h1>SSM Soybean Model — Simulation Report</h1>
<p class="subtitle">Generated: %s &nbsp;|&nbsp; Source: <code>%s</code></p>

<!-- 1. Overview -->
<h2 id="overview">1. Overview</h2>
<div class="stat-grid">
  <div class="stat-box"><div class="stat-val">%d</div><div class="stat-lbl">Scenarios</div></div>
  <div class="stat-box"><div class="stat-val">%d</div><div class="stat-lbl">Locations</div></div>
  <div class="stat-box"><div class="stat-val">%d</div><div class="stat-lbl">Years per scenario</div></div>
  <div class="stat-box"><div class="stat-val">%s</div><div class="stat-lbl">Sim. years total</div></div>
  <div class="stat-box"><div class="stat-val">%d</div><div class="stat-lbl">Managements</div></div>
  <div class="stat-box"><div class="stat-val">%d</div><div class="stat-lbl">Cultivars</div></div>
</div>
<div class="note">
  Year range: <strong>%d – %d</strong>.&nbsp;
  Normal maturity: <strong>%s (%.1f%%)</strong> &nbsp;|&nbsp;
  Premature senescence: <strong>%s (%.1f%%)</strong> &nbsp;|&nbsp;
  Flood kill: <strong>%s (%.1f%%)</strong>.
</div>

<!-- 2. Locations -->
<h2 id="locations">2. Locations and Environmental Characterisation</h2>
<div class="card">
  <strong>%d locations</strong>, climate years %d–%d (%d years each).
  <ul>%s</ul>
</div>
<h3>Mean growing-season environment (sowing to maturity)</h3>
%s

<h3>Growing-season climate and yield variability</h3>
<p>Rainfed check cultivar, all planting windows and years combined. Boxes show inter-annual variability; dashed line = overall mean.</p>
<figure><img src="%s" style="max-width:100%%;border-radius:6px;border:1px solid #dee2e6">
<figcaption>Figure 1 — Growing-season climate and yield across 10 locations (rainfed check cultivar, 30 years).
Upper row: temperature, middle: radiation and rain; lower right: yield.</figcaption></figure>

%s

<!-- 3. Scenarios -->
<h2 id="scenarios">3. Scenario Structure</h2>
<div class="card">
  Each scenario: <strong>Location – Water – Planting window – Cultivar</strong>.
  <ul>
    <li><strong>Water:</strong> RFD (rainfed) | IRRI (irrigated)</li>
    <li><strong>Planting window:</strong> ELY (early) | MID (mid) | LTE (late)</li>
    <li><strong>Cultivar:</strong> check (vpdtp=0) | LT1.5 / LT2 / LT2.5 (vpdtp=1)</li>
  </ul>
</div>
<p>Managements: %s</p>
<p style="margin-top:6px;">Cultivars: %s</p>

<!-- 4. Yield & Biomass -->
<h2 id="yield">4. Yield and Biomass Summary</h2>
<h3>By location (mean ± SD, all scenarios and years)</h3>
%s
<h3>By cultivar</h3>
%s
<h3>By management (water × planting window)</h3>
%s

<!-- 5. Phenology -->
<h2 id="phenology">5. Phenological Stages — Days After Planting</h2>
<p>Mean ± SD over all management types and years per location.</p>
%s

<!-- 6. Water Balance -->
<h2 id="water">6. Seasonal Water Balance</h2>
<div class="note">CTR = transpiration · CE = soil evaporation · ET = CTR+CE ·
CDRAIN = deep drainage · CRUNOF = runoff.</div>
%s

<!-- 7. Maturity Status -->
<h2 id="maturity">7. Maturity Status and Phenological Duration</h2>
<div class="card"><p>
  Season length from emergence to physiological maturity (R7) split into
  <strong>vegetative phase</strong> (emergence → R3 beginning pod) and
  <strong>reproductive phase</strong> (R3 → R7). Values are mean ± SD across all
  scenarios and years per location.
  Maturity type counts: <span class="badge bg">1 — Normal</span>&nbsp;
  <span class="badge bo">2 — Premature senescence</span>&nbsp;
  <span class="badge br">5 — Flood kill</span>
</p></div>
%s

%s
%s

</main>
</body>
</html>',
    CSS,
    toc_vpd, toc_daily,
    format(Sys.time(), "%Y-%m-%d %H:%M"),
    basename(results_file),
    length(scenarios), length(locations), length(years),
    format(n_rows, big.mark = ","),
    length(managements), length(crops),
    year_range[1], year_range[2],
    format(n_normal, big.mark = ","), n_normal / n_rows * 100,
    format(n_prem,   big.mark = ","), n_prem   / n_rows * 100,
    format(n_flood,  big.mark = ","), n_flood  / n_rows * 100,
    length(locations), year_range[1], year_range[2], length(years),
    loc_list,
    df_to_html(env_tbl, cls = "tbl"),
    fig_a,
    fig_b_sec,
    scen_list, crop_list,
    df_to_html(loc_tbl,   cls = "tbl"),
    df_to_html(crop_tbl,  cls = "tbl"),
    df_to_html(manag_tbl, cls = "tbl"),
    df_to_html(pheno_tbl, cls = "tbl"),
    df_to_html(water_tbl, cls = "tbl"),
    df_to_html(mat_tbl,   cls = "tbl"),
    vpd_sec, daily_sec
  )

  writeLines(html, out_html)
  sz <- round(file.info(out_html)$size / 1e6, 1)
  cat(sprintf("\nReport written: %s  (%.1f MB)\n", out_html, sz))
  invisible(out_html)
}


# Run if called directly
if (!interactive()) {
  generate_report()
}
