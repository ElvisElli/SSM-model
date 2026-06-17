# =============================================================================
# SSM Soybean Model - Validation & Comparison Plots
# =============================================================================
# Compares R model outputs to the reference Excel model outputs for
# all 10 locations. Produces a suite of diagnostic plots.
#
# Plot types:
#   1. 1:1 scatter plots for key output variables (WGRN, WTOP, MXLAI, dtR8, etc.)
#   2. Time-series comparison plots for selected scenarios
#   3. Bias and RMSE summary table
#   4. Distribution plots (boxplots) per location and scenario type
#
# Usage:
#   source("09_validate_plots.R")
#   create_validation_plots()
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(gridExtra)
  library(openxlsx)
})

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b

# Determine paths relative to code directory
CODE_DIR   <- normalizePath(dirname(sys.frame(1)$ofile), mustWork = FALSE)
BASE_DIR   <- normalizePath(file.path(CODE_DIR, ".."), mustWork = FALSE)
OUTPUT_DIR <- file.path(BASE_DIR, "outputs")
PLOTS_DIR  <- file.path(OUTPUT_DIR, "plots")
RESULTS_DIR<- file.path(OUTPUT_DIR, "results")
REF_DIR    <- file.path(BASE_DIR, "..", "excel-model", "outputs")

dir.create(PLOTS_DIR, recursive = TRUE, showWarnings = FALSE)

# Color palette for locations
LOC_COLORS <- c(
  "Albany"      = "#E41A1C",
  "Eustis"      = "#377EB8",
  "Jonesboro"   = "#4DAF4A",
  "Keiser"      = "#984EA3",
  "Lincoln"     = "#FF7F00",
  "Marianna"    = "#A65628",
  "Mount Vernon"= "#F781BF",
  "North Platte"= "#999999",
  "Novelty"     = "#66C2A5",
  "Rohwer"      = "#FC8D62"
)

# Map between R model location names and reference file names
REF_FILES <- c(
  "Albany"       = "Albany_output.xlsx",
  "Eustis"       = "Eustis_output.xlsx",
  "Jonesboro"    = "Jonesboro_output.xlsx",
  "Keiser"       = "Keiser_output.xlsx",
  "Lincoln"      = "Lincoln_output.xlsx",
  "Marianna"     = "Marianna_output.xlsx",
  "Mount Vernon" = "Mount_Vernon_output.xlsx",
  "North Platte" = "North_Platte_output.xlsx",
  "Novelty"      = "Novelty_output.xlsx",
  "Rohwer"       = "Rohwer_output.xlsx"
)


# =============================================================================
# FUNCTION: load_reference_output
# Reads the YearlyO sheet from an Excel reference output file.
# =============================================================================
load_reference_output <- function(ref_dir, filename) {
  filepath <- file.path(ref_dir, filename)
  if (!file.exists(filepath)) {
    warning("Reference file not found: ", filepath)
    return(NULL)
  }
  tryCatch({
    df <- suppressMessages(read_excel(filepath, sheet = "YearlyO", col_names = TRUE))
    df <- as.data.frame(df)
    df
  }, error = function(e) {
    warning("Could not read: ", filepath, " - ", e$message)
    NULL
  })
}


# =============================================================================
# FUNCTION: load_all_reference
# Loads all 10 reference output files and combines them.
# =============================================================================
load_all_reference <- function(ref_dir = REF_DIR) {
  all_ref <- list()
  for (loc in names(REF_FILES)) {
    ref <- load_reference_output(ref_dir, REF_FILES[loc])
    if (!is.null(ref)) {
      ref$Location_clean <- loc
      all_ref[[loc]]     <- ref
    }
  }
  if (length(all_ref) == 0) {
    stop("No reference output files found in: ", ref_dir)
  }
  do.call(rbind, all_ref)
}


# =============================================================================
# FUNCTION: load_r_results
# Loads all R model results from the results directory.
# =============================================================================
load_r_results <- function(results_dir = RESULTS_DIR) {
  all_file <- file.path(results_dir, "all_results.csv")
  if (file.exists(all_file)) {
    return(read.csv(all_file, stringsAsFactors = FALSE))
  }
  # Fall back to per-location files
  files <- list.files(results_dir, pattern = "_results.csv", full.names = TRUE)
  if (length(files) == 0) stop("No R model results found in: ", results_dir)
  do.call(rbind, lapply(files, read.csv, stringsAsFactors = FALSE))
}


# =============================================================================
# FUNCTION: match_r_to_ref
# Joins R results to reference results by scenario name and year.
# =============================================================================
match_r_to_ref <- function(r_df, ref_df) {
  # Standardize reference column names to match R output
  ref_df$sName  <- ref_df[[1]]   # sName column
  ref_df$Pyear  <- as.integer(ref_df$Pyear)
  ref_df$Pdoy   <- as.integer(ref_df$Pdoy)

  r_df$sName  <- r_df$sName
  r_df$Pyear  <- as.integer(r_df$Pyear)

  # Join on scenario name + year
  merged <- inner_join(
    r_df   %>% select(sName, Pyear, Pdoy, WGRN, WTOP, HI, MXLAI, R5=R5,
                      dtEMR, R1, R3, R5=R5, R7, R8,
                      CE, CTR, CRAIN, CDRAIN, CRUNOF, ATSWSL, ET,
                      MATYP, Ywet) %>%
             rename_with(~paste0("R_", .), -c(sName, Pyear)),

    ref_df %>% select(sName, Pyear, Pdoy, WGRN, WTOP, HI, MXLAI,
                      dtEMR, R8=`R8.MAT`,
                      R1=`R1.SEL.SIL`, R3=`R3.EAR`, R5=`R5.BSG`, R7=`R7.PM`,
                      CE, CTR, CRAIN, CDRAIN, CRUNOF, ATSWSL=ATSWSL, ET,
                      MATYP, Ywet, Location_clean) %>%
             rename_with(~paste0("Ref_", .), -c(sName, Pyear, Location_clean)),

    by = c("sName", "Pyear")
  )
  merged
}


# =============================================================================
# FUNCTION: calc_stats
# Calculates RMSE, bias, and R² for a pair of vectors.
# =============================================================================
calc_stats <- function(sim, obs) {
  if (length(sim) < 3) return(list(RMSE = NA, RRMSE = NA, Bias = NA, R2 = NA, n = length(sim)))
  valid <- !is.na(sim) & !is.na(obs)
  sim <- sim[valid]; obs <- obs[valid]
  n     <- length(sim)
  RMSE  <- sqrt(mean((sim - obs)^2))
  RRMSE <- if (mean(obs) != 0) RMSE / abs(mean(obs)) * 100 else NA  # % of observed mean
  Bias  <- mean(sim - obs)
  ss_res <- sum((sim - obs)^2)
  ss_tot <- sum((obs - mean(obs))^2)
  R2    <- 1 - ss_res / max(ss_tot, 1e-10)
  list(RMSE = RMSE, RRMSE = RRMSE, Bias = Bias, R2 = R2, n = n)
}


# =============================================================================
# FUNCTION: plot_11_scatter
# Creates a 1:1 comparison scatter plot for one variable.
# =============================================================================
plot_11_scatter <- function(data, x_var, y_var, title, xlab, ylab,
                             color_var = "Location_clean", xlim = NULL, ylim = NULL) {
  stats <- calc_stats(data[[y_var]], data[[x_var]])
  label <- sprintf("n=%d  RMSE=%.2f  Bias=%.2f  R²=%.3f",
                   stats$n, stats$RMSE %||% NA, stats$Bias %||% NA, stats$R2 %||% NA)

  # Axis limits
  all_vals <- c(data[[x_var]], data[[y_var]])
  all_vals <- all_vals[!is.na(all_vals)]
  lims <- range(all_vals, na.rm = TRUE)
  if (is.null(xlim)) xlim <- lims
  if (is.null(ylim)) ylim <- lims

  ggplot(data, aes_string(x = x_var, y = y_var, color = color_var)) +
    geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed", size = 0.8) +
    geom_point(alpha = 0.5, size = 1.2) +
    scale_color_manual(values = LOC_COLORS, name = "Location") +
    coord_equal(xlim = xlim, ylim = ylim) +
    labs(title = title, x = xlab, y = ylab,
         subtitle = label) +
    theme_bw(base_size = 11) +
    theme(
      legend.position  = "right",
      legend.key.size  = unit(0.4, "cm"),
      plot.subtitle    = element_text(size = 9, color = "grey40")
    )
}


# =============================================================================
# FUNCTION: create_validation_plots
# Main function that generates and saves all validation plots.
# =============================================================================
create_validation_plots <- function(results_dir = RESULTS_DIR,
                                     ref_dir = REF_DIR,
                                     plots_dir = PLOTS_DIR) {

  cat("Loading R model results...\n")
  r_df <- tryCatch(load_r_results(results_dir), error = function(e) {
    stop("Could not load R model results. Run 08_run_model.R first.\n", e$message)
  })
  cat(sprintf("  Loaded %d R result rows\n", nrow(r_df)))

  cat("Loading reference (Excel model) results...\n")
  ref_df <- tryCatch(load_all_reference(ref_dir), error = function(e) {
    stop("Could not load reference outputs from: ", ref_dir, "\n", e$message)
  })
  cat(sprintf("  Loaded %d reference rows\n", nrow(ref_df)))

  # Standardize reference column names
  ref_colmap <- c(
    "sName"      = "sName",
    "Location"   = "Location",
    "Pyear"      = "Pyear",
    "Pdoy"       = "Pdoy",
    "dtEMR"      = "dtEMR",
    "R8.MAT"     = "R8",
    "R1.SEL.SIL" = "R1",
    "R3.EAR"     = "R3",
    "R5.BSG"     = "R5",
    "R7.PM"      = "R7",
    "MXLAI"      = "MXLAI",
    "WTOP"       = "WTOP",
    "WGRN"       = "WGRN",
    "HI"         = "HI",
    "CRAIN"      = "CRAIN",
    "CE"         = "CE",
    "CTR"        = "CTR",
    "CDRAIN"     = "CDRAIN",
    "CRUNOF"     = "CRUNOF",
    "ATSWSL"     = "ATSWSL",
    "ET"         = "ET",
    "MATYP"      = "MATYP",
    "Ywet"       = "Ywet"
  )
  for (orig in names(ref_colmap)) {
    if (orig %in% colnames(ref_df) && !ref_colmap[orig] %in% colnames(ref_df)) {
      ref_df[[ref_colmap[orig]]] <- ref_df[[orig]]
    }
  }

  # Add R² numeric to R and ref columns for matching
  r_df$Pyear   <- as.integer(r_df$Pyear)
  ref_df$Pyear <- as.integer(ref_df$Pyear)

  # Join on sName + Pyear
  r_key   <- paste0(r_df$sName,  "_", r_df$Pyear)
  ref_key <- paste0(ref_df$sName, "_", ref_df$Pyear)

  common_keys <- intersect(r_key, ref_key)
  cat(sprintf("  Matched %d scenario-year records\n", length(common_keys)))

  if (length(common_keys) == 0) {
    cat("WARNING: No matching records between R output and reference.\n")
    cat("  R scenarios (first 5):", head(r_df$sName, 5), "\n")
    cat("  Ref scenarios (first 5):", head(ref_df$sName, 5), "\n")
    return(invisible(NULL))
  }

  r_matched   <- r_df[r_key %in% common_keys, ]
  ref_matched <- ref_df[ref_key %in% common_keys, ]

  # Order both by key for proper joining
  r_matched   <- r_matched[order(paste0(r_matched$sName, r_matched$Pyear)), ]
  ref_matched <- ref_matched[order(paste0(ref_matched$sName, ref_matched$Pyear)), ]

  # Build combined comparison data.frame
  cmp <- data.frame(
    sName     = r_matched$sName,
    Pyear     = r_matched$Pyear,
    Location  = ref_matched$Location_clean %||% ref_matched$Location,
    # R model outputs
    R_WGRN    = as.numeric(r_matched$WGRN),
    R_WTOP    = as.numeric(r_matched$WTOP),
    R_MXLAI   = as.numeric(r_matched$MXLAI),
    R_dtR8    = as.numeric(r_matched$R8),
    R_dtR5    = as.numeric(r_matched$R5),
    R_CE      = as.numeric(r_matched$CE),
    R_CTR     = as.numeric(r_matched$CTR),
    R_CRAIN   = as.numeric(r_matched$CRAIN),
    R_CIRGW   = as.numeric(r_matched$CIRGW),
    R_IRGNO   = as.numeric(r_matched$IRGNO),
    R_IPASW   = as.numeric(r_matched$IPASW),
    R_HI      = as.numeric(r_matched$HI),
    R_Ywet    = as.numeric(r_matched$Ywet),
    # Reference outputs
    Ref_WGRN  = as.numeric(ref_matched$WGRN),
    Ref_WTOP  = as.numeric(ref_matched$WTOP),
    Ref_MXLAI = as.numeric(ref_matched$MXLAI),
    Ref_dtR8  = as.numeric(ref_matched$R8),
    Ref_dtR5  = as.numeric(ref_matched$R5),
    Ref_CE    = as.numeric(ref_matched$CE),
    Ref_CTR   = as.numeric(ref_matched$CTR),
    Ref_CRAIN = as.numeric(ref_matched$CRAIN),
    Ref_CIRGW = as.numeric(ref_matched$CIRGW),
    Ref_IRGNO = as.numeric(ref_matched$IRGNO),
    Ref_IPASW = as.numeric(ref_matched$IPASW),
    Ref_HI    = as.numeric(ref_matched$HI),
    Ref_Ywet  = as.numeric(ref_matched$Ywet),
    stringsAsFactors = FALSE
  )

  # Ensure Location is properly mapped
  if (all(is.na(cmp$Location))) {
    cmp$Location <- gsub("^([A-Z][A-Z])-.*", "\\1", cmp$sName)
  }

  cat(sprintf("Creating validation plots in: %s\n", plots_dir))

  # ----------------------------------------------------------
  # PLOT 1: Grain yield (WGRN) 1:1 scatter
  # ----------------------------------------------------------
  p1 <- plot_11_scatter(cmp, "Ref_WGRN", "R_WGRN",
                         "Grain Yield (WGRN)",
                         "Excel model (g/m²)", "R model (g/m²)",
                         color_var = "Location")
  ggsave(file.path(plots_dir, "01_validation_WGRN.png"), p1,
         width = 7, height = 6, dpi = 150)
  cat("  Saved: 01_validation_WGRN.png\n")

  # ----------------------------------------------------------
  # PLOT 2: Total above-ground DM (WTOP) 1:1 scatter
  # ----------------------------------------------------------
  p2 <- plot_11_scatter(cmp, "Ref_WTOP", "R_WTOP",
                         "Total Dry Matter (WTOP)",
                         "Excel model (g/m²)", "R model (g/m²)",
                         color_var = "Location")
  ggsave(file.path(plots_dir, "02_validation_WTOP.png"), p2,
         width = 7, height = 6, dpi = 150)
  cat("  Saved: 02_validation_WTOP.png\n")

  # ----------------------------------------------------------
  # PLOT 3: Maximum LAI 1:1 scatter
  # ----------------------------------------------------------
  p3 <- plot_11_scatter(cmp, "Ref_MXLAI", "R_MXLAI",
                         "Maximum LAI (MXLAI)",
                         "Excel model (m²/m²)", "R model (m²/m²)",
                         color_var = "Location")
  ggsave(file.path(plots_dir, "03_validation_MXLAI.png"), p3,
         width = 7, height = 6, dpi = 150)
  cat("  Saved: 03_validation_MXLAI.png\n")

  # ----------------------------------------------------------
  # PLOT 4: Days to maturity (dtR8) 1:1 scatter
  # ----------------------------------------------------------
  p4 <- plot_11_scatter(cmp, "Ref_dtR8", "R_dtR8",
                         "Days to Maturity (dtR8)",
                         "Excel model (days)", "R model (days)",
                         color_var = "Location")
  ggsave(file.path(plots_dir, "04_validation_dtR8.png"), p4,
         width = 7, height = 6, dpi = 150)
  cat("  Saved: 04_validation_dtR8.png\n")

  # ----------------------------------------------------------
  # PLOT 5: Cumulative transpiration (CTR) 1:1 scatter
  # ----------------------------------------------------------
  p5 <- plot_11_scatter(cmp, "Ref_CTR", "R_CTR",
                         "Cumulative Transpiration (CTR)",
                         "Excel model (mm)", "R model (mm)",
                         color_var = "Location")
  ggsave(file.path(plots_dir, "05_validation_CTR.png"), p5,
         width = 7, height = 6, dpi = 150)
  cat("  Saved: 05_validation_CTR.png\n")

  # ----------------------------------------------------------
  # PLOT 6: Cumulative soil evaporation (CE) 1:1 scatter
  # ----------------------------------------------------------
  p6 <- plot_11_scatter(cmp, "Ref_CE", "R_CE",
                         "Cumulative Soil Evaporation (CE)",
                         "Excel model (mm)", "R model (mm)",
                         color_var = "Location")
  ggsave(file.path(plots_dir, "06_validation_CE.png"), p6,
         width = 7, height = 6, dpi = 150)
  cat("  Saved: 06_validation_CE.png\n")

  # ----------------------------------------------------------
  # PLOT 7: Harvest Index (HI) 1:1 scatter
  # ----------------------------------------------------------
  p7 <- plot_11_scatter(cmp, "Ref_HI", "R_HI",
                         "Harvest Index (HI)",
                         "Excel model (g/g)", "R model (g/g)",
                         color_var = "Location")
  ggsave(file.path(plots_dir, "07_validation_HI.png"), p7,
         width = 7, height = 6, dpi = 150)
  cat("  Saved: 07_validation_HI.png\n")

  # ----------------------------------------------------------
  # PLOT 8: Days to R5 (beginning seed growth) 1:1 scatter
  # ----------------------------------------------------------
  p8 <- plot_11_scatter(cmp, "Ref_dtR5", "R_dtR5",
                         "Days to R5 (beginning seed growth)",
                         "Excel model (days)", "R model (days)",
                         color_var = "Location")
  ggsave(file.path(plots_dir, "08_validation_dtR5.png"), p8,
         width = 7, height = 6, dpi = 150)
  cat("  Saved: 08_validation_dtR5.png\n")

  # ----------------------------------------------------------
  # PLOT 9: Wet yield (Ywet) 1:1 scatter
  # ----------------------------------------------------------
  p9 <- plot_11_scatter(cmp, "Ref_Ywet", "R_Ywet",
                         "Wet Yield (Ywet)",
                         "Excel model (kg/ha)", "R model (kg/ha)",
                         color_var = "Location")
  ggsave(file.path(plots_dir, "09_validation_Ywet.png"), p9,
         width = 7, height = 6, dpi = 150)
  cat("  Saved: 09_validation_Ywet.png\n")

  # ----------------------------------------------------------
  # PLOT 10: Cumulative irrigation water (CIRGW) — IRRI scenarios only
  # ----------------------------------------------------------
  cmp_irri <- cmp[grepl("IRRI", cmp$sName), ]
  p10a <- plot_11_scatter(cmp_irri, "Ref_CIRGW", "R_CIRGW",
                          "Cumulative Irrigation (CIRGW) — IRRI scenarios",
                          "Excel model (mm)", "R model (mm)",
                          color_var = "Location")
  ggsave(file.path(plots_dir, "10_validation_CIRGW.png"), p10a,
         width = 7, height = 6, dpi = 150)
  cat("  Saved: 10_validation_CIRGW.png\n")

  # ----------------------------------------------------------
  # PLOT 11: Irrigation events (IRGNO) — IRRI scenarios only
  # ----------------------------------------------------------
  p11a <- plot_11_scatter(cmp_irri, "Ref_IRGNO", "R_IRGNO",
                          "Irrigation Events (IRGNO) — IRRI scenarios",
                          "Excel model (count)", "R model (count)",
                          color_var = "Location")
  ggsave(file.path(plots_dir, "11_validation_IRGNO.png"), p11a,
         width = 7, height = 6, dpi = 150)
  cat("  Saved: 11_validation_IRGNO.png\n")

  # ----------------------------------------------------------
  # PLOT 12: Initial plant-available soil water (IPASW)
  # ----------------------------------------------------------
  p12a <- plot_11_scatter(cmp, "Ref_IPASW", "R_IPASW",
                          "Initial Plant-Available Soil Water (IPASW)",
                          "Excel model (mm)", "R model (mm)",
                          color_var = "Location")
  ggsave(file.path(plots_dir, "12_validation_IPASW.png"), p12a,
         width = 7, height = 6, dpi = 150)
  cat("  Saved: 12_validation_IPASW.png\n")

  # ----------------------------------------------------------
  # PLOT 13: Summary panel (2×3 grid of key variables)
  # ----------------------------------------------------------
  p_panel <- grid.arrange(
    p1 + theme(legend.position = "none"),
    p2 + theme(legend.position = "none"),
    p3 + theme(legend.position = "none"),
    p4 + theme(legend.position = "none"),
    p5 + theme(legend.position = "none"),
    p7 + theme(legend.position = "none"),
    ncol = 3,
    top = "SSM Soybean Model - R vs Excel Validation (10 locations, 30 years)"
  )
  ggsave(file.path(plots_dir, "13_validation_summary_panel.png"), p_panel,
         width = 14, height = 9, dpi = 150)
  cat("  Saved: 13_validation_summary_panel.png\n")

  # ----------------------------------------------------------
  # PLOT 11: Per-location RMSE bar chart for WGRN
  # ----------------------------------------------------------
  loc_stats <- cmp %>%
    group_by(Location) %>%
    summarise(
      RMSE_WGRN  = sqrt(mean((R_WGRN - Ref_WGRN)^2, na.rm = TRUE)),
      Bias_WGRN  = mean(R_WGRN - Ref_WGRN, na.rm = TRUE),
      RMSE_dtR8  = sqrt(mean((R_dtR8 - Ref_dtR8)^2, na.rm = TRUE)),
      RMSE_MXLAI = sqrt(mean((R_MXLAI - Ref_MXLAI)^2, na.rm = TRUE)),
      n          = n(),
      .groups = "drop"
    )

  p11 <- ggplot(loc_stats, aes(x = reorder(Location, RMSE_WGRN), y = RMSE_WGRN,
                                fill = Location)) +
    geom_col() +
    scale_fill_manual(values = LOC_COLORS) +
    coord_flip() +
    labs(title = "RMSE of Grain Yield (WGRN) by Location",
         x = NULL, y = "RMSE (g/m²)") +
    theme_bw(base_size = 11) +
    theme(legend.position = "none")
  ggsave(file.path(plots_dir, "14_rmse_by_location.png"), p11,
         width = 7, height = 5, dpi = 150)
  cat("  Saved: 14_rmse_by_location.png\n")

  # ----------------------------------------------------------
  # PLOT 12: Residual plot (R - Ref) for WGRN vs Ref_WGRN
  # ----------------------------------------------------------
  cmp$resid_WGRN <- cmp$R_WGRN - cmp$Ref_WGRN
  p12 <- ggplot(cmp, aes(x = Ref_WGRN, y = resid_WGRN, color = Location)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
    geom_point(alpha = 0.4, size = 1.2) +
    scale_color_manual(values = LOC_COLORS) +
    labs(title = "Grain Yield Residuals (R model - Excel model)",
         x = "Excel model WGRN (g/m²)", y = "Residual (g/m²)") +
    theme_bw(base_size = 11)
  ggsave(file.path(plots_dir, "15_residuals_WGRN.png"), p12,
         width = 8, height = 5, dpi = 150)
  cat("  Saved: 15_residuals_WGRN.png\n")

  # ----------------------------------------------------------
  # Save statistics table
  # ----------------------------------------------------------
  var_pairs <- list(
    list(r = "R_WGRN",  ref = "Ref_WGRN",  name = "Grain Yield (g/m²)",          data = cmp),
    list(r = "R_WTOP",  ref = "Ref_WTOP",  name = "Total DM (g/m²)",             data = cmp),
    list(r = "R_MXLAI", ref = "Ref_MXLAI", name = "Max LAI (m²/m²)",             data = cmp),
    list(r = "R_dtR8",  ref = "Ref_dtR8",  name = "Days to Maturity",             data = cmp),
    list(r = "R_dtR5",  ref = "Ref_dtR5",  name = "Days to R5",                   data = cmp),
    list(r = "R_CE",    ref = "Ref_CE",    name = "Soil Evaporation (mm)",         data = cmp),
    list(r = "R_CTR",   ref = "Ref_CTR",   name = "Transpiration (mm)",            data = cmp),
    list(r = "R_HI",    ref = "Ref_HI",    name = "Harvest Index",                 data = cmp),
    list(r = "R_Ywet",  ref = "Ref_Ywet",  name = "Wet Yield (kg/ha)",            data = cmp),
    list(r = "R_CIRGW", ref = "Ref_CIRGW", name = "Cumul. Irrigation (mm) [IRRI]",data = cmp_irri),
    list(r = "R_IRGNO", ref = "Ref_IRGNO", name = "Irrigation Events [IRRI]",     data = cmp_irri),
    list(r = "R_IPASW", ref = "Ref_IPASW", name = "Init. Plant-Avail. SW (mm)",   data = cmp)
  )

  stats_rows <- lapply(var_pairs, function(vp) {
    s <- calc_stats(vp$data[[vp$r]], vp$data[[vp$ref]])
    data.frame(Variable = vp$name, n = s$n,
               RMSE = round(s$RMSE, 3), RRMSE_pct = round(s$RRMSE, 2),
               Bias = round(s$Bias, 3), R2 = round(s$R2, 4),
               stringsAsFactors = FALSE)
  })
  stats_df <- do.call(rbind, stats_rows)

  write.csv(stats_df, file.path(PLOTS_DIR, "validation_statistics.csv"), row.names = FALSE)
  cat("\nValidation statistics:\n")
  print(stats_df)

  cat(sprintf("\nAll plots saved to: %s\n", plots_dir))
  invisible(list(comparison = cmp, stats = stats_df, loc_stats = loc_stats))
}


# Run if called directly
if (!interactive()) {
  create_validation_plots()
}
