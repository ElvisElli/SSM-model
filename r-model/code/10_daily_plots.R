# =============================================================================
# SSM Soybean Model - Daily Time-Step Comparison Plots
# =============================================================================
# Runs the full 240-scenario chain (for correct FTSWRZ carry-over) but
# captures daily output only for selected (scenario, year) pairs.
# Plots R model vs Excel DailyO for key state variables.
#
# Usage:
#   Rscript r-model/code/10_daily_plots.R
# =============================================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(ggplot2); library(gridExtra)
})

BASE_DIR    <- "/home/user/SSM-model/r-model"
CODE_DIR    <- file.path(BASE_DIR, "code")
INPUT_DIR   <- file.path(BASE_DIR, "inputs")
WEATHER_DIR <- file.path(INPUT_DIR, "weather")
OUTPUT_DIR  <- file.path(BASE_DIR, "outputs")
PLOTS_DIR   <- file.path(OUTPUT_DIR, "plots", "daily")
REF_DIR     <- file.path(BASE_DIR, "..", "excel-model", "outputs")

dir.create(PLOTS_DIR, recursive = TRUE, showWarnings = FALSE)

for (f in c("01_read_inputs.R","02_phenology.R","03_crop_lai.R",
            "04_dm_production.R","05_dm_distribution.R",
            "06_soil_water.R","07_ssm_model.R")) {
  source(file.path(CODE_DIR, f))
}

# =============================================================================
# TARGET SCENARIOS: a representative cross-section
# =============================================================================
TARGETS <- list(
  list(scenario = "JB-RFD-LTE-check",  years = c(1990, 2000, 2010)),
  list(scenario = "JB-IRRI-ELY-check", years = c(1990, 2000, 2010)),
  list(scenario = "KS-RFD-LTE-check",  years = c(1990, 2005, 2015)),
  list(scenario = "LN-RFD-LTE-check",  years = c(1990, 2005, 2015)),
  list(scenario = "LN-IRRI-MID-check", years = c(1995, 2005, 2015)),
  list(scenario = "AL-RFD-MID-check",  years = c(1995, 2005, 2015)),
  list(scenario = "AL-IRRI-MID-check", years = c(1995, 2005, 2015))
)

target_lookup <- list()
for (t in TARGETS) {
  for (yr in t$years) {
    key <- paste0(t$scenario, "_", yr)
    target_lookup[[key]] <- TRUE
  }
}

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
# STEP 1: Run full chain, capture daily for target scenario-years only
# =============================================================================
cat("Running full chain to collect daily outputs for targets...\n")
t0 <- proc.time()

scn_table   <- read_scenarios(file.path(INPUT_DIR, "scenarios.csv"))
soil_json   <- read_soil_json(file.path(INPUT_DIR, "soil_data.json"))
weather_cache <- list()
soil_cache    <- list()
daily_store   <- list()
global_ftswrz <- 0

for (i in seq_len(nrow(scn_table))) {
  scn     <- as.list(scn_table[i, ])
  wth_file <- scn$wth_file
  if (!wth_file %in% names(weather_cache)) {
    wth_path <- file.path(WEATHER_DIR, wth_file)
    if (!file.exists(wth_path)) next
    weather_cache[[wth_file]] <- read_weather(wth_path)
  }
  wth_data <- weather_cache[[wth_file]]

  soil_key <- as.character(scn$soil_row)
  if (!soil_key %in% names(soil_cache)) {
    if (soil_key %in% names(soil_json))
      soil_cache[[soil_key]] <- build_soil_from_json_entry(soil_json[[soil_key]])
    else next
  }
  soil <- soil_cache[[soil_key]]

  start_year <- as.integer(scn$fyear)
  n_years    <- as.integer(scn$yrno)

  for (yr in seq(start_year, start_year + n_years - 1)) {
    if (!yr %in% wth_data$YEAR) next
    key     <- paste0(scn$scenario, "_", yr)
    is_tgt  <- isTRUE(target_lookup[[key]])
    result  <- tryCatch(
      run_ssm_year(scn, wth_data, soil, yr, verbose = is_tgt,
                   init_ftswrz = global_ftswrz),
      error = function(e) NULL
    )
    if (!is.null(result)) {
      global_ftswrz <- result$final_ftswrz
      if (is_tgt && !is.null(result$daily) && nrow(result$daily) > 0) {
        d <- result$daily
        d$scenario <- scn$scenario
        d$year     <- yr
        d$Location <- scn$loc_name
        d$water    <- scn$water
        daily_store[[key]] <- d
        cat(sprintf("  Captured daily: %s  %d  (%d days)\n",
                    scn$scenario, yr, nrow(d)))
      }
    }
  }
}

r_daily <- do.call(rbind, daily_store)
cat(sprintf("Chain complete in %.0f s. Captured %d daily rows.\n\n",
            (proc.time() - t0)["elapsed"], nrow(r_daily)))

# =============================================================================
# STEP 2: Load matching Excel daily rows
# =============================================================================
cat("Loading Excel DailyO reference data...\n")
ref_daily_list <- list()
for (loc in names(REF_FILES)) {
  fp <- file.path(REF_DIR, REF_FILES[loc])
  if (!file.exists(fp)) next
  df <- suppressMessages(read_excel(fp, sheet = "DailyO"))
  df <- as.data.frame(df)
  df$Location <- loc
  # Keep only rows that match our targets
  df$key <- paste0(df$sName, "_", df$Pyear)
  df <- df[df$key %in% names(target_lookup), ]
  if (nrow(df) > 0) ref_daily_list[[loc]] <- df
}
ref_daily <- do.call(rbind, ref_daily_list)
cat(sprintf("Loaded %d Excel daily rows.\n\n", nrow(ref_daily)))

# =============================================================================
# STEP 3: Merge and make long-format for plotting
# =============================================================================
VARS <- c("LAI","WTOP","WGRN","CE","CTR","FTSWRZ","SEVP","TR","IRGW","DDMP")

r_long <- r_daily %>%
  select(scenario, year, Location, DOY = doy, all_of(VARS)) %>%
  pivot_longer(all_of(VARS), names_to = "variable", values_to = "r_val")

ref_long <- ref_daily %>%
  select(scenario = sName, year = Pyear, Location, DOY, all_of(VARS)) %>%
  pivot_longer(all_of(VARS), names_to = "variable", values_to = "excel_val")

both <- inner_join(r_long, ref_long, by = c("scenario","year","Location","DOY","variable"))

# =============================================================================
# STEP 4: Generate one plot per (scenario, year)
# =============================================================================
PANEL_VARS <- c("LAI","WTOP","WGRN","CE","CTR","FTSWRZ")
YLABS <- c(
  LAI    = "LAI (m²/m²)",
  WTOP   = "Total DM (g/m²)",
  WGRN   = "Grain DM (g/m²)",
  CE     = "Cum. soil evap. (mm)",
  CTR    = "Cum. transpiration (mm)",
  FTSWRZ = "FTSW root zone"
)

plot_list <- list()
targets_flat <- do.call(rbind, lapply(TARGETS, function(t)
  data.frame(scenario = t$scenario, year = t$years, stringsAsFactors = FALSE)))

for (ri in seq_len(nrow(targets_flat))) {
  scn_name <- targets_flat$scenario[ri]
  yr       <- targets_flat$year[ri]
  key      <- paste0(scn_name, "_", yr)

  sub <- both %>%
    filter(scenario == scn_name, year == yr, variable %in% PANEL_VARS) %>%
    mutate(variable = factor(variable, levels = PANEL_VARS))

  if (nrow(sub) == 0) {
    cat(sprintf("  No data for %s %d — skipping\n", scn_name, yr))
    next
  }

  loc  <- unique(sub$Location)[1]
  watr <- if (grepl("IRRI", scn_name)) "Irrigated" else "Rainfed"
  ttl  <- sprintf("%s | %s | %d (%s)", scn_name, loc, yr, watr)

  p <- ggplot(sub, aes(x = DOY)) +
    geom_line(aes(y = excel_val, colour = "Excel"), linewidth = 0.8) +
    geom_line(aes(y = r_val,    colour = "R model"), linewidth = 0.5,
              linetype = "dashed") +
    facet_wrap(~variable, scales = "free_y", ncol = 2,
               labeller = labeller(variable = YLABS)) +
    scale_colour_manual(values = c(Excel = "#1f77b4", `R model` = "#d62728"),
                        name = NULL) +
    labs(title = ttl, x = "Day of year", y = NULL) +
    theme_bw(base_size = 9) +
    theme(legend.position = "top",
          strip.background = element_rect(fill = "grey92"),
          plot.title = element_text(size = 8, face = "bold"))

  fname <- file.path(PLOTS_DIR, sprintf("daily_%s_%d.png", scn_name, yr))
  ggsave(fname, p, width = 8, height = 7, dpi = 150)
  cat(sprintf("  Saved: %s\n", basename(fname)))
  plot_list[[key]] <- p
}

# =============================================================================
# STEP 5: One summary panel — all years of a single scenario side by side
# =============================================================================
make_scenario_panel <- function(scn_name, years) {
  sub <- both %>%
    filter(scenario == scn_name, year %in% years,
           variable %in% c("LAI","WTOP","CE","FTSWRZ")) %>%
    mutate(variable = factor(variable, levels = c("LAI","WTOP","CE","FTSWRZ")),
           year = factor(year))

  if (nrow(sub) == 0) return(NULL)

  loc  <- unique(sub$Location)[1]
  watr <- if (grepl("IRRI", scn_name)) "Irrigated" else "Rainfed"

  ggplot(sub, aes(x = DOY, colour = year)) +
    geom_line(aes(y = excel_val), linewidth = 0.9, alpha = 0.85) +
    geom_line(aes(y = r_val),    linewidth = 0.5, linetype = "dashed") +
    facet_wrap(~variable, scales = "free_y", ncol = 2,
               labeller = labeller(variable = YLABS)) +
    scale_colour_brewer(palette = "Dark2", name = "Year (solid=Excel, dashed=R)") +
    labs(title = sprintf("%s — %s (%s) | multi-year", scn_name, loc, watr),
         x = "Day of year", y = NULL) +
    theme_bw(base_size = 9) +
    theme(legend.position = "top",
          strip.background = element_rect(fill = "grey92"),
          plot.title = element_text(size = 8, face = "bold"))
}

for (t in TARGETS) {
  p <- make_scenario_panel(t$scenario, t$years)
  if (is.null(p)) next
  fname <- file.path(PLOTS_DIR, sprintf("panel_%s.png", t$scenario))
  ggsave(fname, p, width = 8, height = 7, dpi = 150)
  cat(sprintf("  Saved panel: %s\n", basename(fname)))
}

cat(sprintf("\nAll daily plots saved to: %s\n", PLOTS_DIR))
