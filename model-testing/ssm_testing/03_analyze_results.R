# =============================================================================
# SSM Soybean Model — Comprehensive Testing & Evaluation
# Sites: SAREC (2024-2025), Pinetree (2024), Rohwer (2024)
# Genotypes: up to 12 varieties | Treatments: Irrigated vs Rainfed
# Initial run: all genotypes share MG4 parameters → establishes baseline
# =============================================================================

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(readxl)
  library(patchwork); library(ggrepel); library(tidyr); library(lubridate)
})

# --- Locate repo root and set key paths -------------------------------------
if (!exists("REPO_ROOT") || !dir.exists(REPO_ROOT)) {
  REPO_ROOT <- tryCatch({
    script_path <- NULL
    for (i in seq_len(sys.nframe())) {
      ofile <- sys.frame(i)$ofile
      if (!is.null(ofile) && nchar(ofile) > 0) {
        script_path <- normalizePath(ofile, mustWork = FALSE)
        break
      }
    }
    if (!is.null(script_path)) {
      candidate <- normalizePath(file.path(dirname(script_path), "..", ".."),
                                 mustWork = FALSE)
      if (file.exists(file.path(candidate, "r-model", "inputs", "scenarios.csv"))) {
        candidate
      } else {
        normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = FALSE)
      }
    } else {
      cwd <- getwd()
      if      (file.exists(file.path(cwd, "r-model", "inputs", "scenarios.csv"))) cwd
      else if (file.exists(file.path(cwd, "..", "r-model", "inputs", "scenarios.csv")))
        normalizePath(file.path(cwd, ".."), mustWork = FALSE)
      else cwd
    }
  }, error = function(e) getwd())
}

TESTING_DIR <- file.path(REPO_ROOT, "model-testing", "ssm_testing")
DATA_DIR    <- file.path(REPO_ROOT, "model-testing", "data analysis", "input")

# Output directories — all under outputs/
OUT_DIR     <- file.path(TESTING_DIR, "outputs")
PLOT_DIR    <- file.path(OUT_DIR, "plots")
RESULTS_DIR <- file.path(OUT_DIR, "results")
DAILY_DIR   <- file.path(RESULTS_DIR, "daily")

dir.create(PLOT_DIR,    recursive = TRUE, showWarnings = FALSE)
dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)

cat("=== SSM Soybean Model Testing — Comprehensive Analysis ===\n\n")
cat("Repo root  :", REPO_ROOT, "\n")
cat("Output dir :", OUT_DIR, "\n\n")

# Genotype name map: raw code from scenario name → clean display name
geno_lookup <- c(
  "P42A84E"   = "P42A84E",
  "P48A14E"   = "P48A14E",
  "P52A14SE"  = "P52A14SE",
  "PI471938"  = "PI471938",
  "PI507408"  = "PI507408",
  "PI548431"  = "PI548431",
  "PI603457A" = "PI603457A",
  "R1814502"  = "R18-14502",
  "R18C13665" = "R18C-13665",
  "R1945980"  = "R19-45980",
  "R1946252"  = "R19-46252",
  "R19C1012"  = "R19C-1012"
)

# Helper: parse scenario name robustly
# Format: TEST-{SITE}-{YEAR}-{TREAT}-{GENO}
# GENO can contain hyphens, so we take everything after the 4th token
parse_scenario <- function(sname) {
  parts     <- strsplit(as.character(sname), "-")[[1]]
  site      <- if (length(parts) >= 2) tolower(parts[2]) else NA_character_
  year      <- if (length(parts) >= 3) as.integer(parts[3]) else NA_integer_
  treat_code <- if (length(parts) >= 4) parts[4] else NA_character_
  geno_raw  <- if (length(parts) >= 5) paste(parts[5:length(parts)], collapse = "") else NA_character_
  Treatment <- ifelse(!is.na(treat_code) & treat_code == "IRRI", "Irrigated", "Rainfed")
  Genotype  <- if (!is.na(geno_raw)) {
    ifelse(geno_raw %in% names(geno_lookup), geno_lookup[geno_raw], geno_raw)
  } else NA_character_
  data.frame(site = site, Year = year, treat_code = treat_code,
             Treatment = Treatment, geno_raw = geno_raw, Genotype = Genotype,
             stringsAsFactors = FALSE)
}

# ============================================================
# 1. LOAD OBSERVED DATA
# ============================================================
obs_file <- file.path(DATA_DIR, "soybean_data_input.xlsx")
if (!file.exists(obs_file)) stop("Observed data not found: ", obs_file)

obs_raw <- read_excel(obs_file, sheet = "all mm")

obs_all <- obs_raw %>%
  filter(!is.na(Genotype), !is.na(Treatment), !is.na(Location)) %>%
  mutate(
    Genotype  = trimws(as.character(Genotype)),
    Treatment = trimws(as.character(Treatment)),
    Location  = trimws(tolower(as.character(Location))),
    Year      = as.integer(Year),
    Harvest   = as.character(Harvest),
    DOY       = as.integer(DOY)
  )

obs_final <- obs_all %>%
  filter(Harvest == "final") %>%
  group_by(Year, Location, Treatment, Genotype) %>%
  summarise(
    obs_yield   = mean(yield_kg_ha,       na.rm = TRUE),
    obs_biomass = mean(TotalBiomass_kg_ha, na.rm = TRUE),
    obs_HI      = mean(HI,                na.rm = TRUE),
    n_reps      = n(),
    .groups = "drop"
  ) %>%
  filter(!is.na(obs_yield), obs_yield > 100)

obs_inseason <- obs_all %>%
  filter(!is.na(TotalBiomass_kg_ha), !is.na(DOY), TotalBiomass_kg_ha > 0) %>%
  group_by(Year, Location, Treatment, Genotype, Harvest, DOY, DAP) %>%
  summarise(
    obs_biomass  = mean(TotalBiomass_kg_ha, na.rm = TRUE),
    obs_seed     = mean(SeedBiomass,        na.rm = TRUE),
    obs_stem     = mean(StemBiomass,        na.rm = TRUE),
    obs_green    = mean(GreenBiomass,       na.rm = TRUE),
    n_reps       = n(),
    .groups = "drop"
  )

cat(sprintf("Observed records (final harvest): %d\n", nrow(obs_final)))
cat("Locations:", paste(sort(unique(obs_final$Location)), collapse = ", "), "\n")
cat("Genotypes:", paste(sort(unique(obs_final$Genotype)), collapse = ", "), "\n\n")

# ============================================================
# 2. LOAD SIMULATED RESULTS
# ============================================================
# Check new output path first, fall back to old path for backward compat
sim_new <- file.path(RESULTS_DIR, "test_results_yearly.csv")
sim_old <- file.path(TESTING_DIR, "results", "test_results_yearly.csv")

if (file.exists(sim_new)) {
  sim_file <- sim_new
  cat("Reading simulated results from: outputs/results/test_results_yearly.csv\n")
} else if (file.exists(sim_old)) {
  sim_file <- sim_old
  cat("NOTE: Reading from old path (results/test_results_yearly.csv).\n")
  cat("Run run_model_testing.R to generate outputs in the new location.\n\n")
} else {
  stop(paste(
    "Simulated results not found. Please run:\n",
    "  source('model-testing/ssm_testing/run_model_testing.R')\n",
    "to generate test results first."
  ))
}

sim_raw <- read.csv(sim_file, stringsAsFactors = FALSE)

# Parse scenario names and join with model output
parsed <- do.call(rbind, lapply(sim_raw$sName, parse_scenario))

sim <- cbind(sim_raw, parsed) %>%
  mutate(
    sim_yield   = Ywet,
    sim_biomass = WTOP * 10,          # g/m² → kg/ha
    sim_HI      = HI,
    sim_LAI     = MXLAI,
    sim_R1_DOY  = as.integer(Pdoy) + as.integer(R1) - 1,
    sim_R3_DOY  = as.integer(Pdoy) + as.integer(R3) - 1,
    sim_R5_DOY  = as.integer(Pdoy) + as.integer(R5) - 1,
    sim_R7_DOY  = as.integer(Pdoy) + as.integer(R7) - 1,
    sim_R8_DOY  = as.integer(Pdoy) + as.integer(R8) - 1,
    sim_R1_DAP  = as.integer(R1),
    sim_R8_DAP  = as.integer(R8)
  )

cat(sprintf("Simulated scenarios: %d\n", nrow(sim)))
cat(sprintf("Simulated Ywet range: %.0f - %.0f kg/ha\n",
            min(sim$sim_yield, na.rm = TRUE), max(sim$sim_yield, na.rm = TRUE)))
cat("NOTE: All genotypes share MG4 parameters in this initial run.\n\n")

# ============================================================
# 3. JOIN OBS vs SIM (FINAL HARVEST)
# ============================================================
df <- obs_final %>%
  inner_join(
    sim %>% select(Year, site, Treatment, Genotype,
                   sim_yield, sim_biomass, sim_HI, sim_LAI,
                   sim_R1_DOY, sim_R8_DOY, sim_R1_DAP, sim_R8_DAP,
                   CRAIN, CIRGW, R8),
    by = c("Year" = "Year", "Location" = "site",
           "Treatment" = "Treatment", "Genotype" = "Genotype")
  )

cat(sprintf("Matched obs-sim pairs: %d\n\n", nrow(df)))

df %>%
  group_by(Treatment) %>%
  summarise(
    n        = n(),
    obs_mean = round(mean(obs_yield), 0),
    sim_mean = round(mean(sim_yield), 0),
    bias     = round(mean(sim_yield - obs_yield), 0),
    bias_pct = round(mean((sim_yield - obs_yield) / obs_yield * 100), 1),
    .groups  = "drop"
  ) %>%
  { cat("=== Yield summary by treatment (all sites, all genotypes) ===\n");
    print(.); cat("\n") }

# ============================================================
# 4. STATISTICS
# ============================================================
calc_stats <- function(obs, sim, label = "") {
  v <- complete.cases(obs, sim) & is.finite(obs) & is.finite(sim)
  o <- obs[v]; s <- sim[v]; n <- length(o)
  if (n < 2) return(data.frame(Variable = label, n = n,
                               RMSE = NA, RRMSE_pct = NA, Bias = NA, R2 = NA))
  data.frame(Variable  = label, n = n,
             RMSE      = round(sqrt(mean((s - o)^2)), 1),
             RRMSE_pct = round(sqrt(mean((s - o)^2)) / mean(o) * 100, 1),
             Bias      = round(mean(s - o), 1),
             R2        = round(cor(o, s)^2, 3))
}

stats <- bind_rows(
  calc_stats(df$obs_yield,   df$sim_yield,   "Yield (kg/ha) - All"),
  calc_stats(df$obs_biomass, df$sim_biomass, "Biomass (kg/ha) - All"),
  calc_stats(df$obs_HI,      df$sim_HI,      "Harvest Index - All")
)
for (trt in c("Irrigated", "Rainfed")) {
  s <- df %>% filter(Treatment == trt)
  stats <- bind_rows(stats,
    calc_stats(s$obs_yield,   s$sim_yield,   paste0("Yield (kg/ha) - ", trt)),
    calc_stats(s$obs_biomass, s$sim_biomass, paste0("Biomass (kg/ha) - ", trt)),
    calc_stats(s$obs_HI,      s$sim_HI,      paste0("HI - ", trt))
  )
}
for (yr in c(2024, 2025)) {
  s <- df %>% filter(Year == yr)
  if (nrow(s) > 1)
    stats <- bind_rows(stats,
      calc_stats(s$obs_yield, s$sim_yield, paste0("Yield (kg/ha) - ", yr)))
}

stats_file <- file.path(OUT_DIR, "model_statistics.csv")
write.csv(stats, stats_file, row.names = FALSE)
cat("=== PERFORMANCE STATISTICS ===\n")
print(stats, row.names = FALSE)
cat("\n")

# ============================================================
# 5. PLOTS 1-9 (existing, improved)
# ============================================================

# Helper: nice axis limits for 1:1 plots
lim <- function(x, y, pad = 0.05) {
  mx <- max(c(x, y), na.rm = TRUE) * (1 + pad)
  c(0, ceiling(mx / 500) * 500)
}

## --- Plot 1: 1:1 yield scatter (all data) -----------------------------------
ylims    <- lim(df$obs_yield, df$sim_yield)
stat_ann <- stats %>% filter(Variable == "Yield (kg/ha) - All")

p1 <- ggplot(df, aes(obs_yield, sim_yield,
                     color = Treatment, shape = as.factor(Year))) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "gray40", linewidth = 0.8) +
  geom_point(size = 2.8, alpha = 0.75) +
  scale_color_manual(values = c(Irrigated = "#2196F3", Rainfed = "#FF7043")) +
  scale_shape_manual(values = c("2024" = 16, "2025" = 17), name = "Year") +
  coord_equal(xlim = ylims, ylim = ylims) +
  annotate("text", x = ylims[1] + 200, y = ylims[2] * 0.95,
           hjust = 0, size = 3.5,
           label = sprintf("RMSE  = %.0f kg/ha\nRRMSE = %.1f%%\nBias  = %+.0f kg/ha\nR2    = %.3f",
                           stat_ann$RMSE, stat_ann$RRMSE_pct,
                           stat_ann$Bias, stat_ann$R2),
           family = "mono") +
  labs(title = "Observed vs Simulated Soybean Yield",
       subtitle = "All sites (SAREC, Pinetree, Rohwer) | 2024-2025 | Initial MG4 parameters",
       x = "Observed Yield (kg/ha)", y = "Simulated Yield (kg/ha)",
       color = "Treatment") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), legend.position = "right")
ggsave(file.path(PLOT_DIR, "01_yield_1to1_all.png"), p1,
       width = 8, height = 7.5, dpi = 200)
cat("Plot 1 saved\n")

## --- Plot 2: 1:1 by treatment panels ----------------------------------------
stats_trt <- df %>%
  group_by(Treatment) %>%
  summarise(
    RMSE  = round(sqrt(mean((sim_yield - obs_yield)^2, na.rm = TRUE)), 0),
    RRMSE = round(sqrt(mean((sim_yield - obs_yield)^2, na.rm = TRUE)) /
                    mean(obs_yield, na.rm = TRUE) * 100, 1),
    Bias  = round(mean(sim_yield - obs_yield, na.rm = TRUE), 0),
    R2    = round(cor(obs_yield, sim_yield, use = "complete.obs")^2, 3),
    xpos  = 200,
    ypos  = max(sim_yield, na.rm = TRUE) * 0.95,
    .groups = "drop"
  )

p2 <- ggplot(df, aes(obs_yield, sim_yield, color = Genotype)) +
  geom_abline(slope = 1, linetype = "dashed", color = "gray40") +
  geom_point(size = 2.5, alpha = 0.85) +
  geom_text_repel(aes(label = Genotype), size = 2.2,
                  max.overlaps = 8, show.legend = FALSE) +
  facet_wrap(~Treatment) +
  geom_text(data = stats_trt,
            aes(x = xpos, y = ypos,
                label = sprintf("RMSE=%.0f  RRMSE=%.1f%%\nBias=%+.0f  R2=%.3f",
                                RMSE, RRMSE, Bias, R2)),
            inherit.aes = FALSE, hjust = 0, size = 3, family = "mono") +
  labs(title = "Yield 1:1 by Treatment | All Sites & Years",
       x = "Observed (kg/ha)", y = "Simulated (kg/ha)", color = "Genotype") +
  theme_bw(base_size = 10) +
  theme(legend.position = "right", plot.title = element_text(face = "bold"))
ggsave(file.path(PLOT_DIR, "02_yield_by_treatment.png"), p2,
       width = 14, height = 7, dpi = 200)
cat("Plot 2 saved\n")

## --- Plot 3: Observed vs Simulated yield per genotype (SAREC, bar) ----------
df_sarec <- df %>%
  filter(Location == "sarec") %>%
  select(Year, Treatment, Genotype, obs_yield, sim_yield) %>%
  pivot_longer(cols = c(obs_yield, sim_yield),
               names_to = "Source", values_to = "Yield") %>%
  mutate(Source = recode(Source, "obs_yield" = "Observed", "sim_yield" = "Simulated"))

p3 <- ggplot(df_sarec, aes(x = Genotype, y = Yield, fill = Source)) +
  geom_col(position = position_dodge(0.75), width = 0.65, alpha = 0.85) +
  facet_grid(Year ~ Treatment) +
  scale_fill_manual(values = c(Observed = "#2C3E50", Simulated = "#E74C3C")) +
  scale_y_continuous(limits = c(0, 11000), breaks = seq(0, 10000, 2000)) +
  geom_hline(yintercept = 0) +
  labs(title = "Observed vs Simulated Yield by Genotype — SAREC",
       subtitle = "Initial run: all genotypes share MG4 parameters",
       x = "", y = "Yield (kg/ha)", fill = "") +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        plot.title = element_text(face = "bold"),
        strip.background = element_rect(fill = "#ECF0F1"))
ggsave(file.path(PLOT_DIR, "03_yield_by_genotype_sarec.png"), p3,
       width = 14, height = 10, dpi = 200)
cat("Plot 3 saved\n")

## --- Plot 4: In-season biomass SAREC 2024 -----------------------------------
# Determine which daily directory to read from (new path first, old path fallback)
daily_dir_new <- file.path(RESULTS_DIR, "daily")
daily_dir_old <- file.path(TESTING_DIR, "results", "daily")
daily_dir_use <- if (length(list.files(daily_dir_new, "*.csv")) > 0) daily_dir_new else daily_dir_old

daily_files <- list.files(daily_dir_use, "\\.csv$", full.names = TRUE)

if (length(daily_files) > 0) {
  daily_all <- lapply(daily_files, function(f) {
    nm    <- tools::file_path_sans_ext(basename(f))
    nm    <- sub("_daily$", "", nm)
    d     <- tryCatch(read.csv(f), error = function(e) NULL)
    if (is.null(d) || nrow(d) == 0) return(NULL)
    # Parse scenario name robustly: TEST-SITE-YEAR-TREAT-GENO
    parts <- strsplit(nm, "-")[[1]]
    if (length(parts) < 5) return(NULL)
    site       <- tolower(parts[2])
    year_run   <- as.integer(parts[3])
    treat_code <- parts[4]
    geno_raw   <- paste(parts[5:length(parts)], collapse = "")
    d$site       <- site
    d$Year_run   <- year_run
    d$treat_code <- treat_code
    d$geno_raw   <- geno_raw
    d$Treatment  <- ifelse(treat_code == "IRRI", "Irrigated", "Rainfed")
    d$Genotype   <- ifelse(geno_raw %in% names(geno_lookup),
                           geno_lookup[geno_raw], geno_raw)
    d$sim_biomass <- d$WTOP * 10   # g/m² → kg/ha
    d
  })
  daily_all <- do.call(rbind, Filter(Negate(is.null), daily_all))

  obs24 <- obs_inseason %>%
    filter(Location == "sarec", Year == 2024, !is.na(obs_biomass))
  sim24 <- daily_all %>%
    filter(site == "sarec", Year_run == 2024, DAP >= 0)
  focus_genos <- c("P42A84E", "P48A14E", "R18-14502",
                   "R19C-1012", "PI548431", "R19-45980")
  obs24_sub <- obs24 %>% filter(Genotype %in% focus_genos)
  sim24_sub <- sim24 %>% filter(Genotype %in% focus_genos)

  if (nrow(sim24_sub) > 0) {
    p4 <- ggplot() +
      geom_line(data = sim24_sub, aes(DAP, sim_biomass, linetype = Treatment),
                color = "#E74C3C", linewidth = 0.9, alpha = 0.8) +
      geom_point(data = obs24_sub, aes(DAP, obs_biomass, shape = Treatment),
                 color = "#2C3E50", size = 2.8) +
      scale_linetype_manual(values = c(Irrigated = "solid", Rainfed = "dashed")) +
      scale_shape_manual(values = c(Irrigated = 16, Rainfed = 1)) +
      facet_wrap(~Genotype, ncol = 3, scales = "free_y") +
      labs(title = "In-Season Biomass: Simulated (red lines) vs Observed (black points)",
           subtitle = "SAREC 2024 | NOTE: all simulated lines identical across genotypes (MG4 parameters)",
           x = "Days After Planting (DAP)", y = "Total Aboveground Biomass (kg/ha)",
           linetype = "Treatment", shape = "Treatment") +
      theme_bw(base_size = 10) +
      theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
            strip.background = element_rect(fill = "#ECF0F1"))
    ggsave(file.path(PLOT_DIR, "04_inseason_biomass.png"), p4,
           width = 14, height = 10, dpi = 200)
    cat("Plot 4 saved\n")
  } else {
    cat("Plot 4 skipped (no SAREC 2024 daily data found)\n")
  }
} else {
  daily_all <- data.frame()
  cat("Plot 4 skipped (no daily CSV files found — run with SAVE_DAILY=TRUE)\n")
}

## --- Plot 5: Irrigated vs Rainfed yield ratio --------------------------------
reduce_obs <- obs_final %>%
  filter(Location == "sarec") %>%
  select(Year, Genotype, Treatment, obs_yield) %>%
  pivot_wider(names_from = Treatment, values_from = obs_yield) %>%
  filter(!is.na(Irrigated), !is.na(Rainfed)) %>%
  mutate(reduction_pct = (1 - Rainfed / Irrigated) * 100, Source = "Observed")

reduce_sim <- sim %>%
  filter(site == "sarec") %>%
  select(Year, Genotype, Treatment, sim_yield) %>%
  pivot_wider(names_from = Treatment, values_from = sim_yield) %>%
  filter(!is.na(Irrigated), !is.na(Rainfed)) %>%
  mutate(reduction_pct = (1 - Rainfed / Irrigated) * 100, Source = "Simulated")

if (nrow(reduce_obs) > 0 && nrow(reduce_sim) > 0) {
  reduce_all <- bind_rows(
    reduce_obs %>% mutate(Year = as.factor(Year)),
    reduce_sim %>% mutate(Year = as.factor(Year))
  )

  p5 <- ggplot(reduce_all,
               aes(x = reorder(Genotype, -reduction_pct), y = reduction_pct,
                   fill = Source, alpha = Year)) +
    geom_col(position = position_dodge(0.8), width = 0.7) +
    facet_wrap(~Year) +
    scale_fill_manual(values = c(Observed = "#2C3E50", Simulated = "#E74C3C")) +
    scale_alpha_manual(values = c("2024" = 0.9, "2025" = 0.8), guide = "none") +
    coord_flip() +
    labs(title = "Rainfed Yield Reduction vs Irrigated (%)",
         subtitle = "SAREC | Obs (dark) vs Sim (red)",
         x = "", y = "Yield Reduction (%)", fill = "") +
    theme_bw(base_size = 10) +
    theme(legend.position = "top", plot.title = element_text(face = "bold"),
          strip.background = element_rect(fill = "#ECF0F1"))
  ggsave(file.path(PLOT_DIR, "05_yield_reduction.png"), p5,
         width = 12, height = 7, dpi = 200)
  cat("Plot 5 saved\n")
} else {
  cat("Plot 5 skipped (insufficient data for yield reduction comparison)\n")
}

## --- Plot 6: Phenology (R1 and R8) ------------------------------------------
pheno_file <- file.path(DATA_DIR, "phenology.xlsx")
pheno_sheets <- tryCatch(excel_sheets(pheno_file), error = function(e) character(0))

# Use "phenology" sheet (has all locations), fall back to "phenology_sarec_2024"
pheno_sheet <- if ("phenology" %in% pheno_sheets) "phenology" else
               if ("phenology_sarec_2024" %in% pheno_sheets) "phenology_sarec_2024" else NA

if (!is.na(pheno_sheet) && file.exists(pheno_file)) {
  pheno_obs_raw <- read_excel(pheno_file, sheet = pheno_sheet)

  pheno_obs <- pheno_obs_raw %>%
    filter(!is.na(Phenology), !is.na(Date)) %>%
    mutate(
      DOY_obs   = yday(as.Date(Date)),
      Genotype  = trimws(as.character(Genotype)),
      Treatment = trimws(as.character(Treatment)),
      Location  = tolower(trimws(as.character(Location)))
    )

  pheno_R1 <- pheno_obs %>% filter(Phenology == "R1") %>%
    group_by(Location, Genotype, Treatment) %>%
    summarise(obs_R1_DOY = median(DOY_obs, na.rm = TRUE), .groups = "drop")

  pheno_R8 <- pheno_obs %>% filter(Phenology == "R8") %>%
    group_by(Location, Genotype, Treatment) %>%
    summarise(obs_R8_DOY = median(DOY_obs, na.rm = TRUE), .groups = "drop")

  sim_pheno <- sim %>%
    filter(site == "sarec", Year == 2024) %>%
    select(Genotype, Treatment, sim_R1_DOY, sim_R8_DOY)

  pheno_comp <- pheno_R1 %>%
    filter(Location == "sarec") %>%
    inner_join(pheno_R8 %>% filter(Location == "sarec"),
               by = c("Location", "Genotype", "Treatment")) %>%
    inner_join(sim_pheno, by = c("Genotype", "Treatment")) %>%
    mutate(R1_err = sim_R1_DOY - obs_R1_DOY,
           R8_err = sim_R8_DOY - obs_R8_DOY)

  cat("\n=== Phenology errors (sim - obs, DOY), SAREC 2024 ===\n")
  print(pheno_comp %>%
          select(Genotype, Treatment, obs_R1_DOY, sim_R1_DOY, R1_err,
                 obs_R8_DOY, sim_R8_DOY, R8_err),
        row.names = FALSE)

  if (nrow(pheno_comp) > 0) {
    p6a <- ggplot(pheno_comp, aes(obs_R1_DOY, sim_R1_DOY,
                                  color = Genotype, shape = Treatment)) +
      geom_abline(slope = 1, linetype = "dashed", color = "gray40") +
      geom_point(size = 3) +
      geom_text_repel(aes(label = Genotype), size = 2.5,
                      max.overlaps = 10, show.legend = FALSE) +
      annotate("text",
               x = min(pheno_comp$obs_R1_DOY, na.rm = TRUE),
               y = max(pheno_comp$sim_R1_DOY, na.rm = TRUE),
               hjust = 0, size = 3.5,
               label = sprintf("Mean error: %+.1f d",
                               mean(pheno_comp$R1_err, na.rm = TRUE))) +
      labs(title = "R1 (Flowering) DOY", x = "Observed", y = "Simulated") +
      theme_bw(base_size = 10) + theme(legend.position = "bottom")

    p6b <- ggplot(pheno_comp, aes(obs_R8_DOY, sim_R8_DOY,
                                  color = Genotype, shape = Treatment)) +
      geom_abline(slope = 1, linetype = "dashed", color = "gray40") +
      geom_point(size = 3) +
      geom_text_repel(aes(label = Genotype), size = 2.5,
                      max.overlaps = 10, show.legend = FALSE) +
      annotate("text",
               x = min(pheno_comp$obs_R8_DOY, na.rm = TRUE),
               y = max(pheno_comp$sim_R8_DOY, na.rm = TRUE),
               hjust = 0, size = 3.5,
               label = sprintf("Mean error: %+.1f d",
                               mean(pheno_comp$R8_err, na.rm = TRUE))) +
      labs(title = "R8 (Maturity) DOY", x = "Observed", y = "Simulated") +
      theme_bw(base_size = 10) + theme(legend.position = "bottom")

    p6 <- p6a + p6b +
      plot_annotation(
        title    = "Phenology: Simulated vs Observed (SAREC 2024)",
        subtitle = "NOTE: all genotypes share same MG4 parameters - identical simulated DOY")
    ggsave(file.path(PLOT_DIR, "06_phenology.png"), p6,
           width = 14, height = 7, dpi = 200)
    cat("\nPlot 6 saved\n")
  }
} else {
  pheno_comp <- data.frame()
  cat("Plot 6 skipped (phenology.xlsx not found)\n")
}

## --- Plot 7: HI analysis (SAREC) --------------------------------------------
df_sarec_hi <- df %>% filter(Location == "sarec")
if (nrow(df_sarec_hi) > 0) {
  p7 <- ggplot(df_sarec_hi, aes(x = obs_biomass, y = obs_yield, color = Treatment)) +
    geom_point(aes(shape = "Observed"), size = 2.5, alpha = 0.8) +
    geom_point(aes(x = sim_biomass, y = sim_yield, shape = "Simulated"),
               size = 3, alpha = 0.8) +
    geom_abline(slope = 0.5, linetype = "dashed", color = "gray60", linewidth = 0.7) +
    scale_color_manual(values = c(Irrigated = "#2196F3", Rainfed = "#FF7043")) +
    scale_shape_manual(values = c(Observed = 16, Simulated = 8), name = "") +
    facet_wrap(~Year) +
    labs(title = "Biomass vs Yield (HI Analysis) - SAREC",
         subtitle = "Dashed line = HI 0.50 | Points=Observed, Stars=Simulated",
         x = "Total Biomass (kg/ha)", y = "Seed Yield (kg/ha)",
         color = "Treatment") +
    theme_bw(base_size = 10) +
    theme(legend.position = "right", plot.title = element_text(face = "bold"))
  ggsave(file.path(PLOT_DIR, "07_HI_analysis.png"), p7,
         width = 12, height = 6, dpi = 200)
  cat("Plot 7 saved\n")
}

## --- Plot 8: Relative error per genotype ------------------------------------
df_err <- df %>%
  filter(Location == "sarec") %>%
  group_by(Genotype, Treatment) %>%
  summarise(
    obs_mean = mean(obs_yield, na.rm = TRUE),
    sim_mean = mean(sim_yield, na.rm = TRUE),
    rel_err  = mean((sim_yield - obs_yield) / obs_yield * 100, na.rm = TRUE),
    .groups  = "drop"
  )

if (nrow(df_err) > 0) {
  p8 <- ggplot(df_err, aes(x = reorder(Genotype, -rel_err), y = rel_err,
                            fill = Treatment)) +
    geom_col(position = position_dodge(0.75), width = 0.65, alpha = 0.85) +
    geom_hline(yintercept = 0, linewidth = 0.8) +
    geom_hline(yintercept = c(-20, 20), linetype = "dashed", color = "gray50") +
    scale_fill_manual(values = c(Irrigated = "#2196F3", Rainfed = "#FF7043")) +
    facet_wrap(~Treatment, ncol = 2) +
    labs(title = "Relative Yield Error by Genotype - SAREC (averaged 2024+2025)",
         subtitle = "(Simulated - Observed) / Observed x 100 | Dashed = +/-20% threshold",
         x = "", y = "Relative Error (%)", fill = "Treatment") +
    theme_bw(base_size = 10) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none",
          plot.title = element_text(face = "bold"))
  ggsave(file.path(PLOT_DIR, "08_relative_error.png"), p8,
         width = 12, height = 7, dpi = 200)
  cat("Plot 8 saved\n")
}

## --- Plot 9: Multi-site comparison ------------------------------------------
p9 <- ggplot(df, aes(obs_yield, sim_yield, color = Treatment, shape = Location)) +
  geom_abline(slope = 1, linetype = "dashed", color = "gray40") +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = c(Irrigated = "#2196F3", Rainfed = "#FF7043")) +
  scale_shape_manual(values = c(sarec = 16, pinetree = 17, rohwer = 15)) +
  facet_wrap(~as.factor(Year)) +
  labs(title = "Multi-Site Yield Comparison: 2024 vs 2025",
       x = "Observed (kg/ha)", y = "Simulated (kg/ha)",
       color = "Treatment", shape = "Location") +
  theme_bw(base_size = 10) +
  theme(legend.position = "right", plot.title = element_text(face = "bold"))
ggsave(file.path(PLOT_DIR, "09_multisite.png"), p9,
       width = 12, height = 6, dpi = 200)
cat("Plot 9 saved\n")

# ============================================================
# NEW SECTION A: PHENOLOGY ANALYSIS (PLOTS 10-11)
# ============================================================
cat("\n--- Phenology analysis (Plots 10-11) ---\n")

if (nrow(pheno_comp) > 0 && exists("pheno_obs")) {
  # Build table with all key stages: R1, R3, R5, R7, R8
  key_stages <- c("R1", "R3", "R5", "R7", "R8")

  pheno_stages_obs <- pheno_obs %>%
    filter(Phenology %in% key_stages, Location == "sarec") %>%
    group_by(Genotype, Treatment, Phenology) %>%
    summarise(obs_DOY = median(DOY_obs, na.rm = TRUE), .groups = "drop")

  # Simulated DOY for each stage from yearly results
  pheno_stages_sim <- sim %>%
    filter(site == "sarec", Year == 2024) %>%
    select(Genotype, Treatment,
           R1 = sim_R1_DOY, R3 = sim_R3_DOY,
           R5 = sim_R5_DOY, R7 = sim_R7_DOY, R8 = sim_R8_DOY) %>%
    pivot_longer(cols = c(R1, R3, R5, R7, R8),
                 names_to = "Phenology", values_to = "sim_DOY")

  pheno_stages_comp <- pheno_stages_obs %>%
    inner_join(pheno_stages_sim, by = c("Genotype", "Treatment", "Phenology")) %>%
    mutate(Phenology = factor(Phenology, levels = key_stages))

  if (nrow(pheno_stages_comp) > 0) {
    ## Plot 10: Bar chart — observed vs simulated DOY per stage, by genotype
    pheno_long <- pheno_stages_comp %>%
      pivot_longer(cols = c(obs_DOY, sim_DOY),
                   names_to = "Source", values_to = "DOY") %>%
      mutate(Source = recode(Source, "obs_DOY" = "Observed", "sim_DOY" = "Simulated"))

    p10 <- ggplot(pheno_long, aes(x = Genotype, y = DOY, fill = Source)) +
      geom_col(position = position_dodge(0.75), width = 0.65, alpha = 0.85) +
      facet_grid(Phenology ~ Treatment, scales = "free_y") +
      scale_fill_manual(values = c(Observed = "#2C3E50", Simulated = "#E74C3C")) +
      labs(title = "Observed vs Simulated Phenology Stages — SAREC 2024",
           subtitle = "Bar chart: day-of-year for R1, R3, R5, R7, R8",
           x = "", y = "Day of Year (DOY)", fill = "") +
      theme_bw(base_size = 9) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "top",
            plot.title = element_text(face = "bold"),
            strip.background = element_rect(fill = "#ECF0F1"))
    ggsave(file.path(PLOT_DIR, "10_phenology_stages.png"), p10,
           width = 14, height = 14, dpi = 200)
    cat("Plot 10 saved\n")

    ## Plot 11: Observed vs simulated R1 DOY scatter (1:1 line)
    r1_comp <- pheno_stages_comp %>% filter(Phenology == "R1")
    if (nrow(r1_comp) > 2) {
      xlim11 <- range(c(r1_comp$obs_DOY, r1_comp$sim_DOY), na.rm = TRUE)
      xlim11 <- c(floor(xlim11[1] / 5) * 5, ceiling(xlim11[2] / 5) * 5)

      p11 <- ggplot(r1_comp, aes(obs_DOY, sim_DOY, color = Genotype, shape = Treatment)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
        geom_point(size = 3.5, alpha = 0.85) +
        geom_text_repel(aes(label = Genotype), size = 2.8,
                        max.overlaps = 12, show.legend = FALSE) +
        coord_equal(xlim = xlim11, ylim = xlim11) +
        annotate("text",
                 x = xlim11[1] + 1, y = xlim11[2] - 1,
                 hjust = 0, vjust = 1, size = 3.5,
                 label = sprintf("Mean error: %+.1f d\nRMSE: %.1f d",
                                 mean(r1_comp$sim_DOY - r1_comp$obs_DOY, na.rm = TRUE),
                                 sqrt(mean((r1_comp$sim_DOY - r1_comp$obs_DOY)^2,
                                          na.rm = TRUE)))) +
        labs(title = "R1 (Flowering) DOY: Simulated vs Observed",
             subtitle = "SAREC 2024 | 1:1 line = perfect prediction",
             x = "Observed R1 DOY", y = "Simulated R1 DOY",
             color = "Genotype", shape = "Treatment") +
        theme_bw(base_size = 11) +
        theme(legend.position = "right", plot.title = element_text(face = "bold"))
      ggsave(file.path(PLOT_DIR, "11_phenology_scatter.png"), p11,
             width = 9, height = 8, dpi = 200)
      cat("Plot 11 saved\n")
    }
  } else {
    cat("Plots 10-11 skipped (no matched phenology data)\n")
  }
} else {
  cat("Plots 10-11 skipped (phenology data not loaded)\n")
}

# ============================================================
# NEW SECTION B: BIOMASS PARTITIONING (PLOTS 12-14)
# ============================================================
cat("\n--- Biomass partitioning analysis (Plots 12-14) ---\n")

if (exists("daily_all") && nrow(daily_all) > 0 && nrow(obs_inseason) > 0) {

  obs_sarec_bm <- obs_inseason %>%
    filter(Location == "sarec", Year == 2024,
           !is.na(obs_biomass), obs_biomass > 0) %>%
    mutate(
      obs_HI_inseason = ifelse(!is.na(obs_seed) & obs_biomass > 0,
                               obs_seed / obs_biomass, NA_real_)
    )

  sim_sarec_daily <- daily_all %>%
    filter(site == "sarec", Year_run == 2024) %>%
    mutate(
      sim_biomass_ha = WTOP * 10,
      sim_seed_ha    = WGRN * 10,
      sim_stem_ha    = WST  * 10,
      sim_HI_daily   = ifelse(WTOP > 0, WGRN / WTOP, NA_real_)
    )

  # Match obs to sim by DOY (nearest DOY within +-3 days)
  # Use a join approach instead of rowwise for better performance
  if (!is.null(sim_sarec_daily$doy)) {
    # Average sim daily output across genotypes per Treatment+DOY (baseline: all genos same)
    sim_avg_daily <- sim_sarec_daily %>%
      group_by(Treatment, doy) %>%
      summarise(
        sim_biomass_ha = mean(sim_biomass_ha, na.rm = TRUE),
        sim_seed_ha    = mean(sim_seed_ha,    na.rm = TRUE),
        sim_stem_ha    = mean(sim_stem_ha,    na.rm = TRUE),
        sim_HI_daily   = mean(sim_HI_daily,   na.rm = TRUE),
        .groups = "drop"
      )

    # For each obs row, find nearest sim DOY within +-3 days
    match_sim_doy <- function(obs_doy, treatment, sim_df) {
      sub <- sim_df %>% filter(Treatment == treatment, abs(doy - obs_doy) <= 3)
      if (nrow(sub) == 0) return(data.frame(sim_biomass_ha=NA_real_, sim_seed_ha=NA_real_,
                                             sim_stem_ha=NA_real_, sim_HI_val=NA_real_))
      idx <- which.min(abs(sub$doy - obs_doy))
      data.frame(sim_biomass_ha = sub$sim_biomass_ha[idx],
                 sim_seed_ha    = sub$sim_seed_ha[idx],
                 sim_stem_ha    = sub$sim_stem_ha[idx],
                 sim_HI_val     = sub$sim_HI_daily[idx])
    }

    matched_list <- mapply(
      match_sim_doy,
      obs_doy   = obs_sarec_bm$DOY,
      treatment = obs_sarec_bm$Treatment,
      MoreArgs  = list(sim_df = sim_avg_daily),
      SIMPLIFY  = FALSE
    )
    matched_extra <- do.call(rbind, matched_list)

    bm_matched <- cbind(obs_sarec_bm, matched_extra)

    ## Plot 12: In-season total biomass (obs vs sim) by Location x Treatment
    if (nrow(bm_matched) > 0 && any(!is.na(bm_matched$sim_biomass_ha))) {
      p12 <- ggplot() +
        # Simulated lines from daily output (full season)
        geom_line(
          data = sim_sarec_daily %>%
            group_by(Treatment, doy) %>%
            summarise(sim_biomass_ha = mean(sim_biomass_ha, na.rm = TRUE),
                      .groups = "drop"),
          aes(doy, sim_biomass_ha, color = Treatment, linetype = "Simulated"),
          linewidth = 1
        ) +
        # Observed points
        geom_point(
          data = obs_sarec_bm %>%
            group_by(Treatment, DOY) %>%
            summarise(obs_biomass = mean(obs_biomass, na.rm = TRUE),
                      .groups = "drop"),
          aes(DOY, obs_biomass, color = Treatment, shape = "Observed"),
          size = 3
        ) +
        scale_color_manual(values = c(Irrigated = "#2196F3", Rainfed = "#FF7043")) +
        scale_linetype_manual(values = c(Simulated = "solid"), name = "") +
        scale_shape_manual(values = c(Observed = 16), name = "") +
        labs(title = "In-Season Total Biomass: Simulated vs Observed",
             subtitle = "SAREC 2024 | Lines = simulated daily; Points = observed sampling dates",
             x = "Day of Year (DOY)", y = "Aboveground Biomass (kg/ha)",
             color = "Treatment") +
        theme_bw(base_size = 11) +
        theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
      ggsave(file.path(PLOT_DIR, "12_biomass_total_inseason.png"), p12,
             width = 10, height = 7, dpi = 200)
      cat("Plot 12 saved\n")
    } else {
      cat("Plot 12 skipped (insufficient matched biomass data)\n")
    }

    ## Plot 13: Stacked bar — biomass partitioning at 3 sampling dates
    target_doys <- c(191, 235, 282)
    doy_tol     <- 15   # +-15 days

    part_obs <- obs_sarec_bm %>%
      filter(sapply(DOY, function(d) any(abs(d - target_doys) <= doy_tol))) %>%
      mutate(
        doy_group = sapply(DOY, function(d) target_doys[which.min(abs(d - target_doys))]),
        other_obs = pmax(0, obs_biomass - coalesce(obs_seed, 0) - coalesce(obs_stem, 0))
      ) %>%
      group_by(doy_group, Treatment) %>%
      summarise(
        Seed  = mean(obs_seed,  na.rm = TRUE),
        Stem  = mean(obs_stem,  na.rm = TRUE),
        Other = mean(other_obs, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(cols = c(Seed, Stem, Other),
                   names_to = "Component", values_to = "Biomass") %>%
      mutate(Source = "Observed", doy_label = paste0("DOY ~", doy_group))

    if (nrow(daily_all) > 0) {
      part_sim <- sim_sarec_daily %>%
        filter(sapply(doy, function(d) any(abs(d - target_doys) <= doy_tol))) %>%
        mutate(
          doy_group = sapply(doy, function(d) target_doys[which.min(abs(d - target_doys))]),
          Other_sim = pmax(0, sim_biomass_ha - sim_seed_ha - sim_stem_ha)
        ) %>%
        group_by(doy_group, Treatment) %>%
        summarise(
          Seed  = mean(sim_seed_ha,  na.rm = TRUE),
          Stem  = mean(sim_stem_ha,  na.rm = TRUE),
          Other = mean(Other_sim,    na.rm = TRUE),
          .groups = "drop"
        ) %>%
        pivot_longer(cols = c(Seed, Stem, Other),
                     names_to = "Component", values_to = "Biomass") %>%
        mutate(Source = "Simulated", doy_label = paste0("DOY ~", doy_group))

      part_all <- bind_rows(part_obs, part_sim) %>%
        filter(!is.na(Biomass), Biomass >= 0) %>%
        mutate(
          Component = factor(Component, levels = c("Other", "Stem", "Seed")),
          Source    = factor(Source, levels = c("Observed", "Simulated")),
          doy_label = factor(doy_label)
        )

      if (nrow(part_all) > 0) {
        p13 <- ggplot(part_all,
                      aes(x = Source, y = Biomass, fill = Component)) +
          geom_col(position = "stack", width = 0.65, alpha = 0.85) +
          facet_grid(Treatment ~ doy_label) +
          scale_fill_manual(values = c(Seed = "#F39C12", Stem = "#27AE60", Other = "#7F8C8D")) +
          labs(title = "Biomass Partitioning at 3 Sampling Dates — SAREC 2024",
               subtitle = "Stacked bars: Seed + Stem + Other (leaf/reproductive) | Avg across genotypes",
               x = "", y = "Biomass (kg/ha)", fill = "Component") +
          theme_bw(base_size = 10) +
          theme(legend.position = "right", plot.title = element_text(face = "bold"),
                strip.background = element_rect(fill = "#ECF0F1"))
        ggsave(file.path(PLOT_DIR, "13_biomass_partitioning.png"), p13,
               width = 12, height = 8, dpi = 200)
        cat("Plot 13 saved\n")
      } else {
        cat("Plot 13 skipped (no data near target DOYs)\n")
      }
    }

    ## Plot 14: HI progression over time (obs vs sim)
    hi_obs <- obs_sarec_bm %>%
      filter(!is.na(obs_HI_inseason), obs_HI_inseason > 0, obs_HI_inseason <= 1) %>%
      group_by(Treatment, DOY) %>%
      summarise(HI = mean(obs_HI_inseason, na.rm = TRUE), .groups = "drop") %>%
      mutate(Source = "Observed")

    hi_sim <- sim_sarec_daily %>%
      filter(!is.na(sim_HI_daily), sim_HI_daily > 0, sim_HI_daily <= 1) %>%
      group_by(Treatment, doy) %>%
      summarise(HI = mean(sim_HI_daily, na.rm = TRUE), .groups = "drop") %>%
      rename(DOY = doy) %>%
      mutate(Source = "Simulated")

    hi_all <- bind_rows(hi_obs, hi_sim)

    if (nrow(hi_all) > 0) {
      p14 <- ggplot(hi_all, aes(DOY, HI, color = Treatment, linetype = Source)) +
        geom_line(data = hi_all %>% filter(Source == "Simulated"),
                  linewidth = 1, alpha = 0.9) +
        geom_point(data = hi_all %>% filter(Source == "Observed"),
                   size = 3, alpha = 0.85) +
        scale_color_manual(values = c(Irrigated = "#2196F3", Rainfed = "#FF7043")) +
        scale_linetype_manual(values = c(Simulated = "solid", Observed = "blank"),
                              guide = "none") +
        labs(title = "Harvest Index Progression — SAREC 2024",
             subtitle = "Lines = simulated daily HI (WGRN/WTOP); Points = observed HI at sampling dates",
             x = "Day of Year (DOY)", y = "Harvest Index (HI)",
             color = "Treatment") +
        theme_bw(base_size = 11) +
        theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
      ggsave(file.path(PLOT_DIR, "14_HI_progression.png"), p14,
             width = 10, height = 7, dpi = 200)
      cat("Plot 14 saved\n")
    } else {
      cat("Plot 14 skipped (insufficient HI data)\n")
    }
  }
} else {
  cat("Plots 12-14 skipped (daily outputs or observed in-season data not available)\n")
  cat("  Run run_model_testing.R with SAVE_DAILY=TRUE first.\n")
}

# ============================================================
# 6. CALIBRATION RECOMMENDATIONS
# ============================================================
cat("\n\n=== CALIBRATION RECOMMENDATIONS ===\n")
cat("Based on initial model run (all genotypes with MG4 parameters)\n\n")

irr_obs_mean <- mean(df$obs_yield[df$Treatment == "Irrigated"],  na.rm = TRUE)
irr_sim_mean <- mean(df$sim_yield[df$Treatment == "Irrigated"],  na.rm = TRUE)
rfd_obs_mean <- mean(df$obs_yield[df$Treatment == "Rainfed"],    na.rm = TRUE)
rfd_sim_mean <- mean(df$sim_yield[df$Treatment == "Rainfed"],    na.rm = TRUE)
irue_scale   <- irr_obs_mean / irr_sim_mean

cat(sprintf("IRRIGATED: obs mean=%.0f | sim mean=%.0f | ratio=%.2f\n",
            irr_obs_mean, irr_sim_mean, irr_obs_mean / irr_sim_mean))
cat(sprintf("RAINFED:   obs mean=%.0f | sim mean=%.0f | ratio=%.2f\n",
            rfd_obs_mean, rfd_sim_mean, rfd_obs_mean / rfd_sim_mean))
cat(sprintf("\n-> Recommended IRUE adjustment: %.2f x current value\n", irue_scale))
cat(sprintf("   i.e., IRUE = 2.0 x %.2f = %.2f g/MJ PAR\n", irue_scale, 2.0 * irue_scale))

# Phenology summary (if available)
if (nrow(pheno_comp) > 0) {
  obs_R1_range <- range(pheno_comp$obs_R1_DOY, na.rm = TRUE)
  obs_R8_range <- range(pheno_comp$obs_R8_DOY, na.rm = TRUE)
  sim_R1_val   <- unique(na.omit(pheno_comp$sim_R1_DOY))[1]
  sim_R8_val   <- unique(na.omit(pheno_comp$sim_R8_DOY))[1]

  cat("\n=== PHENOLOGY ANALYSIS ===\n")
  cat(sprintf("Observed R1 DOY range: %.0f - %.0f (sim: %.0f)\n",
              obs_R1_range[1], obs_R1_range[2], sim_R1_val))
  cat(sprintf("Observed R8 DOY range: %.0f - %.0f (sim: %.0f)\n",
              obs_R8_range[1], obs_R8_range[2], sim_R8_val))
}

# Per-genotype calibration table
geno_yield <- df %>%
  filter(Location == "sarec") %>%
  group_by(Genotype) %>%
  summarise(
    obs_irr  = mean(obs_yield[Treatment == "Irrigated"],  na.rm = TRUE),
    obs_rfd  = mean(obs_yield[Treatment == "Rainfed"],    na.rm = TRUE),
    sim_irr  = mean(sim_yield[Treatment == "Irrigated"],  na.rm = TRUE),
    sim_rfd  = mean(sim_yield[Treatment == "Rainfed"],    na.rm = TRUE),
    obs_HI_i = mean(obs_HI[Treatment == "Irrigated"],     na.rm = TRUE),
    obs_HI_r = mean(obs_HI[Treatment == "Rainfed"],       na.rm = TRUE),
    sim_HI   = mean(sim_HI,                               na.rm = TRUE),
    .groups  = "drop"
  )

if (nrow(pheno_comp) > 0) {
  geno_yield <- geno_yield %>%
    left_join(
      pheno_comp %>%
        group_by(Genotype) %>%
        summarise(obs_R1 = mean(obs_R1_DOY, na.rm = TRUE),
                  obs_R8 = mean(obs_R8_DOY, na.rm = TRUE),
                  .groups = "drop"),
      by = "Genotype"
    ) %>%
    mutate(
      irue_needed  = round(2.0 * obs_irr / sim_irr, 2),
      bdR1_adj     = round((obs_R1 - sim_R1_val) * 0.7, 1),
      bdR8_adj     = round((obs_R8 - sim_R8_val) * 0.7, 1),
      HI_gap       = round(obs_HI_i - sim_HI, 3),
      pdhi_note    = ifelse(HI_gap > 0.02, "up PDHI",
                     ifelse(HI_gap < -0.02, "down PDHI", "OK")),
      obs_dr_ratio = obs_rfd / obs_irr,
      sim_dr_ratio = sim_rfd / sim_irr,
      drought_sens = round(obs_dr_ratio - sim_dr_ratio, 3),
      drought_note = ifelse(drought_sens < -0.10,
                            "Too drought sensitive -> up WSSG/WSSD",
                     ifelse(drought_sens > 0.10,
                            "Not sensitive enough -> down WSSG/WSSD",
                            "Drought sensitivity OK"))
    )

  cat("\n=== PER-GENOTYPE CALIBRATION PLAN ===\n\n")
  for (i in seq_len(nrow(geno_yield))) {
    g <- geno_yield[i, ]
    cat(sprintf("--- %s ---\n", g$Genotype))
    cat(sprintf("  Yield (obs)  irr/rfd: %5.0f / %5.0f kg/ha\n", g$obs_irr, g$obs_rfd))
    cat(sprintf("  Yield (sim)  irr/rfd: %5.0f / %5.0f kg/ha\n", g$sim_irr, g$sim_rfd))
    cat(sprintf("  HI obs irr=%.3f | sim=%.3f | gap=%+.3f -> %s\n",
                g$obs_HI_i, g$sim_HI, g$HI_gap, g$pdhi_note))
    if (!is.na(g$obs_R1)) {
      cat(sprintf("  Obs R1/R8 DOY: %d / %d | Sim: %d / %d\n",
                  round(g$obs_R1), round(g$obs_R8), sim_R1_val, sim_R8_val))
    }
    cat(sprintf("  -> Suggested IRUE = %.2f g/MJ PAR\n", g$irue_needed))
    if (!is.na(g$bdR1_adj)) {
      cat(sprintf("  -> bdEMRR1 %+.1f BD (flowering timing)\n", g$bdR1_adj))
      cat(sprintf("  -> bdR5R7+bdR7R8 %+.1f BD (maturity timing)\n", g$bdR8_adj))
    }
    cat(sprintf("  -> Drought: %s\n\n", g$drought_note))
  }
} else {
  geno_yield <- geno_yield %>%
    mutate(irue_needed = round(2.0 * obs_irr / sim_irr, 2))
}

calib_file <- file.path(OUT_DIR, "calibration_recommendations.csv")
write.csv(geno_yield, calib_file, row.names = FALSE)
cat("Calibration table saved to:", calib_file, "\n")

# ============================================================
# 7. FINAL CONCLUSIONS
# ============================================================
cat("\n\n=== CONCLUSIONS ===\n\n")
irri_stats <- stats %>% filter(Variable == "Yield (kg/ha) - Irrigated")
rfed_stats <- stats %>% filter(Variable == "Yield (kg/ha) - Rainfed")

if (nrow(rfed_stats) > 0 && !is.na(rfed_stats$RMSE)) {
  cat(sprintf("RAINFED:  RMSE = %.0f kg/ha (RRMSE = %.1f%%), Bias = %+.0f kg/ha\n",
              rfed_stats$RMSE, rfed_stats$RRMSE_pct, rfed_stats$Bias))
}
if (nrow(irri_stats) > 0 && !is.na(irri_stats$RMSE)) {
  cat(sprintf("IRRIGATED: RMSE = %.0f kg/ha (RRMSE = %.1f%%), Bias = %+.0f kg/ha\n",
              irri_stats$RMSE, irri_stats$RRMSE_pct, irri_stats$Bias))
  cat(sprintf("  Obs mean = %.0f kg/ha | Sim mean = %.0f kg/ha (%.0f%% excess)\n",
              irr_obs_mean, irr_sim_mean, (irr_sim_mean / irr_obs_mean - 1) * 100))
  cat(sprintf("  PRIMARY CALIBRATION: Reduce IRUE from 2.0 to ~%.2f g/MJ PAR\n\n",
              2.0 * irue_scale))
}

cat("NEXT STEPS (Priority Order):\n")
cat("  1. Reduce IRUE globally to match irrigated potential (~1.0-1.3 g/MJ PAR)\n")
cat("  2. Calibrate bdEMRR1 per genotype (+/-2-4 BD) using observed R1 DOY\n")
cat("  3. Calibrate bdR5R7 and bdR7R8 per genotype using observed R8 DOY\n")
cat("  4. Fine-tune PDHI per genotype if HI is still off after yield calibration\n")
cat("  5. Adjust WSSG/WSSD for genotypes with poor rainfed performance fit\n")
cat("  6. Re-run and re-evaluate after each step\n\n")

cat("=== ANALYSIS COMPLETE ===\n")
cat("Plots     :", PLOT_DIR, "\n")
cat("Stats     :", stats_file, "\n")
cat("Calib     :", calib_file, "\n")
n_plots_saved <- length(list.files(PLOT_DIR, "\\.png$"))
cat(sprintf("Total plots saved: %d\n", n_plots_saved))
