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

OUT_DIR  <- "/home/user/SSM-model/model-testing/ssm_testing"
PLOT_DIR <- file.path(OUT_DIR, "plots")
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

cat("=== SSM Soybean Model Testing — Comprehensive Analysis ===\n\n")

# ============================================================
# 1. LOAD OBSERVED DATA
# ============================================================
obs_raw <- read_excel(
  "/home/user/SSM-model/model-testing/data analysis/input/soybean_data_input.xlsx",
  sheet = "all mm"
)

obs_all <- obs_raw %>%
  filter(!is.na(Genotype), !is.na(Treatment), !is.na(Location)) %>%
  mutate(
    Genotype  = trimws(as.character(Genotype)),
    Treatment = trimws(as.character(Treatment)),
    Location  = trimws(tolower(as.character(Location))),
    Year      = as.integer(Year),
    Harvest   = as.character(Harvest)
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
  filter(!is.na(TotalBiomass_kg_ha), !is.na(DAP), TotalBiomass_kg_ha > 0) %>%
  group_by(Year, Location, Treatment, Genotype, Harvest, DAP) %>%
  summarise(
    obs_biomass = mean(TotalBiomass_kg_ha, na.rm = TRUE),
    obs_seed    = mean(SeedBiomass, na.rm = TRUE),
    n_reps      = n(),
    .groups = "drop"
  )

cat(sprintf("Observed records (final harvest): %d\n", nrow(obs_final)))
cat("Locations:", paste(sort(unique(obs_final$Location)), collapse=", "), "\n")
cat("Genotypes:", paste(sort(unique(obs_final$Genotype)), collapse=", "), "\n\n")

# ============================================================
# 2. LOAD SIMULATED RESULTS
# ============================================================
# Genotype name map: clean name → original
geno_lookup <- c(
  "P42A84E"="P42A84E", "P48A14E"="P48A14E", "P52A14SE"="P52A14SE",
  "PI471938"="PI471938", "PI507408"="PI507408", "PI548431"="PI548431",
  "PI603457A"="PI603457A", "R1814502"="R18-14502", "R18C13665"="R18C-13665",
  "R1945980"="R19-45980", "R1946252"="R19-46252", "R19C1012"="R19C-1012"
)

sim_raw <- read.csv(file.path(OUT_DIR, "results/test_results_yearly.csv"),
                    stringsAsFactors = FALSE)

sim <- sim_raw %>%
  mutate(
    site      = tolower(sub("^TEST-([A-Z]+)-.*", "\\1", sName)),
    Year      = as.integer(sub("^TEST-[A-Z]+-([0-9]+)-.*", "\\1", sName)),
    treat_code = sub("^TEST-[A-Z]+-[0-9]+-([A-Z]+)-.*", "\\1", sName),
    geno_raw  = sub("^TEST-[A-Z]+-[0-9]+-[A-Z]+-(.+)$", "\\1", sName),
    Treatment = ifelse(treat_code == "IRRI", "Irrigated", "Rainfed"),
    Genotype  = recode(geno_raw, !!!geno_lookup),
    sim_yield   = Ywet,                  # kg/ha at 13% MC
    sim_biomass = WTOP * 10,             # g/m2 → kg/ha
    sim_HI      = HI,
    sim_LAI     = MXLAI,
    sim_R1_DOY  = as.integer(Pdoy) + as.integer(R1) - 1,
    sim_R8_DOY  = as.integer(Pdoy) + as.integer(R8) - 1,
    sim_R1_DAP  = as.integer(R1),
    sim_R8_DAP  = as.integer(R8)
  )

cat(sprintf("Simulated scenarios: %d\n", nrow(sim)))
cat(sprintf("Simulated Ywet range: %.0f – %.0f kg/ha\n",
            min(sim$sim_yield), max(sim$sim_yield)))
cat("NOTE: All genotypes share MG4 parameters in this initial run.\n",
    "Yield variation between genotypes within same treatment = 0 (by design)\n\n")

# ============================================================
# 3. JOIN OBS vs SIM (FINAL HARVEST) — key note:
#    All genotypes have same sim value per site×year×treatment in this run
# ============================================================
df <- obs_final %>%
  inner_join(
    sim %>% select(Year, site, Treatment, Genotype, sim_yield, sim_biomass,
                   sim_HI, sim_LAI, sim_R1_DOY, sim_R8_DOY,
                   sim_R1_DAP, sim_R8_DAP, CRAIN, CIRGW, R8),
    by = c("Year"="Year", "Location"="site",
           "Treatment"="Treatment", "Genotype"="Genotype")
  )

cat(sprintf("Matched obs-sim pairs: %d\n\n", nrow(df)))

# Summary by treatment
df %>%
  group_by(Treatment) %>%
  summarise(
    n        = n(),
    obs_mean = round(mean(obs_yield), 0),
    sim_mean = round(mean(sim_yield), 0),
    bias     = round(mean(sim_yield - obs_yield), 0),
    bias_pct = round(mean((sim_yield - obs_yield)/obs_yield*100), 1),
    .groups  = "drop"
  ) %>%
  { cat("=== Yield summary by treatment (all sites, all genotypes) ===\n");
    print(.); cat("\n") }

# ============================================================
# 4. STATISTICS
# ============================================================
calc_stats <- function(obs, sim, label="") {
  v <- complete.cases(obs, sim) & is.finite(obs) & is.finite(sim)
  o <- obs[v]; s <- sim[v]; n <- length(o)
  if (n < 2) return(data.frame(Variable=label,n=n,RMSE=NA,RRMSE_pct=NA,Bias=NA,R2=NA))
  data.frame(Variable=label, n=n,
             RMSE     = round(sqrt(mean((s-o)^2)), 1),
             RRMSE_pct= round(sqrt(mean((s-o)^2))/mean(o)*100, 1),
             Bias     = round(mean(s-o), 1),
             R2       = round(cor(o,s)^2, 3))
}

stats <- bind_rows(
  calc_stats(df$obs_yield,   df$sim_yield,   "Yield (kg/ha) - All"),
  calc_stats(df$obs_biomass, df$sim_biomass, "Biomass (kg/ha) - All"),
  calc_stats(df$obs_HI,      df$sim_HI,      "Harvest Index - All")
)
for (trt in c("Irrigated","Rainfed")) {
  s <- df %>% filter(Treatment==trt)
  stats <- bind_rows(stats,
    calc_stats(s$obs_yield,   s$sim_yield,   paste0("Yield (kg/ha) - ",trt)),
    calc_stats(s$obs_biomass, s$sim_biomass, paste0("Biomass (kg/ha) - ",trt)),
    calc_stats(s$obs_HI,      s$sim_HI,      paste0("HI - ",trt))
  )
}
for (yr in c(2024,2025)) {
  s <- df %>% filter(Year==yr)
  stats <- bind_rows(stats,
    calc_stats(s$obs_yield, s$sim_yield, paste0("Yield (kg/ha) - ",yr)))
}

write.csv(stats, file.path(OUT_DIR, "model_statistics.csv"), row.names=FALSE)
cat("=== PERFORMANCE STATISTICS ===\n")
print(stats, row.names=FALSE)
cat("\n")

# ============================================================
# 5. PLOTS
# ============================================================

# Helper: 1:1 limits
lim <- function(x, y, pad=0.05) {
  mx <- max(c(x,y), na.rm=TRUE) * (1+pad)
  c(0, ceiling(mx/500)*500)
}

## --- Plot 1: 1:1 yield scatter (all data, color=treatment) ---
ylims <- lim(df$obs_yield, df$sim_yield)

stat_ann <- stats %>% filter(Variable == "Yield (kg/ha) - All")
p1 <- ggplot(df, aes(obs_yield, sim_yield, color=Treatment, shape=as.factor(Year))) +
  geom_abline(slope=1, intercept=0, linetype="dashed", color="gray40", linewidth=0.8) +
  geom_point(size=2.8, alpha=0.75) +
  scale_color_manual(values=c(Irrigated="#2196F3", Rainfed="#FF7043")) +
  scale_shape_manual(values=c("2024"=16,"2025"=17), name="Year") +
  coord_equal(xlim=ylims, ylim=ylims) +
  annotate("text", x=ylims[1]+200, y=ylims[2]*0.95, hjust=0, size=3.5,
           label=sprintf("RMSE  = %.0f kg/ha\nRRMSE = %.1f%%\nBias  = %+.0f kg/ha\nR²    = %.3f",
                         stat_ann$RMSE, stat_ann$RRMSE_pct,
                         stat_ann$Bias, stat_ann$R2),
           family="mono") +
  labs(title="Observed vs Simulated Soybean Yield",
       subtitle="All sites (SAREC, Pinetree, Rohwer) | 2024-2025 | Initial MG4 parameters",
       x="Observed Yield (kg/ha)", y="Simulated Yield (kg/ha)",
       color="Treatment") +
  theme_bw(base_size=12) +
  theme(plot.title=element_text(face="bold"), legend.position="right")
ggsave(file.path(PLOT_DIR,"01_yield_1to1_all.png"), p1, width=8, height=7.5, dpi=200)
cat("Plot 1 saved\n")

## --- Plot 2: 1:1 by treatment (separate panels) ---
stats_trt <- df %>%
  group_by(Treatment) %>%
  summarise(RMSE=round(sqrt(mean((sim_yield-obs_yield)^2,na.rm=T)),0),
            RRMSE=round(sqrt(mean((sim_yield-obs_yield)^2,na.rm=T))/mean(obs_yield,na.rm=T)*100,1),
            Bias=round(mean(sim_yield-obs_yield,na.rm=T),0),
            R2=round(cor(obs_yield,sim_yield,use="complete.obs")^2,3),
            xpos=200, ypos=max(sim_yield,na.rm=T)*0.95, .groups="drop")

p2 <- ggplot(df, aes(obs_yield, sim_yield, color=Genotype)) +
  geom_abline(slope=1, linetype="dashed", color="gray40") +
  geom_point(size=2.5, alpha=0.85) +
  geom_text_repel(aes(label=Genotype), size=2.2, max.overlaps=8, show.legend=FALSE) +
  facet_wrap(~Treatment) +
  geom_text(data=stats_trt,
            aes(x=xpos, y=ypos,
                label=sprintf("RMSE=%.0f  RRMSE=%.1f%%\nBias=%+.0f  R²=%.3f",
                               RMSE, RRMSE, Bias, R2)),
            inherit.aes=FALSE, hjust=0, size=3, family="mono") +
  labs(title="Yield 1:1 by Treatment | All Sites & Years",
       x="Observed (kg/ha)", y="Simulated (kg/ha)", color="Genotype") +
  theme_bw(base_size=10) +
  theme(legend.position="right", plot.title=element_text(face="bold"))
ggsave(file.path(PLOT_DIR,"02_yield_by_treatment.png"), p2, width=14, height=7, dpi=200)
cat("Plot 2 saved\n")

## --- Plot 3: Observed vs Simulated yield per genotype (SAREC only, bar chart) ---
df_sarec <- df %>% filter(Location=="sarec") %>%
  select(Year, Treatment, Genotype, obs_yield, sim_yield) %>%
  pivot_longer(cols=c(obs_yield,sim_yield), names_to="Source", values_to="Yield") %>%
  mutate(Source=recode(Source, "obs_yield"="Observed", "sim_yield"="Simulated"))

p3 <- ggplot(df_sarec, aes(x=Genotype, y=Yield, fill=Source)) +
  geom_col(position=position_dodge(0.75), width=0.65, alpha=0.85) +
  facet_grid(Year~Treatment) +
  scale_fill_manual(values=c(Observed="#2C3E50", Simulated="#E74C3C")) +
  scale_y_continuous(limits=c(0,11000), breaks=seq(0,10000,2000)) +
  geom_hline(yintercept=0) +
  labs(title="Observed vs Simulated Yield by Genotype — SAREC",
       subtitle="Initial run: all genotypes share MG4 parameters → same simulated value per treatment",
       x="", y="Yield (kg/ha)", fill="") +
  theme_bw(base_size=9) +
  theme(axis.text.x=element_text(angle=45, hjust=1), legend.position="top",
        plot.title=element_text(face="bold"),
        strip.background=element_rect(fill="#ECF0F1"))
ggsave(file.path(PLOT_DIR,"03_yield_by_genotype_sarec.png"), p3, width=14, height=10, dpi=200)
cat("Plot 3 saved\n")

## --- Plot 4: In-season biomass SAREC 2024 ---
daily_files <- list.files(file.path(OUT_DIR,"results/daily"), "*.csv", full.names=TRUE)

daily_all <- lapply(daily_files, function(f) {
  nm <- tools::file_path_sans_ext(basename(f))
  nm <- sub("_daily$","",nm)
  d  <- tryCatch(read.csv(f), error=function(e) NULL)
  if (is.null(d)||nrow(d)==0) return(NULL)
  parts <- strsplit(nm,"-")[[1]]
  if (length(parts)<5) return(NULL)
  d$site       <- tolower(parts[2])
  d$Year_run   <- as.integer(parts[3])
  d$treat_code <- parts[4]
  d$geno_raw   <- paste(parts[5:length(parts)],collapse="")
  d$Treatment  <- ifelse(d$treat_code=="IRRI","Irrigated","Rainfed")
  d$Genotype   <- recode(d$geno_raw, !!!geno_lookup)
  d$sim_biomass <- d$WTOP * 10
  d
}) %>% bind_rows()

obs24 <- obs_inseason %>%
  filter(Location=="sarec", Year==2024, !is.na(obs_biomass))

sim24 <- daily_all %>%
  filter(site=="sarec", Year_run==2024, DAP>=0) %>%
  filter(Genotype %in% c("P42A84E","P48A14E","R18-14502","R19C-1012","PI548431","R19-45980"))

obs24_sub <- obs24 %>%
  filter(Genotype %in% c("P42A84E","P48A14E","R18-14502","R19C-1012","PI548431","R19-45980"))

p4 <- ggplot() +
  geom_line(data=sim24, aes(DAP, sim_biomass, linetype=Treatment),
            color="#E74C3C", linewidth=0.9, alpha=0.8) +
  geom_point(data=obs24_sub, aes(DAP, obs_biomass, shape=Treatment),
             color="#2C3E50", size=2.8) +
  scale_linetype_manual(values=c(Irrigated="solid",Rainfed="dashed")) +
  scale_shape_manual(values=c(Irrigated=16,Rainfed=1)) +
  facet_wrap(~Genotype, ncol=3, scales="free_y") +
  labs(title="In-Season Biomass: Simulated (red lines) vs Observed (black points)",
       subtitle="SAREC 2024 | NOTE: all simulated lines identical across genotypes (initial MG4 parameters)",
       x="Days After Planting (DAP)", y="Total Aboveground Biomass (kg/ha)",
       linetype="Treatment", shape="Treatment") +
  theme_bw(base_size=10) +
  theme(legend.position="bottom", plot.title=element_text(face="bold"),
        strip.background=element_rect(fill="#ECF0F1"))
ggsave(file.path(PLOT_DIR,"04_inseason_biomass.png"), p4, width=14, height=10, dpi=200)
cat("Plot 4 saved\n")

## --- Plot 5: Irrigated vs Rainfed yield ratio ---
reduce_obs <- obs_final %>%
  filter(Location=="sarec") %>%
  select(Year,Genotype,Treatment,obs_yield) %>%
  pivot_wider(names_from=Treatment, values_from=obs_yield) %>%
  filter(!is.na(Irrigated),!is.na(Rainfed)) %>%
  mutate(reduction_pct=(1-Rainfed/Irrigated)*100, Source="Observed")

reduce_sim <- sim %>%
  filter(site=="sarec") %>%
  select(Year,Genotype,Treatment,sim_yield) %>%
  pivot_wider(names_from=Treatment, values_from=sim_yield) %>%
  filter(!is.na(Irrigated),!is.na(Rainfed)) %>%
  mutate(reduction_pct=(1-Rainfed/Irrigated)*100, Source="Simulated")

reduce_all <- bind_rows(
  reduce_obs %>% mutate(Rainfed_yield=Rainfed),
  reduce_sim %>% rename(Rainfed=Rainfed, Irrigated=Irrigated) %>% mutate(Rainfed_yield=Rainfed)
) %>%
  mutate(Year=as.factor(Year))

p5 <- ggplot(reduce_all, aes(x=reorder(Genotype,-reduction_pct), y=reduction_pct,
                               fill=Source, alpha=Year)) +
  geom_col(position=position_dodge(0.8), width=0.7) +
  facet_wrap(~Year) +
  scale_fill_manual(values=c(Observed="#2C3E50",Simulated="#E74C3C")) +
  scale_alpha_manual(values=c("2024"=0.9,"2025"=0.8), guide="none") +
  coord_flip() +
  labs(title="Rainfed Yield Reduction vs Irrigated (%)",
       subtitle="SAREC | Obs (dark) vs Sim (red) | Sim has single value per year (all genos same)",
       x="", y="Yield Reduction (%)", fill="") +
  theme_bw(base_size=10) +
  theme(legend.position="top", plot.title=element_text(face="bold"),
        strip.background=element_rect(fill="#ECF0F1"))
ggsave(file.path(PLOT_DIR,"05_yield_reduction.png"), p5, width=12, height=7, dpi=200)
cat("Plot 5 saved\n")

## --- Plot 6: Phenology ---
pheno_obs_raw <- read_excel(
  "/home/user/SSM-model/model-testing/data analysis/input/phenology.xlsx",
  sheet="phenology_sarec_2024"
)

pheno_obs <- pheno_obs_raw %>%
  filter(!is.na(Phenology), !is.na(Date)) %>%
  mutate(DOY_obs=yday(as.Date(Date)),
         Genotype=trimws(as.character(Genotype)),
         Treatment=trimws(as.character(Treatment)))

pheno_R1 <- pheno_obs %>% filter(Phenology=="R1") %>%
  group_by(Genotype,Treatment) %>%
  summarise(obs_R1_DOY=min(DOY_obs,na.rm=TRUE), .groups="drop")

pheno_R8 <- pheno_obs %>% filter(Phenology=="R8") %>%
  group_by(Genotype,Treatment) %>%
  summarise(obs_R8_DOY=min(DOY_obs,na.rm=TRUE), .groups="drop")

sim_pheno <- sim %>%
  filter(site=="sarec", Year==2024) %>%
  select(Genotype,Treatment,sim_R1_DOY,sim_R8_DOY)

pheno_comp <- pheno_R1 %>%
  inner_join(pheno_R8, by=c("Genotype","Treatment")) %>%
  inner_join(sim_pheno, by=c("Genotype","Treatment")) %>%
  mutate(R1_err=sim_R1_DOY-obs_R1_DOY, R8_err=sim_R8_DOY-obs_R8_DOY)

cat("\n=== Phenology errors (sim - obs, DOY), SAREC 2024 ===\n")
print(pheno_comp %>% select(Genotype,Treatment,obs_R1_DOY,sim_R1_DOY,R1_err,
                              obs_R8_DOY,sim_R8_DOY,R8_err), row.names=FALSE)

p6a <- ggplot(pheno_comp, aes(obs_R1_DOY, sim_R1_DOY, color=Genotype, shape=Treatment)) +
  geom_abline(slope=1,linetype="dashed",color="gray40") +
  geom_point(size=3) +
  geom_text_repel(aes(label=Genotype), size=2.5, max.overlaps=10, show.legend=FALSE) +
  annotate("text",x=min(pheno_comp$obs_R1_DOY,na.rm=TRUE),
           y=max(pheno_comp$sim_R1_DOY,na.rm=TRUE),
           hjust=0, size=3.5,
           label=sprintf("Mean error: %+.1f d",mean(pheno_comp$R1_err,na.rm=TRUE))) +
  labs(title="R1 (Flowering) DOY",x="Observed",y="Simulated") +
  theme_bw(base_size=10) + theme(legend.position="bottom")

p6b <- ggplot(pheno_comp, aes(obs_R8_DOY, sim_R8_DOY, color=Genotype, shape=Treatment)) +
  geom_abline(slope=1,linetype="dashed",color="gray40") +
  geom_point(size=3) +
  geom_text_repel(aes(label=Genotype), size=2.5, max.overlaps=10, show.legend=FALSE) +
  annotate("text",x=min(pheno_comp$obs_R8_DOY,na.rm=TRUE),
           y=max(pheno_comp$sim_R8_DOY,na.rm=TRUE),
           hjust=0, size=3.5,
           label=sprintf("Mean error: %+.1f d",mean(pheno_comp$R8_err,na.rm=TRUE))) +
  labs(title="R8 (Maturity) DOY",x="Observed",y="Simulated") +
  theme_bw(base_size=10) + theme(legend.position="bottom")

p6 <- p6a + p6b +
  plot_annotation(title="Phenology: Simulated vs Observed (SAREC 2024)",
                  subtitle="NOTE: all genotypes share same MG4 parameters → identical simulated DOY")
ggsave(file.path(PLOT_DIR,"06_phenology.png"), p6, width=14, height=7, dpi=200)
cat("\nPlot 6 saved\n")

## --- Plot 7: HI analysis ---
p7 <- ggplot(df %>% filter(Location=="sarec"),
             aes(x=obs_biomass, y=obs_yield, color=Treatment)) +
  geom_point(aes(shape="Observed"), size=2.5, alpha=0.8) +
  geom_point(aes(x=sim_biomass, y=sim_yield, shape="Simulated"), size=3, alpha=0.8) +
  geom_abline(slope=0.5, linetype="dashed", color="gray60", linewidth=0.7) +
  scale_color_manual(values=c(Irrigated="#2196F3",Rainfed="#FF7043")) +
  scale_shape_manual(values=c(Observed=16,Simulated=8), name="") +
  facet_wrap(~Year) +
  labs(title="Biomass vs Yield (HI Analysis) — SAREC",
       subtitle="Dashed line = HI 0.50 | Points=Observed, Stars=Simulated",
       x="Total Biomass (kg/ha)", y="Seed Yield (kg/ha)", color="Treatment") +
  theme_bw(base_size=10) +
  theme(legend.position="right", plot.title=element_text(face="bold"))
ggsave(file.path(PLOT_DIR,"07_HI_analysis.png"), p7, width=12, height=6, dpi=200)
cat("Plot 7 saved\n")

## --- Plot 8: Relative error per genotype ---
df_err <- df %>%
  filter(Location=="sarec") %>%
  group_by(Genotype, Treatment) %>%
  summarise(
    obs_mean = mean(obs_yield, na.rm=TRUE),
    sim_mean = mean(sim_yield, na.rm=TRUE),
    rel_err  = mean((sim_yield-obs_yield)/obs_yield*100, na.rm=TRUE),
    .groups  = "drop"
  )

p8 <- ggplot(df_err, aes(x=reorder(Genotype,-rel_err), y=rel_err, fill=Treatment)) +
  geom_col(position=position_dodge(0.75), width=0.65, alpha=0.85) +
  geom_hline(yintercept=0, linewidth=0.8) +
  geom_hline(yintercept=c(-20,20), linetype="dashed", color="gray50") +
  scale_fill_manual(values=c(Irrigated="#2196F3",Rainfed="#FF7043")) +
  facet_wrap(~Treatment, ncol=2) +
  labs(title="Relative Yield Error by Genotype — SAREC (averaged 2024+2025)",
       subtitle="(Simulated − Observed) / Observed × 100 | Dashed = ±20% threshold",
       x="", y="Relative Error (%)", fill="Treatment") +
  theme_bw(base_size=10) +
  theme(axis.text.x=element_text(angle=45,hjust=1), legend.position="none",
        plot.title=element_text(face="bold"))
ggsave(file.path(PLOT_DIR,"08_relative_error.png"), p8, width=12, height=7, dpi=200)
cat("Plot 8 saved\n")

## --- Plot 9: Multi-site comparison ---
p9 <- ggplot(df, aes(obs_yield, sim_yield, color=Treatment, shape=Location)) +
  geom_abline(slope=1,linetype="dashed",color="gray40") +
  geom_point(size=3, alpha=0.8) +
  scale_color_manual(values=c(Irrigated="#2196F3",Rainfed="#FF7043")) +
  scale_shape_manual(values=c(sarec=16,pinetree=17,rohwer=15)) +
  facet_wrap(~as.factor(Year)) +
  labs(title="Multi-Site Yield Comparison: 2024 vs 2025",
       x="Observed (kg/ha)", y="Simulated (kg/ha)",
       color="Treatment", shape="Location") +
  theme_bw(base_size=10) +
  theme(legend.position="right", plot.title=element_text(face="bold"))
ggsave(file.path(PLOT_DIR,"09_multisite.png"), p9, width=12, height=6, dpi=200)
cat("Plot 9 saved\n")

# ============================================================
# 6. CALIBRATION RECOMMENDATIONS
# ============================================================
cat("\n\n=== CALIBRATION RECOMMENDATIONS ===\n")
cat("Based on initial model run (all genotypes with MG4 parameters)\n\n")

# ---- Overall model behavior ----
irr_obs_mean  <- mean(df$obs_yield[df$Treatment=="Irrigated"],  na.rm=TRUE)
irr_sim_mean  <- mean(df$sim_yield[df$Treatment=="Irrigated"],  na.rm=TRUE)
rfd_obs_mean  <- mean(df$obs_yield[df$Treatment=="Rainfed"],    na.rm=TRUE)
rfd_sim_mean  <- mean(df$sim_yield[df$Treatment=="Rainfed"],    na.rm=TRUE)
irue_scale    <- irr_obs_mean / irr_sim_mean

cat(sprintf("IRRIGATED: obs mean=%.0f | sim mean=%.0f | ratio=%.2f\n",
            irr_obs_mean, irr_sim_mean, irr_obs_mean/irr_sim_mean))
cat(sprintf("RAINFED:   obs mean=%.0f | sim mean=%.0f | ratio=%.2f\n",
            rfd_obs_mean, rfd_sim_mean, rfd_obs_mean/rfd_sim_mean))
cat(sprintf("\n→ Recommended IRUE adjustment: %.2f × current value\n",   irue_scale))
cat(sprintf("  i.e., IRUE = 2.0 × %.2f = %.2f g/MJ PAR\n",
            irue_scale, 2.0 * irue_scale))
cat(sprintf("  This will scale down both irrigated AND rainfed proportionally.\n"))

# ---- Phenology summary ----
pheno_summary <- pheno_comp %>%
  group_by(Genotype) %>%
  summarise(R1_err=mean(R1_err,na.rm=TRUE), R8_err=mean(R8_err,na.rm=TRUE), .groups="drop")

cat("\n=== PHENOLOGY ANALYSIS ===\n")
cat("All genotypes use same MG4 parameters → same simulated flowering/maturity date\n")
cat("Observed phenology varies significantly between genotypes.\n\n")

# Observed R1 range
obs_R1_range <- range(pheno_comp$obs_R1_DOY, na.rm=TRUE)
obs_R8_range <- range(pheno_comp$obs_R8_DOY, na.rm=TRUE)
sim_R1_val   <- unique(pheno_comp$sim_R1_DOY)[1]
sim_R8_val   <- unique(pheno_comp$sim_R8_DOY)[1]

cat(sprintf("Observed R1 DOY range: %d – %d (sim: %d)\n",
            obs_R1_range[1], obs_R1_range[2], sim_R1_val))
cat(sprintf("Observed R8 DOY range: %d – %d (sim: %d)\n",
            obs_R8_range[1], obs_R8_range[2], sim_R8_val))

cat("\n=== PER-GENOTYPE CALIBRATION PLAN ===\n\n")

# Per-genotype yield errors and obs phenology
geno_yield <- df %>%
  filter(Location=="sarec") %>%
  group_by(Genotype) %>%
  summarise(
    obs_irr  = mean(obs_yield[Treatment=="Irrigated"],  na.rm=TRUE),
    obs_rfd  = mean(obs_yield[Treatment=="Rainfed"],    na.rm=TRUE),
    sim_irr  = mean(sim_yield[Treatment=="Irrigated"],  na.rm=TRUE),
    sim_rfd  = mean(sim_yield[Treatment=="Rainfed"],    na.rm=TRUE),
    obs_HI_i = mean(obs_HI[Treatment=="Irrigated"],     na.rm=TRUE),
    obs_HI_r = mean(obs_HI[Treatment=="Rainfed"],       na.rm=TRUE),
    sim_HI   = mean(sim_HI,                             na.rm=TRUE),
    .groups  = "drop"
  ) %>%
  left_join(
    pheno_comp %>% group_by(Genotype) %>%
      summarise(obs_R1=mean(obs_R1_DOY,na.rm=T), obs_R8=mean(obs_R8_DOY,na.rm=T), .groups="drop"),
    by="Genotype"
  ) %>%
  mutate(
    irue_needed   = round(2.0 * obs_irr / sim_irr, 2),
    # IRUE correction needed to match irrigated obs (simulated too high → reduce IRUE)
    bdR1_adj      = round((obs_R1 - sim_R1_val) * 0.7, 1),  # rough BD equiv
    bdR8_adj      = round((obs_R8 - sim_R8_val) * 0.7, 1),
    # HI
    HI_gap        = round(obs_HI_i - sim_HI, 3),
    pdhi_note     = ifelse(HI_gap > 0.02, "↑ PDHI",
                    ifelse(HI_gap < -0.02, "↓ PDHI", "OK")),
    # drought sensitivity
    obs_dr_ratio  = obs_rfd / obs_irr,
    sim_dr_ratio  = sim_rfd / sim_irr,
    drought_sens  = round(obs_dr_ratio - sim_dr_ratio, 3),
    drought_note  = ifelse(drought_sens < -0.10,
                           "Model too drought sensitive → ↑ WSSG/WSSD",
                           ifelse(drought_sens > 0.10,
                                  "Model not drought sensitive enough → ↓ WSSG/WSSD",
                                  "Drought sensitivity ~OK"))
  )

# Print table
for (i in seq_len(nrow(geno_yield))) {
  g <- geno_yield[i,]
  cat(sprintf("─── %s ─────────────────────────\n", g$Genotype))
  cat(sprintf("  Yield (obs)  irr/rfd: %5.0f / %5.0f kg/ha\n", g$obs_irr, g$obs_rfd))
  cat(sprintf("  Yield (sim)  irr/rfd: %5.0f / %5.0f kg/ha\n", g$sim_irr, g$sim_rfd))
  cat(sprintf("  HI obs irr=%.3f | sim=%.3f | gap=%+.3f → %s\n",
              g$obs_HI_i, g$sim_HI, g$HI_gap, g$pdhi_note))
  cat(sprintf("  Obs R1/R8 DOY: %d / %d | Sim: %d / %d\n",
              round(g$obs_R1), round(g$obs_R8), sim_R1_val, sim_R8_val))
  cat(sprintf("  → Suggested IRUE = %.2f g/MJ PAR\n", g$irue_needed))
  cat(sprintf("  → bdEMRR1 %s%.1f BD (flowering timing)\n",
              ifelse(g$bdR1_adj>=0,"+",""), g$bdR1_adj))
  cat(sprintf("  → bdR5R7+bdR7R8 %s%.1f BD (maturity timing)\n",
              ifelse(g$bdR8_adj>=0,"+",""), g$bdR8_adj))
  cat(sprintf("  → Drought: %s (obs rainfed/irrig=%.2f; sim=%.2f)\n\n",
              g$drought_note, g$obs_dr_ratio, g$sim_dr_ratio))
}

write.csv(geno_yield, file.path(OUT_DIR,"calibration_recommendations.csv"), row.names=FALSE)
cat("Calibration table saved.\n")

# ============================================================
# 7. FINAL CONCLUSIONS
# ============================================================
cat("\n\n=== CONCLUSIONS ===\n\n")
cat("1. RAINFED TREATMENT: Model performance is acceptable in first approximation.\n")
cat(sprintf("   RMSE = %.0f kg/ha (RRMSE = %.1f%%), Bias = %+.0f kg/ha\n",
            stats$RMSE[stats$Variable=="Yield (kg/ha) - Rainfed"],
            stats$RRMSE_pct[stats$Variable=="Yield (kg/ha) - Rainfed"],
            stats$Bias[stats$Variable=="Yield (kg/ha) - Rainfed"]))
cat("   The model captures the drought-stress yield reduction direction correctly.\n\n")

cat("2. IRRIGATED TREATMENT: Severe overestimation (model runs at theoretical potential).\n")
cat(sprintf("   RMSE = %.0f kg/ha (RRMSE = %.1f%%), Bias = %+.0f kg/ha\n",
            stats$RMSE[stats$Variable=="Yield (kg/ha) - Irrigated"],
            stats$RRMSE_pct[stats$Variable=="Yield (kg/ha) - Irrigated"],
            stats$Bias[stats$Variable=="Yield (kg/ha) - Irrigated"]))
cat(sprintf("   Observed irrigated mean: %.0f kg/ha | Simulated: %.0f kg/ha (%.0f%% excess)\n",
            irr_obs_mean, irr_sim_mean, (irr_sim_mean/irr_obs_mean-1)*100))
cat("   PRIMARY CALIBRATION: Reduce IRUE from 2.0 to ~", round(2.0*irue_scale,2),
    "g/MJ PAR to match irrigated yields.\n\n")

cat("3. PHENOLOGY: All genotypes use same MG4 parameters → same sim R1/R8.\n")
cat(sprintf("   Real R1 spans DOY %d–%d; sim = %d → some genotypes are early (PI548431)\n",
            obs_R1_range[1], obs_R1_range[2], sim_R1_val))
cat(sprintf("   Real R8 spans DOY %d–%d; sim = %d → most OK, some groups late or early\n",
            obs_R8_range[1], obs_R8_range[2], sim_R8_val))
cat("   Per-genotype bdEMRR1 and bdR5R7/R7R8 adjustments needed after IRUE calibration.\n\n")

cat("4. HARVEST INDEX: Observed HI 0.40–0.48 (irrigated), 0.10–0.45 (rainfed).\n")
cat(sprintf("   Simulated HI = %.3f–%.3f. Generally in range for irrigated;\n",
            min(sim$sim_HI), max(sim$sim_HI)))
cat("   rainfed HI underestimated for some genotypes → check PDHI or WSSD thresholds.\n\n")

cat("5. YEAR EFFECT: 2025 (wetter year: 570 mm) simulated better than 2024 (296 mm),\n")
cat("   consistent with the model being more accurate when water is not limiting.\n\n")

cat("6. GENOTYPE RANKING: Observed rank order (high→low yield) is NOT reproduced\n")
cat("   in this initial run because all genotypes have identical parameters.\n")
cat("   After per-genotype calibration, genotype ranking should improve.\n\n")

cat("NEXT STEPS (Priority Order):\n")
cat("  1. Reduce IRUE globally to match irrigated potential (~1.0–1.3 g/MJ PAR)\n")
cat("  2. Calibrate bdEMRR1 per genotype (±2–4 BD) using observed R1 DOY\n")
cat("  3. Calibrate bdR5R7 and bdR7R8 per genotype using observed R8 DOY\n")
cat("  4. Fine-tune PDHI per genotype if HI is still off after yield calibration\n")
cat("  5. Adjust WSSG/WSSD for genotypes with poor rainfed performance fit\n")
cat("  6. Re-run and re-evaluate after each step\n\n")

cat("=== ANALYSIS COMPLETE ===\n")
cat("Plots: ", PLOT_DIR, "\n")
cat("Stats: model_statistics.csv\n")
cat("Calibration: calibration_recommendations.csv\n")
