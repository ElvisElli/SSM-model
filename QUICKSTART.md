# SSM Soybean Model — Quick Start Guide

A process-based daily crop simulation model for soybean, based on  
Soltani & Sinclair (2012) *Modeling Physiology of Crop Development, Growth and Yield*.

---

## 1. Prerequisites

- **R ≥ 4.0** — download from <https://cran.r-project.org>
- **RStudio** (optional but recommended) — download from <https://posit.co>

Run the setup script **once** to install all required packages:

```r
Rscript r-model/code/00_install_packages.R
```

The model also auto-installs its core dependencies on first run if any are missing.

---

## 2. Run Your First Simulation

### From the command line

```bash
# Run all scenarios, yearly outputs only
Rscript r-model/code/08_run_model.R

# Yearly + daily time-step outputs
Rscript r-model/code/08_run_model.R --daily

# Parallel mode — auto-detects physical cores
Rscript r-model/code/08_run_model.R --parallel

# Parallel + daily outputs, 16 cores
Rscript r-model/code/08_run_model.R --parallel --cores 16 --daily
```

### From R / RStudio

Open `SSM-soybean.Rproj` in RStudio, then:

```r
source("r-model/code/08_run_model.R")

results <- run_all_scenarios()                        # yearly only
results <- run_all_scenarios(save_daily = TRUE)       # yearly + daily
results <- run_all_scenarios(parallel = TRUE)         # parallel mode
results <- run_all_scenarios(parallel = TRUE,
                             n_cores  = 16,
                             save_daily = TRUE)       # parallel + daily
```

Each scenario (row in `scenarios.csv`) is an independent simulation. Serial
and parallel modes produce identical results.

---

## 3. Yearly Outputs

Yearly summary CSVs are written to `r-model/outputs/results/`:

| File | Contents |
|------|----------|
| `all_results.csv` | All scenarios combined |
| `<Location>_results.csv` | One file per location |

Each row is one simulated year. Key output columns:

| Column | Description |
|--------|-------------|
| `WGRN` | Grain dry mass (g m⁻²) |
| `WTOP` | Total above-ground dry mass (g m⁻²) |
| `HI` | Harvest index |
| `MXLAI` | Maximum leaf area index (m² m⁻²) |
| `dtR1…R8` | Days after planting to each phenological stage |
| `CE` | Cumulative soil evaporation (mm) |
| `CTR` | Cumulative crop transpiration (mm) |
| `CRAIN` | Cumulative rainfall (mm) |
| `CIRGW` | Cumulative irrigation applied (mm) |
| `IRGNO` | Number of irrigation events |
| `IPASW` | Initial plant-available soil water (mm) |
| `CDRAIN` | Cumulative drainage below root zone (mm) |
| `MATYP` | Maturity type (1=normal, 2=early-LAI death, 4=stopped, 5=flood kill) |

---

## 4. Daily Outputs

Enable with `save_daily = TRUE` (or `--daily` from the command line).  
One CSV per scenario is saved to `r-model/outputs/results/daily/`:

```
daily/<sName>_daily.csv
```

Each row is one simulated day. Key daily columns:

| Column | Description |
|--------|-------------|
| `sName`, `year`, `doy`, `DAP` | Scenario and date identifiers |
| `CBD` | Cumulative biological days (phenological clock) |
| `LAI` | Leaf area index (m² m⁻²) |
| `DDMP` | Daily dry matter production (g m⁻² d⁻¹) |
| `SGR` | Seed growth rate (g m⁻² d⁻¹) |
| `WTOP` | Above-ground dry mass (g m⁻²) |
| `WGRN` | Grain dry mass (g m⁻²) |
| `RAIN`, `IRGW` | Daily rainfall and irrigation (mm) |
| `SEVP`, `TR` | Soil evaporation and transpiration (mm d⁻¹) |
| `CE`, `CTR` | Cumulative evaporation and transpiration (mm) |
| `FTSWRZ` | Fraction of transpirable soil water in root zone (0–1) |
| `WSFL`, `WSFG`, `WSFD` | Water stress factors for leaf, growth, development |
| `ATSWRZ` | Available soil water in root zone (mm) |
| `DEPORT` | Rooting depth (cm) |

Example — read and plot daily LAI for one scenario:

```r
d <- read.csv("r-model/outputs/results/daily/JB-RFD-LTE-check_daily.csv")
plot(d$doy[d$year == 1991], d$LAI[d$year == 1991],
     type = "l", xlab = "Day of year", ylab = "LAI")
```

---

## 5. Changing Weather

Weather files are Excel workbooks in `r-model/inputs/weather/`.  
Each file covers one location with daily data:

| Column | Unit | Description |
|--------|------|-------------|
| `YEAR` | — | Calendar year |
| `DOY` | 1–365 | Day of year |
| `SRAD` | MJ m⁻² d⁻¹ | Solar radiation |
| `TMAX` | °C | Maximum temperature |
| `TMIN` | °C | Minimum temperature |
| `RAIN` | mm | Precipitation |

**To add a new location:**

1. Create an Excel file following the same column structure.
2. Place it in `r-model/inputs/weather/`.
3. Add rows to `scenarios.csv` pointing to it (`wth_file` column).

**Climate-change adjustments** without editing the weather file — set in `scenarios.csv`:

- `tchng` — temperature offset (°C), e.g. `2` adds 2°C to every day
- `pchng` — rainfall multiplier, e.g. `0.9` reduces all rainfall by 10%

---

## 6. Changing Management

Edit `r-model/inputs/scenarios.csv`. Each row is one simulation scenario.

| Column | Description | Example values |
|--------|-------------|----------------|
| `pdoy` | Planting day of year | `120` (Apr 30), `150` (May 30) |
| `sim_doy` | Simulation start DOY (≤ pdoy) | Same as `pdoy` |
| `pden` | Plant density (plants m⁻²) | `30`, `40` |
| `water` | Water mode: 0=rain-fed, 1=auto-irr, 2=SCS-CN | `0`, `1` |
| `irglvl` | Auto-irrigation trigger (FTSWRZ threshold) | `0.60` |
| `co2` | Ambient CO₂ (ppm) | `420`, `550` |
| `tchng` | Temperature change (°C) | `0`, `2`, `4` |
| `pchng` | Rainfall multiplier | `1.0`, `0.9` |

To add a new scenario, copy an existing row and edit the columns.  
Give it a unique name in the `scenario` column.

---

## 7. Changing Crop Cultivar

Cultivar parameters are columns in `scenarios.csv` (same value applied to all years of that row).

| Column | Description |
|--------|-------------|
| `PHYL` | Phyllochron (°C·d per leaf) |
| `PLACON`, `PLAPOW` | Leaf area power-function coefficients |
| `PDHI` | Potential daily harvest index increment |
| `IRUE` | Intrinsic radiation-use efficiency (g DM MJ⁻¹ PAR) |
| `vpdtp` | VPD mode: 0 = daily (no LT trait), 1 = hourly (LT trait active) |
| `VPDcr` | Critical VPD for limited-transpiration trait (kPa) |
| `cpp`, `ppsen` | Critical photoperiod and photoperiod sensitivity |
| `bdR5R7` | Seed-fill duration (biological days) |

Standard cultivars in the current scenarios:

| Cultivar | `vpdtp` | `VPDcr` | Description |
|----------|---------|---------|-------------|
| check | 0 | — | No LT trait |
| LT1.5 | 1 | 1.5 kPa | Limited transpiration, low threshold |
| LT2 | 1 | 2.0 kPa | Limited transpiration, medium |
| LT2.5 | 1 | 2.5 kPa | Limited transpiration, high threshold |

---

## 8. Changing Soil

Soil profiles are in `r-model/inputs/soil_data.json`.  
Each entry defines 10 layers with:

| Parameter | Description |
|-----------|-------------|
| `DUL` | Drained upper limit (field capacity), cm³ cm⁻³ |
| `LL` | Lower limit (permanent wilting point), cm³ cm⁻³ |
| `SAT` | Saturation water content, cm³ cm⁻³ |
| `DRAINF` | Drainage fraction per layer per day |
| `thickness` | Layer thickness (cm) |

To add a new soil, add an entry to the JSON and reference its key in `soil_row` in `scenarios.csv`.

---

## 9. Parallel Execution

Each scenario is an independent simulation — results are identical whether run in
serial or parallel mode.

```bash
# Auto-detect physical cores
Rscript r-model/code/08_run_model.R --parallel

# Pin to a specific number of cores
Rscript r-model/code/08_run_model.R --parallel --cores 16
```

```r
# From R
results <- run_all_scenarios(parallel = TRUE)
results <- run_all_scenarios(parallel = TRUE, n_cores = 16)
```

The model auto-detects the OS: uses `mclapply` on Linux/macOS (fork-based,
inherits environment), or `parLapply` with a PSOCK cluster on Windows.

---

## 10. Project Structure

```
SSM-model/
├── QUICKSTART.md              ← this file
├── SSM-soybean.Rproj          ← open this in RStudio
├── r-model/
│   ├── code/                  ← simulation model (shared with users)
│   │   ├── 00_install_packages.R  ← one-time setup
│   │   ├── 01_read_inputs.R       ← weather, soil, scenario readers
│   │   ├── 02_phenology.R         ← biological day framework
│   │   ├── 03_crop_lai.R          ← leaf area index sub-model
│   │   ├── 04_dm_production.R     ← radiation use efficiency / DM production
│   │   ├── 05_dm_distribution.R   ← grain filling / DM partitioning
│   │   ├── 06_soil_water.R        ← 10-layer soil water balance
│   │   ├── 07_ssm_model.R         ← daily integration loop
│   │   └── 08_run_model.R         ← batch runner (serial & parallel)
│   ├── inputs/
│   │   ├── scenarios.csv          ← all scenario definitions
│   │   ├── soil_data.json         ← soil profiles
│   │   └── weather/               ← one .xlsx per location
│   └── outputs/
│       └── results/               ← CSV outputs written here
│           └── daily/             ← daily CSVs (when save_daily=TRUE)
└── docs/
    └── SSM_Soybean_Documentation.html  ← full technical documentation
```

---

## 11. Troubleshooting

| Symptom | Likely cause | Fix |
|---------|-------------|-----|
| `Cannot locate r-model base directory` | Script not found via call stack | Set `BASE_DIR <- "/path/to/r-model"` before sourcing |
| `Weather file not found` | `wth_file` column doesn't match actual filename | Check spelling in `inputs/weather/` |
| `No soil data for soil_row=…` | Key not in `soil_data.json` | Add entry to JSON or fix `soil_row` in scenarios.csv |
| Package not found | Packages not installed | Run `Rscript r-model/code/00_install_packages.R` |
| Parallel hangs on Windows | PSOCK cluster issue | Try fewer cores or use serial mode |
