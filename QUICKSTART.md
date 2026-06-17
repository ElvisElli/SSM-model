# SSM Soybean Model — Quick Start Guide

A process-based daily crop simulation model for soybean, based on  
Soltani & Sinclair (2012) *Modeling Physiology of Crop Development, Growth and Yield*.

---

## 1. Prerequisites

- **R ≥ 4.0** — download from <https://cran.r-project.org>
- **RStudio** (optional but recommended) — download from <https://posit.co>

Open a terminal (or the R console) in the project root and run the setup script **once**:

```r
Rscript r-model/code/00_install_packages.R
```

This installs all required CRAN packages automatically. The core model (`08_run_model.R`) also auto-installs its own dependencies on first run.

---

## 2. Run Your First Simulation

### From the command line

```bash
# Serial mode (exact reference match — recommended for validation)
Rscript r-model/code/08_run_model.R

# Parallel mode — auto-detects physical cores (fast, for exploratory work)
Rscript r-model/code/08_run_model.R --parallel

# Parallel mode — specify core count
Rscript r-model/code/08_run_model.R --parallel --cores 16
```

### From R / RStudio

Open `SSM-soybean.Rproj` in RStudio, then:

```r
source("r-model/code/08_run_model.R")

# Serial run (exact VBA match)
results <- run_all_scenarios()

# Parallel run (each scenario independent, FTSWRZ starts at 0)
results <- run_all_scenarios(parallel = TRUE)
results <- run_all_scenarios(parallel = TRUE, n_cores = 8)
```

### Output

Results are written to `r-model/outputs/results/`:

| File | Contents |
|------|----------|
| `all_results.csv` | All scenarios combined |
| `<Location>_results.csv` | One file per location |

Each row is one simulated year. Key columns:

| Column | Description |
|--------|-------------|
| `WGRN` | Grain dry mass (g m⁻²) |
| `WTOP` | Total above-ground dry mass (g m⁻²) |
| `HI` | Harvest index |
| `MXLAI` | Maximum leaf area index |
| `CE` | Cumulative soil evaporation (mm) |
| `CTR` | Cumulative transpiration (mm) |
| `CRAIN` | Cumulative rainfall (mm) |
| `CIRGW` | Cumulative irrigation water applied (mm) |
| `IRGNO` | Number of irrigation events |
| `dtR1…R8` | Days after planting to each phenological stage |

---

## 3. Changing Weather

Weather files are Excel workbooks in `r-model/inputs/weather/`.  
Each file covers one location and contains daily data with columns:

| Column | Unit | Description |
|--------|------|-------------|
| `YEAR` | — | Calendar year |
| `DOY` | 1–365 | Day of year |
| `SRAD` | MJ m⁻² d⁻¹ | Solar radiation |
| `TMAX` | °C | Maximum temperature |
| `TMIN` | °C | Minimum temperature |
| `RAIN` | mm | Precipitation |

**To add a new location:**

1. Create a new Excel file following the same column structure.
2. Place it in `r-model/inputs/weather/`.
3. Add rows to `r-model/inputs/scenarios.csv` pointing to the new file:
   - Set `wth_file` to the filename (e.g., `NewSite.xlsx`).
   - Set `loc_name`, `lat`, and other location-specific fields.

**To apply a climate-change scenario** without editing the weather file, use the `tchng` and `pchng` columns in `scenarios.csv`:

- `tchng` — temperature offset (°C), e.g. `2` adds 2°C to every day
- `pchng` — rainfall multiplier, e.g. `0.9` reduces rainfall by 10%

---

## 4. Changing Management

Edit `r-model/inputs/scenarios.csv`. Each row is one simulation scenario.  
Key management columns:

| Column | Description | Example values |
|--------|-------------|---------------|
| `pdoy` | Planting day of year | `120` (Apr 30), `150` (May 30) |
| `sim_doy` | Simulation start DOY (≤ pdoy) | Same as `pdoy` |
| `pden` | Plant density (plants m⁻²) | `30`, `40` |
| `water` | Water management: 0=rain-fed, 1=auto-irr, 2=SCS-CN, 3=mixed | `0`, `1` |
| `irglvl` | Irrigation trigger (FTSWRZ threshold, 0–1) | `0.60` |
| `tchng` | Temperature change (°C) | `0`, `2`, `4` |
| `pchng` | Rainfall multiplier | `1.0`, `0.9` |
| `co2` | Ambient CO₂ (ppm) | `420`, `550` |

**To add a new scenario**, copy an existing row and edit the columns above.  
Give it a unique name in the `scenario` column.

---

## 5. Changing Crop Cultivar

Crop cultivar parameters are stored as columns in `scenarios.csv`  
(one value per scenario row, same cultivar applied to all years).

Key cultivar parameters:

| Column | Description |
|--------|-------------|
| `PHYL` | Phyllochron (°C·d per leaf) |
| `PLACON`, `PLAPOW` | Leaf area coefficients |
| `PDHI` | Potential daily harvest index increment |
| `IRUE` | Intrinsic radiation-use efficiency (g DM MJ⁻¹ PAR) |
| `vpdtp` | VPD mode: 0 = daily, 1 = hourly (limited transpiration trait) |
| `VPDcr` | Critical VPD for LT trait (kPa) |
| `cpp`, `ppsen` | Photoperiod parameters |
| `bdR5R7` | Duration of seed fill in biological days |

The four standard cultivars in the current scenarios:

| Cultivar | `vpdtp` | `VPDcr` | Description |
|----------|---------|---------|-------------|
| check | 0 | — | No LT trait (unlimited transpiration) |
| LT1.5 | 1 | 1.5 kPa | Limited transpiration, low threshold |
| LT2 | 1 | 2.0 kPa | Limited transpiration, medium |
| LT2.5 | 1 | 2.5 kPa | Limited transpiration, high threshold |

---

## 6. Changing Soil

Soil profiles are stored in `r-model/inputs/soil_data.json` as named entries.  
Each entry defines 10 soil layers with volumetric water content parameters:

| Parameter | Description |
|-----------|-------------|
| `DUL` | Drained upper limit (field capacity), cm³ cm⁻³ |
| `LL` | Lower limit (permanent wilting point), cm³ cm⁻³ |
| `SAT` | Saturation water content, cm³ cm⁻³ |
| `DRAINF` | Drainage fraction per layer per day |
| `thickness` | Layer thickness (cm) |

**To add a new soil**, add a new JSON entry following the existing structure,  
then reference its key in the `soil_row` column of `scenarios.csv`.

---

## 7. Running Validation Plots

After running the model, compare results to the Excel reference:

```r
BASE_DIR <- "/path/to/r-model"   # set your path
source(file.path(BASE_DIR, "code", "09_validate_plots.R"))
```

Plots are saved to `r-model/outputs/plots/`. They include scatter plots  
(R vs Excel reference) with R² and RMSE for: WGRN, WTOP, MXLAI, CE, CTR,  
CIRGW, IRGNO, IPASW, and phenological stages.

---

## 8. Parallel Execution (HPC / Multi-Core)

### When to use parallel mode

- For large sensitivity analyses or new-scenario exploration
- When strict VBA numerical match is not required
- On machines with ≥ 4 cores

### How it works

- **Serial (default)**: FTSWRZ threads across all 240 scenarios in CSV order — exact VBA match.
- **Parallel**: each scenario runs independently with FTSWRZ = 0 at start. Faster, small numerical difference at season boundaries.

```bash
# Use all physical cores minus one (auto)
Rscript r-model/code/08_run_model.R --parallel

# Pin to 16 cores explicitly
Rscript r-model/code/08_run_model.R --parallel --cores 16
```

From R:

```r
# Auto-detect cores
results <- run_all_scenarios(parallel = TRUE)

# Specify cores
results <- run_all_scenarios(parallel = TRUE, n_cores = 16)
```

The model auto-detects the OS and uses `mclapply` (Linux/macOS) or  
`parLapply` with a PSOCK cluster (Windows).

---

## 9. Project Structure

```
SSM-model/
├── QUICKSTART.md              ← this file
├── SSM-soybean.Rproj          ← RStudio project (open this first)
├── r-model/
│   ├── code/
│   │   ├── 00_install_packages.R  ← run once to set up
│   │   ├── 01_read_inputs.R       ← weather, soil, scenario readers
│   │   ├── 02_phenology.R         ← biological day framework
│   │   ├── 03_crop_lai.R          ← leaf area index sub-model
│   │   ├── 04_dm_production.R     ← radiation use efficiency / DM production
│   │   ├── 05_dm_distribution.R   ← grain filling / DM partitioning
│   │   ├── 06_soil_water.R        ← 10-layer soil water balance
│   │   ├── 07_ssm_model.R         ← daily integration loop
│   │   ├── 08_run_model.R         ← batch runner (serial & parallel)
│   │   ├── 09_validate_plots.R    ← R vs Excel reference plots
│   │   └── 10_daily_plots.R       ← daily time-step overlay plots
│   ├── inputs/
│   │   ├── scenarios.csv          ← all scenario definitions
│   │   ├── soil_data.json         ← soil profiles
│   │   └── weather/               ← one .xlsx per location
│   └── outputs/
│       ├── results/               ← CSV outputs written here
│       └── plots/                 ← validation and daily plots
└── docs/
    └── SSM_Soybean_Documentation.html  ← full technical documentation
```

---

## 10. Troubleshooting

| Symptom | Likely cause | Fix |
|---------|-------------|-----|
| `Cannot locate r-model base directory` | Script not found via call stack | Set `BASE_DIR <- "/path/to/r-model"` before sourcing |
| `Weather file not found: ...` | `wth_file` column in scenarios.csv doesn't match actual filename | Check spelling and file location in `inputs/weather/` |
| `No soil data for soil_row=...` | `soil_row` key not in `soil_data.json` | Add entry to JSON or fix `soil_row` in scenarios.csv |
| Package not found error | Packages not installed | Run `Rscript r-model/code/00_install_packages.R` |
| Parallel mode hangs on Windows | PSOCK cluster issue | Try fewer cores or use serial mode |
| Results differ between serial and parallel | FTSWRZ carry-over absent in parallel | Expected; use serial for exact VBA match |
