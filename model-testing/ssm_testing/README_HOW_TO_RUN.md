# SSM Soybean Model — Model Testing: How to Run

## Prerequisites

- **R >= 4.1** (tested on 4.3+)
- **RStudio** (recommended) or any R environment
- Required packages (install once):

```r
install.packages(c("readxl", "dplyr", "jsonlite",
                   "ggplot2", "patchwork", "ggrepel",
                   "tidyr", "lubridate"))
```

---

## Quick Start

### Step 1 — Open the project

Open `SSM-model.Rproj` in RStudio. This sets the working directory to the
repository root automatically.

Alternatively, set it manually:
```r
setwd("C:/path/to/SSM-model")   # Windows
setwd("/home/yourname/SSM-model") # Linux/Mac
```

### Step 2 — Run the model on all test scenarios

```r
source("model-testing/ssm_testing/run_model_testing.R")
```

This will:
- Load the 72 test scenarios from `inputs/test_scenarios.csv`
- Read weather files from `r-model/inputs/weather/`
- Read soil data from `r-model/inputs/soil_data.json`
- Run each scenario (serial by default; ~2-5 minutes total)
- Save yearly results to `outputs/results/test_results_yearly.csv`
- Save daily outputs to `outputs/results/daily/` (one CSV per scenario)

**Optional flags** (edit at the top of `run_model_testing.R`):
```r
SAVE_DAILY   <- TRUE   # Save per-day outputs (needed for biomass plots)
USE_PARALLEL <- TRUE   # Use multiple CPU cores (faster on local machine)
N_CORES      <- 4      # Number of cores (NULL = auto)
```

### Step 3 — Run the analysis and generate plots

```r
source("model-testing/ssm_testing/03_analyze_results.R")
```

This reads the results from Step 2 and generates all comparison plots,
statistics, and calibration recommendations.

---

## Folder Structure

```
SSM-model/
├── r-model/
│   ├── code/
│   │   ├── 01_read_inputs.R      # Input readers (weather, soil, scenarios)
│   │   ├── 02_phenology.R        # Thermal-time phenology
│   │   ├── 03_crop_lai.R         # Leaf area index
│   │   ├── 04_dm_production.R    # Dry matter production (RUE)
│   │   ├── 05_dm_distribution.R  # Assimilate partitioning
│   │   ├── 06_soil_water.R       # Soil water balance
│   │   ├── 07_ssm_model.R        # Top-level year integrator
│   │   └── 08_run_model.R        # Batch runner for main scenarios.csv
│   └── inputs/
│       ├── scenarios.csv         # Main production scenarios
│       ├── soil_data.json        # Soil profile data
│       └── weather/              # Weather Excel files per location
│
└── model-testing/
    ├── data analysis/
    │   └── input/
    │       ├── soybean_data_input.xlsx   # Observed field data
    │       └── phenology.xlsx            # Observed phenology stages
    └── ssm_testing/
        ├── run_model_testing.R   ← STEP 1: run this
        ├── 03_analyze_results.R  ← STEP 2: run this
        ├── inputs/
        │   ├── test_scenarios.csv       # 72 test scenarios (12 genotypes x sites x treatments)
        │   └── cultivar_parameters.csv  # Baseline MG4 parameters for all 12 genotypes
        └── outputs/              ← all generated outputs go here
            ├── model_statistics.csv
            ├── calibration_recommendations.csv
            ├── plots/            ← 14 PNG plots
            └── results/
                ├── test_results_yearly.csv
                └── daily/        ← one CSV per scenario (72 files)
```

---

## Output Files and Plots

### CSV outputs (in `outputs/`)

| File | Description |
|------|-------------|
| `model_statistics.csv` | RMSE, RRMSE, Bias, R² for yield, biomass, HI — overall and by treatment/year |
| `calibration_recommendations.csv` | Per-genotype suggested IRUE, bdEMRR1, bdR5R7/R7R8, PDHI, drought sensitivity notes |
| `results/test_results_yearly.csv` | Full model output: one row per scenario-year (64 columns) |
| `results/daily/*.csv` | Daily time-step outputs for each scenario (biomass, water, phenology) |

### Plots (in `outputs/plots/`)

| Plot | Description |
|------|-------------|
| `01_yield_1to1_all.png` | Observed vs simulated yield 1:1 scatter, all sites/years, colored by treatment |
| `02_yield_by_treatment.png` | 1:1 scatter faceted by Irrigated/Rainfed, labeled by genotype |
| `03_yield_by_genotype_sarec.png` | Bar chart: obs vs sim yield per genotype, SAREC only, faceted by year × treatment |
| `04_inseason_biomass.png` | In-season biomass curves (sim) vs observed points, SAREC 2024, 6 genotypes |
| `05_yield_reduction.png` | Rainfed yield reduction % vs irrigated: obs vs sim, by genotype, SAREC |
| `06_phenology.png` | R1 and R8 DOY scatter (obs vs sim), SAREC 2024 |
| `07_HI_analysis.png` | Biomass vs yield (HI analysis), SAREC, obs points and sim stars |
| `08_relative_error.png` | Relative yield error % per genotype, SAREC (averaged 2024+2025) |
| `09_multisite.png` | Multi-site yield comparison 2024 vs 2025 |
| `10_phenology_stages.png` | Bar chart: obs vs sim DOY for R1, R3, R5, R7, R8 per genotype, SAREC 2024 |
| `11_phenology_scatter.png` | R1 flowering DOY scatter (obs vs sim) with 1:1 line, SAREC 2024 |
| `12_biomass_total_inseason.png` | In-season total biomass: simulated lines + observed points, by treatment |
| `13_biomass_partitioning.png` | Stacked bar: Seed + Stem + Other biomass at 3 sampling dates, obs vs sim |
| `14_HI_progression.png` | Harvest index progression over season (obs vs sim), by treatment |

---

## Test Scenario Design

The 72 test scenarios cover:
- **12 genotypes**: P42A84E, P48A14E, P52A14SE, PI471938, PI507408, PI548431,
  PI603457A, R18-14502, R18C-13665, R19-45980, R19-46252, R19C-1012
- **3 sites**: SAREC (AR, 2024+2025), Pinetree (AR, 2024), Rohwer (AR, 2024)
- **2 treatments**: Irrigated (IRRI) and Rainfed (RFD)

In this initial run, **all genotypes share the same MG4 baseline parameters**
(IRUE=2.0, etc.). This establishes the baseline performance before per-genotype
calibration.

---

## Troubleshooting

**"Cannot locate repo root"**: Open `SSM-model.Rproj` in RStudio, or set
`REPO_ROOT <- "/path/to/SSM-model"` before sourcing.

**"Weather file not found"**: Ensure you have the weather Excel files in
`r-model/inputs/weather/`. The test scenarios use `SSM_SAREC_AR.xlsx`,
`SSM_PINETREE_AR.xlsx`, and `SSM_ROHWER_AR.xlsx`.

**Plots 10-14 skipped**: These require daily outputs. Set `SAVE_DAILY <- TRUE`
in `run_model_testing.R` and re-run Step 1.

**Package not found**: Run `install.packages(c("ggplot2", "patchwork", "ggrepel", "tidyr", "lubridate", "readxl", "dplyr", "jsonlite"))`.
