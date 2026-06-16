# =============================================================================
# SSM Soybean Model - Simulation Summary Report
# =============================================================================
# Reads all_results.csv (and optionally all_daily.csv) and writes a
# self-contained HTML report summarising what was simulated and showing
# key summary statistics.
#
# Usage:
#   Rscript 11_simulation_report.R
#   source("11_simulation_report.R")          # from R console
#   generate_report(out_html = "my_report.html")  # custom output path
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
})

# ── Locate r-model base directory (same logic as 08_run_model.R) ─────────────
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
      else stop("Cannot find r-model base directory. Set BASE_DIR or open the project.")
    }
  }, error = function(e) stop(e$message))
}

RESULTS_DIR <- file.path(BASE_DIR, "outputs", "results")
DAILY_DIR   <- file.path(RESULTS_DIR, "daily")


# =============================================================================
# FUNCTION: generate_report
# =============================================================================
generate_report <- function(
    results_file = NULL,
    out_html     = NULL
) {
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

  # Optional daily data
  daily_file <- file.path(DAILY_DIR, "all_daily.csv")
  has_daily  <- file.exists(daily_file)
  if (has_daily) {
    cat("Reading daily results:", daily_file, "\n")
    dd <- read.csv(daily_file, stringsAsFactors = FALSE)
    cat(sprintf("  %d daily rows\n", nrow(dd)))
  }

  # ── Helper: HTML table from data.frame ─────────────────────────────────────
  df_to_html <- function(d, id = "", class = "tbl") {
    id_attr <- if (nzchar(id)) sprintf(' id="%s"', id) else ""
    header  <- paste(sprintf("<th>%s</th>", names(d)), collapse = "")
    rows    <- apply(d, 1, function(r) {
      cells <- paste(sprintf("<td>%s</td>", r), collapse = "")
      sprintf("<tr>%s</tr>", cells)
    })
    sprintf('<table%s class="%s"><thead><tr>%s</tr></thead><tbody>%s</tbody></table>',
            id_attr, class, header, paste(rows, collapse = "\n"))
  }

  fmt1 <- function(x) formatC(round(x, 1), format = "f", digits = 1)
  fmt0 <- function(x) formatC(round(x, 0), format = "f", digits = 0, big.mark = ",")
  fmtp <- function(x) paste0(formatC(round(x * 100, 1), format = "f", digits = 1), " %")

  # ── 1. Overview stats ───────────────────────────────────────────────────────
  n_scen     <- length(unique(df$sName))
  n_loc      <- length(unique(df$Location))
  n_years    <- length(unique(df$Pyear))
  year_range <- range(df$Pyear, na.rm = TRUE)
  n_rows     <- nrow(df)
  n_manag    <- length(unique(df$Manag))
  n_crops    <- length(unique(df$Crop))
  n_normal   <- sum(df$MATYP == 1, na.rm = TRUE)
  n_prem     <- sum(df$MATYP == 2, na.rm = TRUE)
  n_flood    <- sum(df$MATYP == 5, na.rm = TRUE)

  locations  <- sort(unique(df$Location))
  managements <- sort(unique(df$Manag))
  crops       <- sort(unique(df$Crop))

  # ── 2. Summary table by location ───────────────────────────────────────────
  loc_sum <- df %>%
    group_by(Location) %>%
    summarise(
      N      = n(),
      `Yield (kg/ha)`   = paste0(fmt0(mean(Ywet,   na.rm=TRUE)), " ± ", fmt0(sd(Ywet,   na.rm=TRUE))),
      `WTOP (g/m²)`     = paste0(fmt1(mean(WTOP,   na.rm=TRUE)), " ± ", fmt1(sd(WTOP,   na.rm=TRUE))),
      `MXLAI`           = paste0(fmt1(mean(MXLAI,  na.rm=TRUE)), " ± ", fmt1(sd(MXLAI,  na.rm=TRUE))),
      `HI`              = paste0(fmtp(mean(HI,     na.rm=TRUE)), " ± ", fmtp(sd(HI,     na.rm=TRUE))),
      `Season (days)`   = paste0(fmt1(mean(R8,     na.rm=TRUE)), " ± ", fmt1(sd(R8,     na.rm=TRUE))),
      `CTR (mm)`        = paste0(fmt1(mean(CTR,    na.rm=TRUE)), " ± ", fmt1(sd(CTR,    na.rm=TRUE))),
      `CE (mm)`         = paste0(fmt1(mean(CE,     na.rm=TRUE)), " ± ", fmt1(sd(CE,     na.rm=TRUE))),
      `CRAIN (mm)`      = paste0(fmt1(mean(CRAIN,  na.rm=TRUE)), " ± ", fmt1(sd(CRAIN,  na.rm=TRUE))),
      .groups = "drop"
    )

  # ── 3. Summary by cultivar type ────────────────────────────────────────────
  crop_sum <- df %>%
    group_by(Crop) %>%
    summarise(
      N      = n(),
      `Yield (kg/ha)`   = paste0(fmt0(mean(Ywet, na.rm=TRUE)), " ± ", fmt0(sd(Ywet, na.rm=TRUE))),
      `WTOP (g/m²)`     = paste0(fmt1(mean(WTOP, na.rm=TRUE)), " ± ", fmt1(sd(WTOP, na.rm=TRUE))),
      `CTR (mm)`        = paste0(fmt1(mean(CTR,  na.rm=TRUE)), " ± ", fmt1(sd(CTR,  na.rm=TRUE))),
      `HI`              = paste0(fmtp(mean(HI,   na.rm=TRUE)), " ± ", fmtp(sd(HI,   na.rm=TRUE))),
      .groups = "drop"
    )

  # ── 4. Summary by management (water regime × planting date) ────────────────
  df$WaterType <- ifelse(grepl("IRR|IRRI", df$Manag, ignore.case=TRUE), "Irrigated", "Rainfed")
  df$PlantWin  <- case_when(
    grepl("ELY", df$Manag, ignore.case=TRUE) ~ "Early",
    grepl("MID", df$Manag, ignore.case=TRUE) ~ "Mid",
    grepl("LTE", df$Manag, ignore.case=TRUE) ~ "Late",
    TRUE ~ "Other"
  )
  manag_sum <- df %>%
    group_by(`Water regime` = WaterType, `Planting window` = PlantWin) %>%
    summarise(
      N    = n(),
      `Yield (kg/ha)` = paste0(fmt0(mean(Ywet,  na.rm=TRUE)), " ± ", fmt0(sd(Ywet,  na.rm=TRUE))),
      `CTR (mm)`      = paste0(fmt1(mean(CTR,   na.rm=TRUE)), " ± ", fmt1(sd(CTR,   na.rm=TRUE))),
      `CIRGW (mm)`    = paste0(fmt1(mean(CIRGW, na.rm=TRUE))),
      `IRGNO`         = paste0(fmt1(mean(IRGNO, na.rm=TRUE))),
      .groups = "drop"
    )

  # ── 5. Phenology summary ────────────────────────────────────────────────────
  pheno_sum <- df %>%
    group_by(Location) %>%
    summarise(
      `EMR (DAP)` = paste0(fmt1(mean(dtEMR, na.rm=TRUE)), " ± ", fmt1(sd(dtEMR, na.rm=TRUE))),
      `R1 (DAP)`  = paste0(fmt1(mean(R1,    na.rm=TRUE)), " ± ", fmt1(sd(R1,    na.rm=TRUE))),
      `R5 (DAP)`  = paste0(fmt1(mean(R5,    na.rm=TRUE)), " ± ", fmt1(sd(R5,    na.rm=TRUE))),
      `R7 (DAP)`  = paste0(fmt1(mean(R7,    na.rm=TRUE)), " ± ", fmt1(sd(R7,    na.rm=TRUE))),
      `R8 (DAP)`  = paste0(fmt1(mean(R8,    na.rm=TRUE)), " ± ", fmt1(sd(R8,    na.rm=TRUE))),
      .groups = "drop"
    )

  # ── 6. Water balance summary ────────────────────────────────────────────────
  water_sum <- df %>%
    group_by(Location) %>%
    summarise(
      `CRAIN (mm)`  = fmt1(mean(CRAIN,  na.rm=TRUE)),
      `CTR (mm)`    = fmt1(mean(CTR,    na.rm=TRUE)),
      `CE (mm)`     = fmt1(mean(CE,     na.rm=TRUE)),
      `CDRAIN (mm)` = fmt1(mean(CDRAIN, na.rm=TRUE)),
      `CRUNOF (mm)` = fmt1(mean(CRUNOF, na.rm=TRUE)),
      `ET (mm)`     = fmt1(mean(ET,     na.rm=TRUE)),
      `E/ET (%)`    = fmtp(mean(EoverET,na.rm=TRUE)),
      .groups = "drop"
    )

  # ── 7. Maturity type breakdown ──────────────────────────────────────────────
  mat_sum <- df %>%
    group_by(Location) %>%
    summarise(
      `Total years` = n(),
      `Normal (1)`  = sum(MATYP == 1, na.rm=TRUE),
      `Premature (2)` = sum(MATYP == 2, na.rm=TRUE),
      `Flood kill (5)` = sum(MATYP == 5, na.rm=TRUE),
      .groups = "drop"
    )

  # ── 8. Environmental summary ────────────────────────────────────────────────
  env_sum <- df %>%
    group_by(Location) %>%
    summarise(
      `Tmin (°C)` = fmt1(mean(MTMINT, na.rm=TRUE)),
      `Tmax (°C)` = fmt1(mean(MTMAXT, na.rm=TRUE)),
      `SRAD (MJ/m²)` = fmt0(mean(SSRADT, na.rm=TRUE)),
      `Rain (mm)`    = fmt0(mean(SRAINT, na.rm=TRUE)),
      `ET (mm)`      = fmt0(mean(SUMETT, na.rm=TRUE)),
      .groups = "drop"
    )

  # ── 9. Daily summary (if available) ────────────────────────────────────────
  daily_section <- ""
  if (has_daily) {
    n_daily_scen  <- length(unique(dd$sName))
    n_daily_rows  <- nrow(dd)
    avg_season_len <- round(n_daily_rows / nrow(df), 0)
    daily_section <- sprintf('
<h2 id="daily">8. Daily Output Summary</h2>
<div class="card">
  <p>Daily time-step data were saved for <strong>%d scenarios</strong>
  (%s rows total). Average season length: <strong>%d days</strong>.</p>
  <p>Daily outputs are stored in <code>outputs/results/daily/</code> as
  per-location CSV files plus a combined <code>all_daily.csv</code>.</p>
  <p>Key daily variables (see parameter_dictionary.xlsx Sheet 3 for full column list):</p>
  <ul>
    <li><strong>doy / DAP / CBD</strong> — time and phenological position</li>
    <li><strong>LAI / FINT / DDMP</strong> — canopy and DM production</li>
    <li><strong>WTOP / WGRN / SGR</strong> — biomass accumulation and grain growth</li>
    <li><strong>TR / SEVP / FTSWRZ</strong> — water fluxes and soil water status</li>
    <li><strong>WSFL / WSFG / WSFD</strong> — water stress factors (0 = max stress)</li>
  </ul>
</div>', n_daily_scen, format(n_daily_rows, big.mark=","), avg_season_len)
  }

  # ── Assemble HTML ───────────────────────────────────────────────────────────
  toc_daily <- if (has_daily) '<a href="#daily">8. Daily output</a>' else ""

  html <- sprintf('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>SSM Soybean — Simulation Report</title>
<style>
  :root {
    --primary: #1a5276; --accent: #2e86c1; --green: #1e8449;
    --orange: #d35400; --bg: #f8f9fa; --card: #ffffff;
    --border: #dee2e6; --text: #212529; --muted: #6c757d;
  }
  * { box-sizing: border-box; margin: 0; padding: 0; }
  body { font-family: "Segoe UI", Arial, sans-serif; font-size: 14px;
         color: var(--text); background: var(--bg); line-height: 1.65; }
  #toc { position: fixed; top: 0; left: 0; width: 220px; height: 100vh;
         overflow-y: auto; background: var(--primary); color: #cde; padding: 12px 0; z-index: 100; }
  #toc h3 { padding: 10px 16px 6px; font-size: 11px; text-transform: uppercase;
             letter-spacing: 1px; color: #acc; }
  #toc a { display: block; padding: 5px 16px; font-size: 12px; color: #cde;
            text-decoration: none; border-left: 3px solid transparent; }
  #toc a:hover { background: rgba(255,255,255,.1); border-left-color: #5bc0de; color: #fff; }
  main { margin-left: 220px; padding: 28px 44px 64px; max-width: 1080px; }
  h1 { font-size: 1.9rem; color: var(--primary); border-bottom: 3px solid var(--accent);
       padding-bottom: 8px; margin-bottom: 4px; }
  .subtitle { color: var(--muted); font-size: 13px; margin-bottom: 28px; }
  h2 { font-size: 1.3rem; color: var(--primary); margin: 36px 0 12px;
       border-left: 5px solid var(--accent); padding-left: 10px; }
  h3 { font-size: 1.1rem; color: var(--accent); margin: 22px 0 8px; }
  p  { margin-bottom: 8px; }
  ul, ol { margin: 6px 0 10px 22px; }
  li { margin-bottom: 3px; }
  .card { background: var(--card); border: 1px solid var(--border); border-radius: 6px;
          padding: 16px 20px; margin: 14px 0; }
  .stat-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(160px, 1fr));
               gap: 12px; margin: 14px 0; }
  .stat-box  { background: var(--card); border: 1px solid var(--border); border-radius: 6px;
               padding: 14px 16px; text-align: center; }
  .stat-val  { font-size: 2rem; font-weight: 700; color: var(--primary); }
  .stat-lbl  { font-size: 11px; color: var(--muted); text-transform: uppercase; letter-spacing: 0.5px; }
  table.tbl  { border-collapse: collapse; width: 100%%; font-size: 12.5px; margin: 12px 0; }
  table.tbl th { background: var(--primary); color: white; padding: 7px 10px;
                 text-align: left; font-weight: 600; }
  table.tbl td { padding: 6px 10px; border-bottom: 1px solid var(--border); }
  table.tbl tr:nth-child(even) td { background: #f2f6fb; }
  table.tbl tr:hover td { background: #e8f4fd; }
  code { background: #f1f3f4; padding: 1px 4px; border-radius: 3px; font-size: 92%%; }
  .note { background: #eaf7fb; border-left: 4px solid var(--accent);
          padding: 10px 14px; margin: 10px 0; border-radius: 0 4px 4px 0; }
  .warn { background: #fef9e7; border-left: 4px solid #f39c12;
          padding: 10px 14px; margin: 10px 0; border-radius: 0 4px 4px 0; }
  .badge { display: inline-block; padding: 2px 8px; border-radius: 12px; font-size: 11px;
           font-weight: 600; }
  .badge-green  { background: #d5f5e3; color: var(--green); }
  .badge-orange { background: #fdebd0; color: var(--orange); }
  .badge-red    { background: #fadbd8; color: #c0392b; }
</style>
</head>
<body>
<nav id="toc">
  <h3>Report</h3>
  <a href="#overview">1. Overview</a>
  <a href="#locations">2. Locations</a>
  <a href="#scenarios">3. Scenarios</a>
  <a href="#yield">4. Yield &amp; Biomass</a>
  <a href="#phenology">5. Phenology</a>
  <a href="#water">6. Water balance</a>
  <a href="#maturity">7. Maturity status</a>
  %s
</nav>
<main>
<h1>SSM Soybean Model — Simulation Report</h1>
<p class="subtitle">Generated: %s &nbsp;|&nbsp; Results file: <code>%s</code></p>

<!-- ── 1. OVERVIEW ─────────────────────────────────────────────────── -->
<h2 id="overview">1. Overview</h2>
<div class="stat-grid">
  <div class="stat-box"><div class="stat-val">%s</div><div class="stat-lbl">Scenarios</div></div>
  <div class="stat-box"><div class="stat-val">%s</div><div class="stat-lbl">Locations</div></div>
  <div class="stat-box"><div class="stat-val">%s</div><div class="stat-lbl">Years</div></div>
  <div class="stat-box"><div class="stat-val">%s</div><div class="stat-lbl">Sim. years total</div></div>
  <div class="stat-box"><div class="stat-val">%s</div><div class="stat-lbl">Managements</div></div>
  <div class="stat-box"><div class="stat-val">%s</div><div class="stat-lbl">Cultivars</div></div>
</div>
<div class="note">
  Year range simulated: <strong>%d – %d</strong>.
  Normal maturity: <strong>%s (%.1f%%)</strong> &nbsp;|&nbsp;
  Premature senescence: <strong>%s (%.1f%%)</strong> &nbsp;|&nbsp;
  Flood kill: <strong>%s (%.1f%%)</strong>.
</div>

<!-- ── 2. LOCATIONS ────────────────────────────────────────────────── -->
<h2 id="locations">2. Locations</h2>
<div class="card">
  <p>%d locations were simulated. Climate-year range: <strong>%d – %d</strong> (%d years).</p>
  <ul>%s</ul>
</div>
%s

<!-- ── 3. SCENARIOS ────────────────────────────────────────────────── -->
<h2 id="scenarios">3. Scenario Structure</h2>
<div class="card">
  <p>Each scenario is identified by a four-part key:
  <strong>Location – Water regime – Planting window – Cultivar</strong>.</p>
  <ul>
    <li><strong>Water regime:</strong> RFD (rainfed), IRRI (irrigated)</li>
    <li><strong>Planting window:</strong> ELY (early), MID (mid-season), LTE (late)</li>
    <li><strong>Cultivar:</strong> check (standard, no LT trait), LT1.5 / LT2 / LT2.5 (limited transpiration VPDcr)</li>
  </ul>
</div>
<h3>Managements simulated</h3>
%s
<h3>Cultivars simulated</h3>
%s

<!-- ── 4. YIELD AND BIOMASS ─────────────────────────────────────────── -->
<h2 id="yield">4. Yield and Biomass — by Location (mean ± SD, all scenarios/years)</h2>
<p>Values represent all management types and cultivars combined. SD reflects year-to-year plus management-type variability.</p>
%s

<h3>By cultivar type</h3>
%s

<h3>By management (water regime × planting window)</h3>
%s

<!-- ── 5. PHENOLOGY ─────────────────────────────────────────────────── -->
<h2 id="phenology">5. Phenology — Days After Planting (mean ± SD)</h2>
<p>EMR = emergence, R1 = first flower, R5 = beginning seed fill, R7 = physiological maturity, R8 = harvest maturity.</p>
%s

<!-- ── 6. WATER BALANCE ─────────────────────────────────────────────── -->
<h2 id="water">6. Seasonal Water Balance by Location (mean, mm)</h2>
<div class="note">Values averaged across all management types and years.
  CTR = crop transpiration, CE = soil evaporation, ET = CTR + CE,
  CDRAIN = deep drainage, CRUNOF = surface runoff.</div>
%s

<!-- ── 7. MATURITY STATUS ───────────────────────────────────────────── -->
<h2 id="maturity">7. Maturity Type by Location</h2>
<div class="card">
  <p>
    <span class="badge badge-green">1 — Normal</span> maturity reached before stop date.
    &nbsp;
    <span class="badge badge-orange">2 — Premature</span> LAI dropped below 0.05 during grain fill (crop aborted early).
    &nbsp;
    <span class="badge badge-red">5 — Flood kill</span> flooded for more than FLDKIL consecutive days.
  </p>
</div>
%s

%s

</main>
</body>
</html>',
    # TOC
    toc_daily,
    # Subtitle
    format(Sys.time(), "%Y-%m-%d %H:%M"),
    basename(results_file),
    # Stat grid
    format(n_scen,  big.mark=","),
    format(n_loc,   big.mark=","),
    format(n_years, big.mark=","),
    format(n_rows,  big.mark=","),
    format(n_manag, big.mark=","),
    format(n_crops, big.mark=","),
    # Note
    year_range[1], year_range[2],
    format(n_normal, big.mark=","), n_normal/n_rows*100,
    format(n_prem,   big.mark=","), n_prem/n_rows*100,
    format(n_flood,  big.mark=","), n_flood/n_rows*100,
    # Locations section
    n_loc, year_range[1], year_range[2], n_years,
    paste(sprintf("<li>%s</li>", locations), collapse=""),
    df_to_html(env_sum, class="tbl"),
    # Scenarios section
    paste(sprintf('<span class="badge badge-green" style="margin:3px;">%s</span>', managements), collapse=" "),
    paste(sprintf('<span class="badge badge-orange" style="margin:3px;">%s</span>', crops), collapse=" "),
    # Yield section
    df_to_html(loc_sum,   class="tbl"),
    df_to_html(crop_sum,  class="tbl"),
    df_to_html(manag_sum, class="tbl"),
    # Phenology
    df_to_html(pheno_sum, class="tbl"),
    # Water balance
    df_to_html(water_sum, class="tbl"),
    # Maturity
    df_to_html(mat_sum, class="tbl"),
    # Daily section
    daily_section
  )

  writeLines(html, out_html)
  cat(sprintf("\nReport written: %s\n", out_html))
  invisible(out_html)
}


# Run if called directly
if (!interactive()) {
  generate_report()
}
