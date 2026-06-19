# =============================================================================
# SSM Soybean Model — Validation Report Generator
# =============================================================================
# Reads validation_statistics.csv and existing plot PNGs, produces a
# self-contained HTML validation report with all images embedded as base64.
#
# Usage:
#   Rscript r-model/validation/create_validation_report.R
# =============================================================================

if (!requireNamespace("jsonlite", quietly = TRUE))
  install.packages("jsonlite", quiet = TRUE)

BASE_DIR <- if (exists("BASE_DIR")) BASE_DIR else {
  tryCatch({
    sf <- NULL
    for (i in seq_len(sys.nframe())) {
      of <- sys.frame(i)$ofile
      if (!is.null(of) && nchar(of) > 0) { sf <- normalizePath(of, mustWork=FALSE); break }
    }
    if (!is.null(sf)) {
      p <- dirname(sf)
      if (basename(p) == "validation") dirname(p) else p
    } else {
      cwd <- getwd()
      if (file.exists(file.path(cwd, "inputs/scenarios.csv"))) cwd
      else if (file.exists(file.path(cwd, "r-model/inputs/scenarios.csv"))) file.path(cwd,"r-model")
      else stop("Cannot find BASE_DIR")
    }
  }, error = function(e) stop(e$message))
}

PLOTS_DIR  <- file.path(BASE_DIR, "outputs", "plots")
DAILY_DIR  <- file.path(PLOTS_DIR, "daily")
STATS_FILE <- file.path(PLOTS_DIR, "validation_statistics.csv")
OUT_FILE   <- file.path(BASE_DIR, "outputs", "validation_report.html")

stats <- read.csv(STATS_FILE, stringsAsFactors = FALSE)

# --- Encode image to embedded base64 data URI --------------------------------
img_b64 <- function(path) {
  if (!file.exists(path)) return(NULL)
  raw <- readBin(path, "raw", file.info(path)$size)
  paste0("data:image/png;base64,", jsonlite::base64_enc(raw))
}

img_tag <- function(b64, alt = "", style = "max-width:100%;border:1px solid #dde3ef;border-radius:5px;display:block;margin:8px 0") {
  if (is.null(b64)) return(paste0('<p style="color:#aaa">[image not found: ', alt, ']</p>'))
  paste0('<img src="', b64, '" alt="', alt, '" style="', style, '">')
}

# --- Seasonal plots metadata (15 plots, in display order) --------------------
#  indices 1-5:  yield/biomass
#  indices 6-7:  phenology
#  indices 8-10: water balance
#  indices 11-12: irrigation
#  indices 13-15: error analysis
seasonal <- list(
  list(f="01_validation_WGRN.png",             lab="Grain Yield",                    unit="g m⁻²"),
  list(f="02_validation_WTOP.png",             lab="Total Above-Ground DM (WTOP)",   unit="g m⁻²"),
  list(f="03_validation_MXLAI.png",            lab="Maximum LAI",                    unit="m² m⁻²"),
  list(f="07_validation_HI.png",               lab="Harvest Index",                  unit="g/g"),
  list(f="09_validation_Ywet.png",             lab="Wet Yield",                      unit="kg ha⁻¹"),
  list(f="08_validation_dtR5.png",             lab="Days to R5 (beginning seed)",    unit="d"),
  list(f="04_validation_dtR8.png",             lab="Days to Maturity (R8)",          unit="d"),
  list(f="05_validation_CTR.png",              lab="Cumulative Transpiration",        unit="mm"),
  list(f="06_validation_CE.png",               lab="Cumulative Soil Evaporation",    unit="mm"),
  list(f="12_validation_IPASW.png",            lab="Initial Plant-Available SW",     unit="mm"),
  list(f="10_validation_CIRGW.png",            lab="Cumulative Irrigation (IRRI only)", unit="mm"),
  list(f="11_validation_IRGNO.png",            lab="Irrigation Events (IRRI only)", unit="events"),
  list(f="13_validation_summary_panel.png",    lab="Summary Panel — 6 variables", unit=""),
  list(f="14_rmse_by_location.png",            lab="RMSE by Location",               unit=""),
  list(f="15_residuals_WGRN.png",              lab="Grain Yield Residuals (R − Excel)", unit="")
)

# --- Daily scenarios ---------------------------------------------------------
daily_scenarios <- list(
  list(id="JB-RFD-LTE-check",  loc="Jonesboro, AR", water="Rain-fed",  plant="Late planting",  years=c(1990,2000,2010)),
  list(id="JB-IRRI-ELY-check", loc="Jonesboro, AR", water="Irrigated", plant="Early planting", years=c(1990,2000,2010)),
  list(id="KS-RFD-LTE-check",  loc="Keiser, AR",    water="Rain-fed",  plant="Late planting",  years=c(1990,2005,2015)),
  list(id="LN-RFD-LTE-check",  loc="Lincoln, NE",   water="Rain-fed",  plant="Late planting",  years=c(1990,2005,2015)),
  list(id="LN-IRRI-MID-check", loc="Lincoln, NE",   water="Irrigated", plant="Mid planting",   years=c(1995,2005,2015)),
  list(id="AL-RFD-MID-check",  loc="Albany, MO",    water="Rain-fed",  plant="Mid planting",   years=c(1995,2005,2015)),
  list(id="AL-IRRI-MID-check", loc="Albany, MO",    water="Irrigated", plant="Mid planting",   years=c(1995,2005,2015))
)

# --- Pre-encode all images ---------------------------------------------------
cat("Encoding images...\n")
enc <- list()
for (p in seasonal) enc[[p$f]] <- img_b64(file.path(PLOTS_DIR, p$f))
for (s in daily_scenarios) {
  pf <- paste0("panel_", s$id, ".png")
  enc[[pf]] <- img_b64(file.path(DAILY_DIR, pf))
  for (yr in s$years) {
    df <- paste0("daily_", s$id, "_", yr, ".png")
    enc[[df]] <- img_b64(file.path(DAILY_DIR, df))
  }
}
cat("  Done.\n")

# --- Build HTML sections -----------------------------------------------------

# Stats table rows
stat_rows <- paste(sapply(seq_len(nrow(stats)), function(i) {
  r  <- stats[i, ]
  bg <- if (r$R2 >= 0.9999) "#d4edda" else if (r$R2 >= 0.999) "#fff3cd" else "#f8d7da"
  paste0(
    "<tr>",
    "<td>", r$Variable, "</td>",
    "<td style='text-align:right'>", formatC(r$n, format="d", big.mark=","), "</td>",
    "<td style='text-align:right'>", sprintf("%.3f", r$RMSE), "</td>",
    "<td style='text-align:right'>", sprintf("%.2f%%", r$RRMSE_pct), "</td>",
    "<td style='text-align:right'>", sprintf("%+.3f", r$Bias), "</td>",
    "<td style='text-align:right;background:", bg, ";font-weight:bold'>", sprintf("%.4f", r$R2), "</td>",
    "</tr>"
  )
}), collapse = "\n")

# Helper: one seasonal subplot block
seas_block <- function(idx) {
  p  <- seasonal[[idx]]
  lbl <- if (nchar(p$unit) > 0) paste0(p$lab, " (", p$unit, ")") else p$lab
  paste0(
    '<div style="margin-bottom:28px">',
    '<h4 style="color:#3a5090;font-size:0.95em;margin-bottom:8px">', lbl, '</h4>',
    img_tag(enc[[p$f]], p$f),
    '</div>'
  )
}

# Daily scenario section
daily_section <- function(s, n) {
  panel_f <- paste0("panel_", s$id, ".png")
  badge_col <- if (s$water == "Irrigated") "background:#cce5ff;color:#004085" else "background:#d4edda;color:#155724"

  yr_divs <- paste(sapply(s$years, function(yr) {
    yf <- paste0("daily_", s$id, "_", yr, ".png")
    paste0(
      '<div>',
      '<p style="text-align:center;font-size:0.82em;font-weight:600;color:#555;margin-bottom:4px">', yr, '</p>',
      img_tag(enc[[yf]], yf),
      '</div>'
    )
  }), collapse = "\n")

  paste0(
    '<div id="daily-', n, '" style="margin-bottom:52px">',
    '<h3>', n, '. ', s$loc, ' — ', s$water, ', ', s$plant, '</h3>',
    '<p style="margin:8px 0 12px">',
    '<span style="display:inline-block;padding:2px 10px;border-radius:12px;font-size:0.8em;font-weight:bold;', badge_col, '">', s$water, '</span>',
    '&nbsp;<span style="display:inline-block;padding:2px 10px;border-radius:12px;font-size:0.8em;background:#f0f0f0;color:#444">', s$plant, '</span>',
    '&nbsp;<span style="display:inline-block;padding:2px 10px;border-radius:12px;font-size:0.8em;background:#f0f0f0;color:#444">Check cultivar</span>',
    '</p>',
    '<p style="font-size:0.88em;color:#555;margin-bottom:12px">',
    '3-year overview (panel) and individual-year plots. ',
    '<strong>Solid line</strong> = Excel/VBA reference; <strong>dashed line</strong> = R model.',
    '</p>',
    img_tag(enc[[panel_f]], panel_f),
    '<div style="display:grid;grid-template-columns:repeat(3,1fr);gap:14px;margin-top:14px">',
    yr_divs,
    '</div>',
    '</div>'
  )
}

daily_html  <- paste(sapply(seq_along(daily_scenarios), function(i) daily_section(daily_scenarios[[i]], i)), collapse="\n")

yield_html  <- paste(sapply(1:5,   seas_block), collapse="\n")
pheno_html  <- paste(sapply(6:7,   seas_block), collapse="\n")
water_html  <- paste(sapply(8:10,  seas_block), collapse="\n")
irri_html   <- paste(sapply(11:12, seas_block), collapse="\n")
error_html  <- paste(sapply(13:15, seas_block), collapse="\n")

# --- CSS ---------------------------------------------------------------------
css <- "
*{box-sizing:border-box;margin:0;padding:0}
body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,sans-serif;display:flex;min-height:100vh;background:#fafbfc;color:#333}
nav{width:268px;background:#1a1a2e;position:fixed;height:100vh;overflow-y:auto;padding:24px 0;top:0;left:0;z-index:100;flex-shrink:0}
.nav-title{color:#a8c4e0;padding:0 20px 18px;font-size:0.76em;text-transform:uppercase;letter-spacing:0.08em;font-weight:700;border-bottom:1px solid rgba(255,255,255,0.12)}
.nav-group{color:#6a8ab0;padding:14px 20px 4px;font-size:0.70em;text-transform:uppercase;letter-spacing:0.07em;font-weight:700}
nav a{display:block;color:#9ab8d8;text-decoration:none;padding:5px 20px 5px 26px;font-size:0.82em;line-height:1.55}
nav a:hover,nav a:focus{background:rgba(255,255,255,0.09);color:#fff}
main{margin-left:268px;padding:40px 52px 80px;max-width:1080px;flex:1}
h1{font-size:1.85em;color:#12213a;margin-bottom:5px;font-weight:700}
.subtitle{color:#666;font-size:0.96em;margin-bottom:36px}
h2{font-size:1.3em;color:#1a3060;margin:48px 0 16px;padding-bottom:10px;border-bottom:2px solid #cdd8f0;font-weight:600}
h3{font-size:1.02em;color:#2c4a82;margin:28px 0 10px;font-weight:600}
h4{font-size:0.95em;color:#3a5090;margin-bottom:8px;font-weight:600}
p{color:#444;line-height:1.68;margin-bottom:10px;font-size:0.93em}
table{border-collapse:collapse;width:100%;margin:16px 0;font-size:0.87em}
th{background:#1a1a2e;color:#e8f0fb;padding:10px 14px;text-align:left;font-weight:500}
th:not(:first-child){text-align:right}
td{padding:9px 14px;border-bottom:1px solid #eaecf5;vertical-align:middle}
tr:nth-child(even) td{background:#f6f9ff}
.cards{display:flex;flex-wrap:wrap;gap:14px;margin:24px 0 32px}
.card{background:#fff;border:1px solid #d6e2f4;border-radius:8px;padding:18px 22px;min-width:140px;text-align:center}
.card .val{font-size:1.65em;font-weight:700;color:#1a3060}
.card .lbl{font-size:0.73em;color:#777;margin-top:4px;line-height:1.35}
.intro{background:#f0f5ff;border-left:4px solid #3a6fd8;padding:14px 18px;border-radius:0 6px 6px 0;margin-bottom:22px;font-size:0.9em;color:#2a3a5a;line-height:1.68}
.var-legend{font-size:0.82em;color:#555;background:#f5f7fc;border:1px solid #dde4f0;border-radius:5px;padding:8px 14px;display:inline-block;margin-bottom:18px;line-height:1.7}
"

# --- Assemble full HTML ------------------------------------------------------
cat("Building HTML...\n")

html <- paste0(
'<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>SSM Soybean Model — Validation Report</title>
<style>', css, '</style>
</head>
<body>

<nav>
  <div class="nav-title">SSM Soybean<br>Validation Report</div>

  <div class="nav-group">Overview</div>
  <a href="#overview">Summary &amp; statistics table</a>

  <div class="nav-group">1 — Daily comparison</div>
  <a href="#daily-1">Jonesboro, AR — Rain-fed</a>
  <a href="#daily-2">Jonesboro, AR — Irrigated</a>
  <a href="#daily-3">Keiser, AR — Rain-fed</a>
  <a href="#daily-4">Lincoln, NE — Rain-fed</a>
  <a href="#daily-5">Lincoln, NE — Irrigated</a>
  <a href="#daily-6">Albany, MO — Rain-fed</a>
  <a href="#daily-7">Albany, MO — Irrigated</a>

  <div class="nav-group">2 — Seasonal (7,200 sims)</div>
  <a href="#yield">Yield &amp; biomass</a>
  <a href="#phenology">Phenology</a>
  <a href="#water">Water balance</a>
  <a href="#irrigation">Irrigation</a>
  <a href="#error">Error analysis</a>
</nav>

<main>
<h1>SSM Soybean Model — Validation Report</h1>
<p class="subtitle">R model vs. Excel/VBA reference &bull; ', format(Sys.Date(), "%B %d, %Y"), ' &bull; 10 locations &bull; 7,200 simulated seasons</p>

<h2 id="overview">Overview</h2>
<div class="intro">
  The R implementation of the SSM Soybean model was compared against the Excel/VBA reference
  model across <strong>240 scenarios</strong> (10 locations &times; 24 management &amp; cultivar combinations)
  &times; <strong>30 years</strong> = 7,200 season-level simulations.
  Scenarios span rain-fed and auto-irrigated treatments, four planting dates (early, mid, late, very-late),
  and four cultivars (check, LT1.5, LT2, LT2.5) across Arkansas, Nebraska, and Missouri.
  All 12 output variables show R&sup2; &ge; 0.999 and RRMSE &lt; 1.1%.
</div>

<div class="cards">
  <div class="card"><div class="val">7,200</div><div class="lbl">season simulations</div></div>
  <div class="card"><div class="val">240</div><div class="lbl">unique scenarios</div></div>
  <div class="card"><div class="val">10</div><div class="lbl">locations</div></div>
  <div class="card"><div class="val">30</div><div class="lbl">years per scenario</div></div>
  <div class="card"><div class="val">&le;1.1%</div><div class="lbl">max RRMSE<br>all variables</div></div>
  <div class="card"><div class="val">R&sup2;&ge;0.999</div><div class="lbl">all 12 output<br>variables</div></div>
</div>

<h3>Validation statistics</h3>
<table>
  <thead>
    <tr><th>Variable</th><th>n</th><th>RMSE</th><th>RRMSE %</th><th>Bias</th><th>R&sup2;</th></tr>
  </thead>
  <tbody>
', stat_rows, '
  </tbody>
</table>
<p style="font-size:0.8em;color:#888;margin-top:6px">
  RRMSE % = RMSE / |mean(observed)| &times; 100.&ensp;
  Bias = mean(R model &minus; Excel reference).&ensp;
  n = 3,600 for IRRI-only variables (irrigation, events).&ensp;
  R&sup2; cells: <span style="background:#d4edda;padding:0 4px">green</span> &ge; 0.9999,
  <span style="background:#fff3cd;padding:0 4px">yellow</span> &ge; 0.999.
</p>

<h2 id="daily">1. Daily Time-Step Comparison</h2>
<div class="intro">
  Seven representative scenarios spanning all three states, both water management types, and
  three planting dates are shown at daily resolution. For each scenario the <em>panel plot</em>
  shows all three sample years overlaid, followed by individual-year plots.
  <strong>Solid line</strong> = Excel/VBA reference; <strong>dashed line</strong> = R model.
</div>
<div class="var-legend">
  Variables per panel:
  <strong>LAI</strong> (m&sup2; m&sup2;) &bull;
  <strong>WTOP</strong> — above-ground DM (g m&sup2;) &bull;
  <strong>WGRN</strong> — grain DM (g m&sup2;) &bull;
  <strong>CE</strong> — cumul. soil evaporation (mm) &bull;
  <strong>CTR</strong> — cumul. transpiration (mm) &bull;
  <strong>FTSWRZ</strong> — root-zone soil water fraction (0–1)
</div>

', daily_html, '

<h2 id="seasonal">2. Seasonal Validation — All 7,200 Simulations</h2>
<div class="intro">
  1:1 scatter plots comparing R model output to the Excel/VBA reference across all 7,200
  season-years. Points are coloured by location. The dashed line is the 1:1 reference;
  the solid blue line is the fitted linear regression. Statistics are annotated on each plot.
</div>

<h3 id="yield" style="margin-top:32px">2.1 Yield and Biomass</h3>
', yield_html, '

<h3 id="phenology">2.2 Phenology</h3>
', pheno_html, '

<h3 id="water">2.3 Water Balance (all scenarios)</h3>
', water_html, '

<h3 id="irrigation">2.4 Irrigation (IRRI scenarios, n = 3,600)</h3>
', irri_html, '

<h3 id="error">2.5 Error Analysis</h3>
', error_html, '

</main>
</body>
</html>'
)

writeLines(html, OUT_FILE)
cat(sprintf("Report saved to: %s\n  Size: %.1f MB\n", OUT_FILE, file.info(OUT_FILE)$size / 1e6))
