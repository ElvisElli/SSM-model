"""
Generate simulation_report.html from all_results.csv without R.
Run: python generate_report_py.py
"""
import csv, os, math
from collections import defaultdict
from datetime import datetime

HERE        = os.path.dirname(os.path.abspath(__file__))
RESULTS_CSV = os.path.join(HERE, "all_results.csv")
OUT_HTML    = os.path.join(HERE, "simulation_report.html")

# ---------------------------------------------------------------------------
def load_csv(path):
    with open(path, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        return list(reader)

def fnum(vals, digits=1):
    clean = [float(v) for v in vals if v not in ("", "NA", "NaN", "nan")]
    if not clean: return "—"
    m = sum(clean) / len(clean)
    s = math.sqrt(sum((x-m)**2 for x in clean) / max(len(clean)-1, 1))
    fmt = f"{{:.{digits}f}}"
    return f"{fmt.format(m)} ± {fmt.format(s)}"

def fmean(vals, digits=1):
    clean = [float(v) for v in vals if v not in ("", "NA", "NaN", "nan")]
    if not clean: return "—"
    fmt = f"{{:.{digits}f}}"
    return fmt.format(sum(clean)/len(clean))

def fcount(vals):
    return f"{len([v for v in vals if v not in ('','NA','NaN','nan')]):,}"

def group_by(rows, key):
    d = defaultdict(list)
    for r in rows: d[r[key]].append(r)
    return d

def html_table(headers, rows_data, cls="tbl"):
    th = "".join(f"<th>{h}</th>" for h in headers)
    body = ""
    for row in rows_data:
        body += "<tr>" + "".join(f"<td>{c}</td>" for c in row) + "</tr>\n"
    return f'<table class="{cls}"><thead><tr>{th}</tr></thead><tbody>{body}</tbody></table>'

# ---------------------------------------------------------------------------
print("Loading results…")
df = load_csv(RESULTS_CSV)
print(f"  {len(df):,} rows, {len(df[0])} columns")

def col(rows, name):
    return [r.get(name, "") for r in rows]

n_rows  = len(df)
scenarios = sorted(set(col(df, "sName")))
locations = sorted(set(col(df, "Location")))
managements = sorted(set(col(df, "Manag")))
crops       = sorted(set(col(df, "Crop")))
years       = sorted(set(col(df, "Pyear")))
year_range  = (years[0], years[-1])

n_normal = sum(1 for r in df if r.get("MATYP","") == "1")
n_prem   = sum(1 for r in df if r.get("MATYP","") == "2")
n_flood  = sum(1 for r in df if r.get("MATYP","") == "5")

# ── Yield & biomass by location ─────────────────────────────────────────────
by_loc = group_by(df, "Location")
loc_rows = []
for loc in locations:
    rows = by_loc[loc]
    loc_rows.append([
        loc,
        len(rows),
        fnum(col(rows,"Ywet"),   0),
        fnum(col(rows,"WTOP"),   1),
        fnum(col(rows,"MXLAI"),  2),
        fnum(col(rows,"HI"),     3),
        fnum(col(rows,"R8"),     1),
        fnum(col(rows,"CTR"),    1),
        fnum(col(rows,"CE"),     1),
        fnum(col(rows,"CRAIN"),  1),
    ])

# ── By cultivar ─────────────────────────────────────────────────────────────
by_crop = group_by(df, "Crop")
crop_rows = []
for crop in sorted(by_crop.keys()):
    rows = by_crop[crop]
    crop_rows.append([
        crop, len(rows),
        fnum(col(rows,"Ywet"),  0),
        fnum(col(rows,"WTOP"),  1),
        fnum(col(rows,"CTR"),   1),
        fnum(col(rows,"HI"),    3),
    ])

# ── By management ───────────────────────────────────────────────────────────
def water_type(manag): return "Irrigated" if "IRR" in manag.upper() else "Rainfed"
def plant_win(manag):
    m = manag.upper()
    if "ELY" in m: return "Early"
    if "MID" in m: return "Mid"
    if "LTE" in m: return "Late"
    return "Other"

manag_groups = defaultdict(list)
for r in df:
    key = (water_type(r["Manag"]), plant_win(r["Manag"]))
    manag_groups[key].append(r)

manag_rows = []
for key in sorted(manag_groups.keys()):
    rows = manag_groups[key]
    manag_rows.append([
        key[0], key[1], len(rows),
        fnum(col(rows,"Ywet"),  0),
        fnum(col(rows,"CTR"),   1),
        fmean(col(rows,"CIRGW"),1),
        fmean(col(rows,"IRGNO"),1),
    ])

# ── Phenology by location ───────────────────────────────────────────────────
pheno_rows = []
for loc in locations:
    rows = by_loc[loc]
    pheno_rows.append([loc,
        fnum(col(rows,"dtEMR"),1),
        fnum(col(rows,"R1"),   1),
        fnum(col(rows,"R5"),   1),
        fnum(col(rows,"R7"),   1),
        fnum(col(rows,"R8"),   1),
    ])

# ── Water balance ────────────────────────────────────────────────────────────
water_rows = []
for loc in locations:
    rows = by_loc[loc]
    water_rows.append([loc,
        fmean(col(rows,"CRAIN"),  1),
        fmean(col(rows,"CTR"),    1),
        fmean(col(rows,"CE"),     1),
        fmean(col(rows,"CDRAIN"), 1),
        fmean(col(rows,"CRUNOF"), 1),
        fmean(col(rows,"ET"),     1),
        fmean([str(float(v)*100) for v in col(rows,"EoverET") if v not in ("","NA")], 1) + " %",
    ])

# ── Maturity type ────────────────────────────────────────────────────────────
mat_rows = []
for loc in locations:
    rows = by_loc[loc]
    n1 = sum(1 for r in rows if r.get("MATYP","")=="1")
    n2 = sum(1 for r in rows if r.get("MATYP","")=="2")
    n5 = sum(1 for r in rows if r.get("MATYP","")=="5")
    mat_rows.append([loc, len(rows), n1, n2, n5])

# ── Environment by location ──────────────────────────────────────────────────
env_rows = []
for loc in locations:
    rows = by_loc[loc]
    env_rows.append([loc,
        fmean(col(rows,"MTMINT"),  1),
        fmean(col(rows,"MTMAXT"),  1),
        fmean(col(rows,"SSRADT"),  0),
        fmean(col(rows,"SRAINT"),  0),
        fmean(col(rows,"SUMETT"),  0),
    ])

# ---------------------------------------------------------------------------
CSS = """
  :root { --primary:#1a5276; --accent:#2e86c1; --green:#1e8449;
          --orange:#d35400; --bg:#f8f9fa; --card:#ffffff;
          --border:#dee2e6; --text:#212529; --muted:#6c757d; }
  *{box-sizing:border-box;margin:0;padding:0;}
  body{font-family:"Segoe UI",Arial,sans-serif;font-size:14px;
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
  ul,ol{margin:6px 0 10px 22px;}
  li{margin-bottom:3px;}
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
"""

loc_list = "".join(f"<li>{l}</li>" for l in locations)
scen_list = "".join(f'<span class="badge bg" style="margin:3px;">{m}</span>' for m in managements)
crop_list = "".join(f'<span class="badge bo" style="margin:3px;">{c}</span>' for c in crops)

HTML = f"""<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>SSM Soybean — Simulation Report</title>
<style>{CSS}</style>
</head>
<body>
<nav id="toc">
  <h3>Report Contents</h3>
  <a href="#overview">1. Overview</a>
  <a href="#locations">2. Locations</a>
  <a href="#scenarios">3. Scenarios</a>
  <a href="#yield">4. Yield &amp; Biomass</a>
  <a href="#phenology">5. Phenology</a>
  <a href="#water">6. Water Balance</a>
  <a href="#maturity">7. Maturity Status</a>
  <a href="#outputs">8. Output Files</a>
</nav>
<main>
<h1>SSM Soybean Model — Simulation Report</h1>
<p class="subtitle">Generated: {datetime.now().strftime("%Y-%m-%d %H:%M")} &nbsp;|&nbsp;
Source: <code>{os.path.basename(RESULTS_CSV)}</code></p>

<!-- 1. Overview -->
<h2 id="overview">1. Overview</h2>
<div class="stat-grid">
  <div class="stat-box"><div class="stat-val">{len(scenarios)}</div><div class="stat-lbl">Scenarios</div></div>
  <div class="stat-box"><div class="stat-val">{len(locations)}</div><div class="stat-lbl">Locations</div></div>
  <div class="stat-box"><div class="stat-val">{len(years)}</div><div class="stat-lbl">Years per scenario</div></div>
  <div class="stat-box"><div class="stat-val">{n_rows:,}</div><div class="stat-lbl">Simulation years total</div></div>
  <div class="stat-box"><div class="stat-val">{len(managements)}</div><div class="stat-lbl">Managements</div></div>
  <div class="stat-box"><div class="stat-val">{len(crops)}</div><div class="stat-lbl">Cultivars</div></div>
</div>
<div class="note">
  Year range: <strong>{year_range[0]} – {year_range[1]}</strong>.&nbsp;
  Normal maturity: <strong>{n_normal:,} ({n_normal/n_rows*100:.1f}%)</strong> &nbsp;|&nbsp;
  Premature senescence: <strong>{n_prem:,} ({n_prem/n_rows*100:.1f}%)</strong> &nbsp;|&nbsp;
  Flood kill: <strong>{n_flood:,} ({n_flood/n_rows*100:.1f}%)</strong>.
</div>

<!-- 2. Locations -->
<h2 id="locations">2. Locations Simulated</h2>
<div class="card">
  <p><strong>{len(locations)} locations</strong>, climate years {year_range[0]}–{year_range[1]} ({len(years)} years each).</p>
  <ul>{loc_list}</ul>
</div>
<h3>Mean seasonal environment by location (sowing to maturity)</h3>
{html_table(["Location","Tmin (°C)","Tmax (°C)","SRAD (MJ/m²)","Rain (mm)","ET (mm)"], env_rows)}

<!-- 3. Scenarios -->
<h2 id="scenarios">3. Scenario Structure</h2>
<div class="card">
  <p>Scenarios follow the pattern <strong>Location – Water – Planting window – Cultivar</strong>.</p>
  <ul>
    <li><strong>Water:</strong> RFD (rainfed, water=2) | IRRI (irrigated, water=3, IRGLVL=0.5)</li>
    <li><strong>Planting window:</strong> ELY (early) | MID (mid-season) | LTE (late)</li>
    <li><strong>Cultivar:</strong> check (standard, vpdtp=0) | LT1.5 / LT2 / LT2.5 (limited transpiration, vpdtp=1)</li>
  </ul>
</div>
<h3>Management codes</h3>
<p>{scen_list}</p>
<h3>Cultivars</h3>
<p>{crop_list}</p>

<!-- 4. Yield & Biomass -->
<h2 id="yield">4. Yield and Biomass Summary</h2>
<p>Values show mean ± SD across all scenarios (management types + cultivars) and years for each location.</p>
<h3>By location</h3>
{html_table(["Location","N","Yield (kg/ha)","WTOP (g/m²)","MXLAI","HI","Season days","CTR (mm)","CE (mm)","CRAIN (mm)"], loc_rows)}

<h3>By cultivar type</h3>
<p>Rainfed scenarios only (all locations, all planting windows).</p>
{html_table(["Cultivar","N","Yield (kg/ha)","WTOP (g/m²)","CTR (mm)","HI"], crop_rows)}

<h3>By management (water regime × planting window)</h3>
{html_table(["Water regime","Planting","N","Yield (kg/ha)","CTR (mm)","Mean CIRGW (mm)","Mean IRGNO"], manag_rows)}

<!-- 5. Phenology -->
<h2 id="phenology">5. Phenological Stages — Days After Planting (mean ± SD)</h2>
<p>Averaged across all management types and years per location.
EMR = emergence · R1 = first flower · R5 = beginning seed fill · R7 = physiological maturity · R8 = harvest maturity.</p>
{html_table(["Location","EMR (DAP)","R1 (DAP)","R5 (DAP)","R7 (DAP)","R8 (DAP)"], pheno_rows)}

<!-- 6. Water Balance -->
<h2 id="water">6. Seasonal Water Balance by Location</h2>
<div class="note">Values are means across all years and management types.
CTR = transpiration · CE = soil evaporation · ET = CTR + CE ·
CDRAIN = deep drainage · CRUNOF = runoff · E/ET = evaporation fraction.</div>
{html_table(["Location","CRAIN (mm)","CTR (mm)","CE (mm)","CDRAIN (mm)","CRUNOF (mm)","ET (mm)","E/ET (%)"], water_rows)}

<!-- 7. Maturity Status -->
<h2 id="maturity">7. Maturity Status by Location</h2>
<div class="card">
  <p>
    <span class="badge bg">1 — Normal</span> maturity reached before stop date.&nbsp;
    <span class="badge bo">2 — Premature</span> LAI &lt; 0.05 during grain fill.&nbsp;
    <span class="badge br">5 — Flood kill</span> &gt; FLDKIL consecutive flood days.
  </p>
</div>
{html_table(["Location","Total years","Normal (1)","Premature (2)","Flood kill (5)"], mat_rows)}

<!-- 8. Output Files -->
<h2 id="outputs">8. Output Files Reference</h2>
<div class="card">
  <p><strong>Yearly summary outputs</strong> — one row per simulation year:</p>
  <ul>
    <li><code>outputs/results/all_results.csv</code> — all {n_rows:,} simulation years combined</li>
    {"".join(f"<li><code>outputs/results/{l.replace(' ','_')}_results.csv</code></li>" for l in locations)}
  </ul>
</div>
<div class="card">
  <p><strong>Daily time-step outputs</strong> — one row per simulation day
  (generated when running <code>run_all_scenarios(save_daily=TRUE)</code>):</p>
  <ul>
    <li><code>outputs/results/daily/all_daily.csv</code> — all scenarios combined</li>
    {"".join(f"<li><code>outputs/results/daily/{l.replace(' ','_')}_daily.csv</code></li>" for l in locations)}
  </ul>
  <p class="warn"><strong>Note:</strong> Daily files are large (~80–150 MB combined).
  Re-run <code>08_run_model.R</code> with <code>save_daily=TRUE</code> or
  <code>Rscript 08_run_model.R --daily</code> to generate them.</p>
</div>
<div class="card">
  <p><strong>Parameter reference</strong> — column definitions for all input and output files:</p>
  <ul>
    <li><code>inputs/parameter_dictionary.xlsx</code>
      &nbsp;— Sheet 1: all 112 input parameters &nbsp;|&nbsp;
              Sheet 2: yearly output columns &nbsp;|&nbsp;
              Sheet 3: daily output columns &nbsp;|&nbsp;
              Sheet 4: glossary of abbreviations</li>
  </ul>
</div>

</main>
</body>
</html>"""

with open(OUT_HTML, "w", encoding="utf-8") as f:
    f.write(HTML)

print(f"\nReport written: {OUT_HTML}")
