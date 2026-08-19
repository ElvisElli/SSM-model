#!/usr/bin/env python3
"""
download_isimip_data.py
=======================
Downloads ISIMIP3b point CSV files for the SSM soybean VPD analysis.

Uses the public ISIMIP files API (https://files.isimip.org/api/v2/) to
submit select_point jobs; results arrive as ZIP archives containing CSVs.
No API key required.  Results are cached so re-runs skip completed files.

Downloads are parallelised across MAX_WORKERS threads so all jobs run
concurrently rather than one-at-a-time; typical wall-clock is ~30-60 min
for the full 930-file dataset.

Data portal:  https://data.isimip.org
Files API:    https://files.isimip.org/api/v2/

Actual CSV naming inside the ZIP (verified from API):
  {gcm_slug}_{variant}_w5e5_{scenario}_{variable}_lon{lon}lat{lat}_daily_{yr_start}_{yr_end}.csv
  e.g. gfdl-esm4_r1i1p1f1_w5e5_ssp370_tasmax_lon-91.82lat33.8_daily_2041_2050.csv

Study locations (SSM soybean project):
  Rohwer, AR   lat=33.8,  lon=-91.82
  Lincoln, NE  lat=40.8,  lon=-96.68

Usage:
    python3 download_isimip_data.py

Dependencies:
    pip install requests

Output:
    CSVs saved to  r-model/outputs/isimip_cache/
    Format:  YYYY-MM-DD,value   (no header, Kelvin for temperature)
"""

import io
import time
import zipfile
import threading
import requests
from pathlib import Path
from concurrent.futures import ThreadPoolExecutor, as_completed

# ---------------------------------------------------------------------------
# CONFIGURATION
# ---------------------------------------------------------------------------

ISIMIP_FILES_API = "https://files.isimip.org/api/v2"
ISIMIP_BASE_PATH = (
    "ISIMIP3b/InputData/climate/atmosphere/bias-adjusted/global/daily"
)

# Locations to extract
LOCATIONS = [
    {"name": "Rohwer_AR",  "lat": 33.8,  "lon": -91.82},
    {"name": "Lincoln_NE", "lat": 40.8,  "lon": -96.68},
]

# GCM table
GCMS = [
    {"label": "GFDL-ESM4",     "folder": "GFDL-ESM4",     "slug": "gfdl-esm4",     "variant": "r1i1p1f1"},
    {"label": "IPSL-CM6A-LR",  "folder": "IPSL-CM6A-LR",  "slug": "ipsl-cm6a-lr",  "variant": "r1i1p1f1"},
    {"label": "MPI-ESM1-2-HR", "folder": "MPI-ESM1-2-HR", "slug": "mpi-esm1-2-hr", "variant": "r1i1p1f1"},
    {"label": "MRI-ESM2-0",    "folder": "MRI-ESM2-0",    "slug": "mri-esm2-0",    "variant": "r1i1p1f1"},
    {"label": "UKESM1-0-LL",   "folder": "UKESM1-0-LL",   "slug": "ukesm1-0-ll",   "variant": "r1i1p1f2"},
]

# Scenario → list of (yr_start, yr_end) file periods on the ISIMIP server.
# Historical uses decade-aligned boundaries verified against the API;
# the last historical file covers only 2011-2014.
SCENARIOS = {
    "historical": [
        (1981, 1990), (1991, 2000), (2001, 2010), (2011, 2014),
    ],
    "ssp126": [
        (2015, 2020), (2021, 2030), (2031, 2040), (2041, 2050),
        (2051, 2060), (2061, 2070), (2071, 2080), (2081, 2090), (2091, 2100),
    ],
    "ssp370": [
        (2015, 2020), (2021, 2030), (2031, 2040), (2041, 2050),
        (2051, 2060), (2061, 2070), (2071, 2080), (2081, 2090), (2091, 2100),
    ],
    "ssp585": [
        (2015, 2020), (2021, 2030), (2031, 2040), (2041, 2050),
        (2051, 2060), (2061, 2070), (2071, 2080), (2081, 2090), (2091, 2100),
    ],
}

VARIABLES = ["tasmax", "tasmin", "hurs"]

POLL_INTERVAL = 10    # seconds between status checks
JOB_TIMEOUT   = 3600  # 1 hour max wait per job
MAX_WORKERS   = 20    # parallel download threads


# ---------------------------------------------------------------------------
# NAMING HELPERS  (verified against actual API output)
# ---------------------------------------------------------------------------

def nc_path(gcm, scenario, variable, yr_start, yr_end):
    """Relative path to global NetCDF on files.isimip.org."""
    return (
        f"{ISIMIP_BASE_PATH}/{scenario}/{gcm['folder']}/"
        f"{gcm['slug']}_{gcm['variant']}_w5e5_{scenario}_{variable}"
        f"_global_daily_{yr_start}_{yr_end}.nc"
    )


def expected_csv_name(gcm, scenario, variable, lat, lon, yr_start, yr_end):
    """
    Filename the ISIMIP API returns inside the ZIP (verified empirically):
      {slug}_{variant}_w5e5_{scenario}_{variable}_lon{lon}lat{lat}_daily_{yr_start}_{yr_end}.csv
    Note: lon BEFORE lat; slug uses hyphens (e.g. 'gfdl-esm4', not 'gfdlesm4').
    """
    return (
        f"{gcm['slug']}_{gcm['variant']}_w5e5_{scenario}_{variable}"
        f"_lon{lon}lat{lat}_daily_{yr_start}_{yr_end}.csv"
    )


# ---------------------------------------------------------------------------
# ISIMIP FILES API  (thread-safe — each call creates its own session)
# ---------------------------------------------------------------------------

_print_lock = threading.Lock()

def tprint(*args, **kwargs):
    """Thread-safe print."""
    with _print_lock:
        print(*args, **kwargs)


def submit_job(nc_file_path, lat, lon):
    """POST a select_point extraction job. Returns job dict or None."""
    payload = {
        "paths": [nc_file_path],
        "operations": [
            {"operation": "select_point", "point": [lat, lon], "output_csv": True}
        ],
    }
    try:
        r = requests.post(ISIMIP_FILES_API + "/", json=payload, timeout=60)
        if r.status_code in (200, 201):
            return r.json()
        tprint(f"  [POST 400] {nc_file_path.split('/')[-1]}: {r.text[:150]}")
        return None
    except requests.RequestException as exc:
        tprint(f"  [POST ERR] {exc}")
        return None


def poll_until_done(job_url, label=""):
    """Poll job_url until finished or timed out. Returns finished job or None."""
    t0 = time.time()
    while True:
        elapsed = time.time() - t0
        if elapsed > JOB_TIMEOUT:
            tprint(f"  [TIMEOUT] {label} after {elapsed:.0f}s")
            return None
        try:
            r = requests.get(job_url, timeout=30)
            if r.status_code == 200 and r.content:
                job = r.json()
                status = job.get("status", "unknown")
                if status == "finished":
                    return job
                if status in ("failed", "error"):
                    tprint(f"  [FAILED] {label}: {job.get('error', '?')}")
                    return None
            # still running — wait and retry
        except requests.RequestException:
            pass
        time.sleep(POLL_INTERVAL)


def download_and_extract(job, expected_name, dest_path):
    """Download ZIP from finished job, extract target CSV. Returns True on success."""
    file_url = job.get("file_url")
    if not file_url:
        tprint(f"  [NO URL] {expected_name}")
        return False
    try:
        r = requests.get(file_url, timeout=300)
        if r.status_code != 200:
            tprint(f"  [ZIP HTTP {r.status_code}] {expected_name}")
            return False
        z = zipfile.ZipFile(io.BytesIO(r.content))
        csv_names = [n for n in z.namelist() if n.endswith(".csv")]
        if not csv_names:
            tprint(f"  [NO CSV] {z.namelist()}")
            return False
        target = expected_name if expected_name in csv_names else csv_names[0]
        if target != expected_name:
            tprint(f"  [NAME MISMATCH] expected '{expected_name}' got '{target}'")
        with open(dest_path, "wb") as fh:
            fh.write(z.read(target))
        return True
    except (requests.RequestException, zipfile.BadZipFile) as exc:
        tprint(f"  [DL ERR] {expected_name}: {exc}")
        return False


# ---------------------------------------------------------------------------
# WORKER  (one per file)
# ---------------------------------------------------------------------------

def process_file(task):
    """Submit → poll → download one ISIMIP CSV. Returns (fname, status_str)."""
    gcm, scenario, variable, yr_start, yr_end, lat, lon, cache_dir = task

    fname = expected_csv_name(gcm, scenario, variable, lat, lon, yr_start, yr_end)
    fpath = cache_dir / fname

    if fpath.exists():
        return fname, "cached"

    nc = nc_path(gcm, scenario, variable, yr_start, yr_end)
    job = submit_job(nc, lat, lon)
    if job is None:
        return fname, "submit_failed"

    job_url = job.get("job_url", f"{ISIMIP_FILES_API}/{job['id']}")
    status  = job.get("status", "queued")

    if status != "finished":
        job = poll_until_done(job_url, label=fname[:50])
    if job is None:
        return fname, "poll_failed"

    ok = download_and_extract(job, fname, fpath)
    if not ok:
        fpath.unlink(missing_ok=True)
        return fname, "dl_failed"

    return fname, "saved"


# ---------------------------------------------------------------------------
# MAIN
# ---------------------------------------------------------------------------

def main():
    script_dir = Path(__file__).parent
    base_dir   = script_dir.parent        # r-model/
    cache_dir  = base_dir / "outputs" / "isimip_cache"
    cache_dir.mkdir(parents=True, exist_ok=True)

    print(f"Cache directory: {cache_dir}\n")

    # Build full work list
    tasks = []
    for gcm in GCMS:
        for scenario, periods in SCENARIOS.items():
            for (yr_start, yr_end) in periods:
                for variable in VARIABLES:
                    for loc in LOCATIONS:
                        tasks.append((
                            gcm, scenario, variable, yr_start, yr_end,
                            loc["lat"], loc["lon"], cache_dir
                        ))

    total = len(tasks)
    print(f"Total files: {total}  |  Workers: {MAX_WORKERS}\n")

    counts = {"cached": 0, "saved": 0,
              "submit_failed": 0, "poll_failed": 0, "dl_failed": 0}
    done = 0

    with ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
        futures = {executor.submit(process_file, t): t for t in tasks}
        for fut in as_completed(futures):
            fname, result = fut.result()
            counts[result] = counts.get(result, 0) + 1
            done += 1
            if result == "saved":
                tprint(f"  [{done:3d}/{total}] ✓ {fname}")
            elif result != "cached":
                tprint(f"  [{done:3d}/{total}] ✗ {result}: {fname}")
            elif done % 50 == 0:
                tprint(f"  [{done:3d}/{total}]   {counts['cached']} cached so far…")

    failed = counts.get("submit_failed", 0) + counts.get("poll_failed", 0) + counts.get("dl_failed", 0)
    print(f"\n{'='*60}")
    print(f"Done.  Total={total}  Cached={counts['cached']}"
          f"  Saved={counts['saved']}  Failed={failed}")
    if failed:
        print(
            f"\nFor failed files, visit https://data.isimip.org,\n"
            "find the file, use Subset > Point, and save the CSV to:\n"
            f"  {cache_dir}\n"
            "Expected name format:\n"
            "  {slug}_{variant}_w5e5_{scenario}_{variable}"
            "_lon{lon}lat{lat}_daily_{yr_start}_{yr_end}.csv"
        )


if __name__ == "__main__":
    main()
