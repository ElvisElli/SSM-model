#!/usr/bin/env python3
"""
download_isimip_data.py
=======================
Downloads ISIMIP3b point CSV files for the SSM soybean VPD analysis.

Uses the public ISIMIP files API (https://files.isimip.org/api/v2/) to
submit point-extraction jobs and download the resulting CSVs.  No API
key is required.  Results are cached locally so re-runs skip completed
downloads.

Data portal:  https://data.isimip.org
Protocol doc: https://github.com/ISI-MIP/isimip-protocol-3
Files API:    https://files.isimip.org/api/v2/

Study locations (SSM soybean project):
  Rohwer, AR   lat=33.8,  lon=-91.82
  Lincoln, NE  lat=40.8,  lon=-96.68

GCMs: GFDL-ESM4, IPSL-CM6A-LR, MPI-ESM1-2-HR, MRI-ESM2-0, UKESM1-0-LL
Scenarios: historical (1985-2014), SSP1-2.6, SSP3-7.0, SSP5-8.5
Variables: tasmax [K], tasmin [K], hurs [%]

Usage:
    python3 download_isimip_data.py

Dependencies:
    pip install requests

Output:
    CSVs saved to  r-model/outputs/isimip_cache/
    Naming:  {gcm_slug}_{variant}_w5e5_{scenario}_{variable}
             _lat{lat}lon{lon}_daily_{yr_start}_{yr_end}.csv
    Format:  YYYY-MM-DD,value   (no header)
"""

import os
import sys
import time
import json
import shutil
import requests
from pathlib import Path

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
    {
        "label":   "GFDL-ESM4",
        "folder":  "GFDL-ESM4",       # folder name on server (CAPS)
        "slug":    "gfdl-esm4",        # NetCDF filename part (lowercase, hyphens)
        "csv_slug":"gfdlesm4",          # ISIMIP portal CSV naming (no hyphens)
        "variant": "r1i1p1f1",
    },
    {
        "label":   "IPSL-CM6A-LR",
        "folder":  "IPSL-CM6A-LR",
        "slug":    "ipsl-cm6a-lr",
        "csv_slug":"ipslcm6alr",
        "variant": "r1i1p1f1",
    },
    {
        "label":   "MPI-ESM1-2-HR",
        "folder":  "MPI-ESM1-2-HR",
        "slug":    "mpi-esm1-2-hr",
        "csv_slug":"mpiesm12hr",
        "variant": "r1i1p1f1",
    },
    {
        "label":   "MRI-ESM2-0",
        "folder":  "MRI-ESM2-0",
        "slug":    "mri-esm2-0",
        "csv_slug":"mriesm20",
        "variant": "r1i1p1f1",
    },
    {
        "label":   "UKESM1-0-LL",
        "folder":  "UKESM1-0-LL",
        "slug":    "ukesm1-0-ll",
        "csv_slug":"ukesm10ll",
        "variant": "r1i1p1f2",
    },
]

# Scenario → list of (yr_start, yr_end) file periods on the ISIMIP server
SCENARIOS = {
    "historical": [
        (1985, 1994), (1995, 2004), (2005, 2014),
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

# Polling interval (seconds) and maximum wait per job
POLL_INTERVAL = 15
JOB_TIMEOUT   = 3600  # 1 hour

# ---------------------------------------------------------------------------
# PATH HELPERS
# ---------------------------------------------------------------------------

def nc_path(gcm, scenario, variable, yr_start, yr_end):
    """Relative path to the global NetCDF file on files.isimip.org."""
    return (
        f"{ISIMIP_BASE_PATH}/{scenario}/{gcm['folder']}/"
        f"{gcm['slug']}_{gcm['variant']}_w5e5_{scenario}_{variable}"
        f"_global_daily_{yr_start}_{yr_end}.nc"
    )


def csv_filename(gcm, scenario, variable, lat, lon, yr_start, yr_end):
    """Local cache filename matching ISIMIP portal naming convention."""
    return (
        f"{gcm['csv_slug']}_{gcm['variant']}_w5e5_{scenario}_{variable}"
        f"_lat{lat:g}lon{lon:g}_daily_{yr_start}_{yr_end}.csv"
    )


# ---------------------------------------------------------------------------
# ISIMIP FILES API
# ---------------------------------------------------------------------------

def submit_point_job(nc_file_path, lat, lon):
    """
    Submit a select_point extraction job.

    POST https://files.isimip.org/api/v2/
    Body:
      {
        "paths": ["ISIMIP3b/.../file.nc"],
        "operations": [{"operation": "select_point",
                        "point": [lat, lon],
                        "output_csv": true}]
      }

    Returns job dict (contains "id" and "status"), or None on failure.
    """
    payload = {
        "paths": [nc_file_path],
        "operations": [
            {
                "operation":  "select_point",
                "point":      [lat, lon],
                "output_csv": True,
            }
        ],
    }
    try:
        r = requests.post(
            ISIMIP_FILES_API + "/",
            json=payload,
            timeout=30,
        )
        if r.status_code in (200, 201):
            return r.json()
        print(f"    [API] POST failed — HTTP {r.status_code}: {r.text[:200]}")
        return None
    except requests.RequestException as exc:
        print(f"    [API] POST error: {exc}")
        return None


def poll_job(job_id):
    """
    Poll a job until finished, failed, or timed out.

    GET https://files.isimip.org/api/v2/{job_id}/

    Returns the final job dict, or None on timeout/error.
    """
    url = f"{ISIMIP_FILES_API}/{job_id}/"
    t0  = time.time()
    while True:
        elapsed = time.time() - t0
        if elapsed > JOB_TIMEOUT:
            print(f"    [API] Job {job_id} timed out after {elapsed:.0f}s")
            return None
        try:
            r = requests.get(url, timeout=30)
            if r.status_code == 200:
                job = r.json()
                status = job.get("status", "unknown")
                if status == "finished":
                    return job
                elif status in ("failed", "error"):
                    print(f"    [API] Job {job_id} failed: {job.get('error', '?')}")
                    return None
                # queued / started → keep polling
                print(f"    [API] Job {job_id}: {status} ({elapsed:.0f}s)", end="\r")
            else:
                print(f"    [API] Poll HTTP {r.status_code}")
        except requests.RequestException as exc:
            print(f"    [API] Poll error: {exc}")
        time.sleep(POLL_INTERVAL)


def download_job_result(job, dest_path):
    """
    Download the CSV produced by a finished job.

    The job dict may contain 'file_url' or 'output_url'; we try both.
    Returns True on success.
    """
    file_url = job.get("file_url") or job.get("output_url")
    if not file_url:
        # Some API versions nest the URL inside a 'files' list
        files = job.get("files", [])
        if files:
            file_url = files[0].get("file_url") or files[0].get("url")
    if not file_url:
        print(f"    [API] No download URL in job: {list(job.keys())}")
        return False
    try:
        r = requests.get(file_url, stream=True, timeout=300)
        if r.status_code == 200:
            with open(dest_path, "wb") as fh:
                shutil.copyfileobj(r.raw, fh)
            return True
        print(f"    [API] Download HTTP {r.status_code}: {file_url}")
        return False
    except requests.RequestException as exc:
        print(f"    [API] Download error: {exc}")
        return False


# ---------------------------------------------------------------------------
# MAIN
# ---------------------------------------------------------------------------

def main():
    # Locate cache directory relative to this script
    script_dir = Path(__file__).parent
    # Go up to r-model root then into outputs/isimip_cache
    base_dir   = script_dir.parent        # r-model/
    cache_dir  = base_dir / "outputs" / "isimip_cache"
    cache_dir.mkdir(parents=True, exist_ok=True)

    print(f"Cache directory: {cache_dir}\n")

    total   = 0
    skipped = 0
    failed  = 0

    for gcm in GCMS:
        for scenario, periods in SCENARIOS.items():
            for (yr_start, yr_end) in periods:
                for variable in VARIABLES:
                    for loc in LOCATIONS:
                        lat = loc["lat"]
                        lon = loc["lon"]

                        fname = csv_filename(gcm, scenario, variable,
                                             lat, lon, yr_start, yr_end)
                        fpath = cache_dir / fname

                        total += 1

                        if fpath.exists():
                            skipped += 1
                            continue  # already cached

                        nc = nc_path(gcm, scenario, variable, yr_start, yr_end)
                        print(f"\n→ {gcm['label']} | {scenario} | {variable}"
                              f" | {yr_start}-{yr_end} | {loc['name']}")
                        print(f"  NC path: {nc}")

                        # Submit extraction job
                        job = submit_point_job(nc, lat, lon)
                        if job is None:
                            failed += 1
                            continue

                        job_id = job.get("id")
                        if not job_id:
                            print(f"  [API] No job ID in response: {job}")
                            failed += 1
                            continue

                        print(f"  Job submitted: {job_id}  (polling every {POLL_INTERVAL}s)")

                        # Check if already finished (fast server)
                        if job.get("status") == "finished":
                            finished_job = job
                        else:
                            finished_job = poll_job(job_id)

                        if finished_job is None:
                            failed += 1
                            continue

                        # Download to a temp file, then rename to expected name
                        tmp_path = fpath.with_suffix(".tmp")
                        ok = download_job_result(finished_job, tmp_path)
                        if ok:
                            tmp_path.rename(fpath)
                            print(f"  Saved: {fname}")
                        else:
                            tmp_path.unlink(missing_ok=True)
                            failed += 1

    print(f"\n{'='*60}")
    print(f"Done.  Total: {total}  |  Skipped (cached): {skipped}"
          f"  |  Failed: {failed}")
    if failed > 0:
        print(
            "\nFor failed files, visit https://data.isimip.org, find the file,\n"
            "select 'Subset > Point', enter the lat/lon, and download the CSV.\n"
            f"Save it to: {cache_dir}\n"
            "with the naming: {gcm_slug}_{variant}_w5e5_{scenario}_{variable}"
            "_lat{lat}lon{lon}_daily_{yr_start}_{yr_end}.csv"
        )


if __name__ == "__main__":
    main()
