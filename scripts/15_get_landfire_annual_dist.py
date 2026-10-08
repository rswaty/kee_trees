"""Download LANDFIRE layers for Houghton and Keweenaw counties via the
LANDFIRE Product Service (LFPS):
  - Final Annual Disturbance (Dist), 2010-2025 -> inputs/landfire/annual_disturbance/<layer>/
  - 2020 Biophysical Settings and 2024 Existing Vegetation Type -> inputs/landfire/<layer>/
(LF2025_EVT is rejected for this AOI, so EVT is the 2024 release.)

LFPS rejects multi-layer requests over this AOI, so each layer is its own job.
Job IDs are cached in inputs/landfire/lfps_jobs.csv; rerun to resume polling.
Downloads are unzipped and renamed from the LFPS job id to the layer name
(e.g. LF2024_Dist24.tif, LF2024_Dist24.tif.vat.dbf).

Run from project root: python3 scripts/15_get_landfire_annual_dist.py [email]
"""

import csv
import io
import json
import sys
import time
import urllib.parse
import urllib.request
import zipfile
from pathlib import Path

API = "https://lfps.usgs.gov/api/job"
AOI = "-89.3 46.39 -87.55 47.52"
EMAIL = sys.argv[1] if len(sys.argv) > 1 else "rswaty@tnc.org"
LF_DIR = Path("inputs/landfire")
DIST_LAYERS = [
    "LF2010_Dist10", "LF2012_Dist11", "LF2012_Dist12", "LF2014_Dist13",
    "LF2014_Dist14", "LF2016_Dist15", "LF2016_Dist16", "LF2020_Dist17",
    "LF2020_Dist18", "LF2020_Dist19", "LF2020_Dist20", "LF2022_Dist21",
    "LF2022_Dist22", "LF2023_Dist23", "LF2024_Dist24", "LF2025_Dist25",
]
LAYERS = {l: LF_DIR / "annual_disturbance" / l for l in DIST_LAYERS}
LAYERS.update({l: LF_DIR / l for l in ["LF2020_BPS", "LF2024_EVT"]})

LF_DIR.mkdir(parents=True, exist_ok=True)
jobs_path = LF_DIR / "lfps_jobs.csv"


def get_json(url):
    with urllib.request.urlopen(url, timeout=60) as r:
        return json.load(r)


def unzip_renamed(data, layer, dest):
    dest.mkdir(parents=True, exist_ok=True)
    with zipfile.ZipFile(io.BytesIO(data)) as z:
        for info in z.infolist():
            if info.is_dir():
                continue
            name = Path(info.filename).name
            if name.startswith("j"):  # LFPS job id, e.g. j658e...fe.tif.vat.dbf
                name = layer + name[name.index("."):]
            (dest / name).write_bytes(z.read(info))


jobs = {}
if jobs_path.exists():
    with open(jobs_path) as f:
        jobs = {row[0]: row[1] for row in csv.reader(f) if len(row) == 2}

for layer, dest in LAYERS.items():
    if layer in jobs or dest.exists():
        continue
    q = urllib.parse.urlencode({
        "Layer_List": layer, "Area_of_Interest": AOI,
        "Include_Layer_List_XML_File": "true", "Email": EMAIL,
    })
    jobs[layer] = get_json(f"{API}/submit?{q}")["jobId"]
    with open(jobs_path, "a") as f:
        f.write(f"{layer},{jobs[layer]}\n")

pending = [l for l, dest in LAYERS.items() if not dest.exists()]
while pending:
    for layer in list(pending):
        status = get_json(f"{API}/status?JobId={jobs[layer]}")
        if status["status"] == "Succeeded":
            with urllib.request.urlopen(status["outputFile"], timeout=300) as r:
                unzip_renamed(r.read(), layer, LAYERS[layer])
            print(f"{layer}: downloaded")
            pending.remove(layer)
        elif status["status"] in ("Failed", "Canceled"):
            print(f"{layer}: {status['status']}; delete its row in {jobs_path} and rerun")
            pending.remove(layer)
    if pending:
        print(f"waiting on {len(pending)} job(s)...")
        time.sleep(30)
