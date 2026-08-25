# Keweenaw & Houghton canopy change

Pixel-level explorer for **Houghton and Keweenaw** counties, Michigan:

- **USFS / NLCD tree canopy cover (TCC)** — annual percent canopy, 2010–2025
- **Hansen Global Forest Change `lossyear`** — year of stand-replacing disturbance, 2001–2024

Hansen layers are reprojected onto the TCC 30 m Albers grid.

**Important:** the map patches and acre totals come from Hansen `lossyear`, which flags pixels where Landsat detected a **stand-replacing** disturbance (forest → non-forest). Only pixels with tree cover ≥ 30% in **2010** (NLCD TCC) are included. Disturbance includes harvest, blowdown, insects, fire, and other clearing — **it is not a harvest inventory.**

The mean canopy % chart uses the separate USFS/NLCD Tree Canopy Cover product. It shows the overall trend but does **not** drive the map or acre totals.

## Run the app

From the project root (after processed CSVs exist; map tiles load from R2):

```r
shiny::runApp(".")          # preferred — same entrypoint as shinyapps.io
# or: shiny::runApp("tiles_app")
```

Deploy (overwrites the existing `kee_tree_cover` app / URL):

```r
source("deploy.R")
```

The older Leaflet in-memory explorer is still at `dashboard/` for local comparison only:

```r
shiny::runApp("dashboard")
```

Move the year slider (or press play). Charts update immediately. Hansen orange ramps by year; TCC blue ramps by canopy-drop magnitude.

## Refresh processed layers

```r
# from the project root
source("scripts/01_harmonize.R")
# or: Rscript scripts/01_harmonize.R
```

This writes aligned rasters and county summaries to `data/processed/`.

## Data

| Path | What |
|------|------|
| `data/TCC_Houghton_Keweenaw/HK_TCC_YYYY.tif` | Annual NLCD TCC clips |
| `data/Hansen_Houghton_Keweenaw/` | Original Hansen clips (WGS84) |
| `data/processed/hansen_lossyear.tif` | Hansen lossyear on the TCC grid |
| `data/processed/hansen_treecover2000.tif` | Hansen 2000 canopy on the TCC grid |
| `data/processed/tcc_change_2010_2025.tif` | TCC 2025 − 2010 |
| `data/processed/loss_by_year.rds` | Loss polygons by year for the map (preferred load path) |
| `data/processed/loss_by_year.gpkg` | Same map polygons in GeoPackage form |
| `data/processed/tcc_decline_2010_2025.rds` | TCC drop ≥ 15 pp (2010–2025), dissolved by `drop_pp` magnitude |
| `data/processed/tcc_decline_pp_2010_2025.tif` | Same decline as integer drop (pp) raster |
| `data/processed/tcc_decline_by_drop_pp.csv` | Acres by drop magnitude class |
| `data/tiles/*.pmtiles` | Vector tiles for `tiles_app` (hosted on Cloudflare R2) |
| `data/houghton_keweenaw_counties.*` | County polygons |

Rebuild TCC decline tiles (and optionally upload):

```r
# UPLOAD_R2=1 Rscript scripts/02_rebuild_tcc_decline_tiles.R
Rscript scripts/02_rebuild_tcc_decline_tiles.R
```

### Next: LANDFIRE Historical Disturbance (HDist)

Hansen + TCC still miss some visible clearing (false negatives), with few false positives so far. **LANDFIRE HDist / Annual Dist** would add typed disturbance (harvest, fire, insect, etc.) and year for another independent layer. HDist is often request-only via the LANDFIRE HelpDesk; Annual Dist CONUS downloads are public. Practical path: clip Annual Dist (or an AOI extract) to Houghton/Keweenaw, crosswalk codes, tile like Hansen, toggle in `tiles_app`.

Keep GeoTIFF `.tif` files. ArcGIS sidecars (`.ovr`, `.tfw`, `.vat.dbf`, lock files) are ignored.
