# Keweenaw & Houghton canopy change

Pixel-level explorer for **Houghton and Keweenaw** counties, Michigan:

- **USFS / NLCD tree canopy cover (TCC)** — annual percent canopy, 2010–2025
- **Hansen Global Forest Change `lossyear`** — year of stand-replacing disturbance, 2001–2024

Hansen layers are reprojected onto the TCC 30 m Albers grid. Loss is **disturbance**, not a harvest inventory.

## Run the dashboard

From the project root (after processed data exist):

```r
shiny::runApp("dashboard")
```

Move the year slider (or press the small play triangle on the slider). Charts update immediately. The map shows disturbance **from 2010 through that year** (gold = earlier, cyan = this year). Switch the background to satellite, dark, or light.

## Refresh processed layers

```r
# from the project root
source("R/01_harmonize.R")
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
| `data/processed/loss_by_year.gpkg` | Loss polygons dissolved by year (map) |
| `data/houghton_keweenaw_counties.*` | County polygons |

Keep GeoTIFF `.tif` files. ArcGIS sidecars (`.ovr`, `.tfw`, `.vat.dbf`, lock files) are ignored.
