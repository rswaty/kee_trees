# Keweenaw & Houghton canopy change

Tree canopy change, ownership change and disturbance on Michigan Commercial Forest Program (CFP)
land and all land in **Houghton and Keweenaw** counties. Randy Swaty and Julia Petersen.

## Folders

| Folder | What's in it |
|:---|:---|
| `inputs/` | Source data, never written by scripts: boundaries, CFP parcels, USFS canopy cover, Hansen, LANDFIRE, BpS model tables |
| `scripts/` | Analysis scripts, numbered in run order (see below) |
| `scripts/charts/` | One script per presentation chart, for PowerPoint |
| `output_csvs/` | Every table the analysis writes |
| `output_spatial/` | Every map layer the analysis writes (GeoTIFF, GeoPackage); see `output_spatial/README.md` for a QGIS guide |
| `output_visuals/` | Figures written by the analysis scripts |
| `output_visuals/powerpoint/` | Charts for PowerPoint: PNGs, one-slide editable decks, and `all_charts.pptx` |
| `docs/` | Quarto reports and the revealjs slide deck |
| `reference/` | Review comments, notes, example spreadsheet |
| `archive/` | Superseded material (old Leaflet dashboard, R-only map objects, map test pages); safe to delete |
| `app.R`, `tiles_app/`, `www/`, `deploy.R`, `rsconnect/` | The Shiny map app (must stay at the top level) |

## Charts for PowerPoint

```r
# from the project root (or open kee_trees.Rproj)
source("scripts/charts/make_all_charts.R")        # all charts + all_charts.pptx
source("scripts/charts/03_canopy_by_year.R")      # or just one chart
```

Fonts, sizes, group labels, colours and an optional PowerPoint template are set at the top of
`scripts/charts/00_setup.R`. Each chart is written to `output_visuals/powerpoint/` as a 300 dpi PNG
and a one-slide `.pptx` in which the chart is made of native PowerPoint shapes (right-click >
Group > Ungroup to edit text and colours). Maps are inserted as pictures. Tables are also written as CSV.

## Analysis scripts

Run from the project root, e.g. `Rscript scripts/13_footprint_tcc_bins.R`.

| Script | Does |
|:---|:---|
| `01_harmonize.R` | Puts Hansen on the canopy grid; county loss and canopy tables |
| `02`–`05` | Canopy-decline, LANDFIRE FDist and CFP ownership layers and tiles for the Shiny app |
| `06`–`09` | CFP canopy and disturbance by owner change; parcel canopy change 2020 → 2025 |
| `10_institutional_sellers_vs_keepers.R` | Sellers vs keepers among institutional owners |
| `11_sale_footprint_pixels.R` | Owner-group zones (incl. Heartlands and Songbird) and Hansen loss by year |
| `12_sale_footprint_landfire_tcc.R` | Canopy cover by year per group (and older LANDFIRE FDist) |
| `13_footprint_tcc_bins.R` | Acres per 10% canopy bin, Heartlands and Songbird |
| `14_footprint_landfire_bins.R` | Years-since-disturbance bins (older LANDFIRE FDist) |
| `15_get_landfire_annual_dist.py` | Downloads LANDFIRE Annual Disturbance 2010–2025, BpS and EVT |
| `16_landfire_annual_dist.R` | LANDFIRE annual disturbance by group and type |
| `17_footprint_drop_year_maps.R` | Year-the-canopy-dropped rasters (canopy and Hansen) and their agreement |
| `18_bps_evt_disturbance_regimes.R` | BpS, EVT, historical vs current disturbance, all land |
| `19_export_gis_layers.R` | Sale-story GeoPackage, canopy change raster and QGIS styles |

The Quarto documents in `docs/` read from `output_csvs/`, `output_spatial/` and `output_visuals/`.
Render with `quarto render docs/cfp_sale_story_slides.qmd`.

## Shiny app

Pixel-level explorer: USFS/NLCD tree canopy cover (2010–2025) and Hansen `lossyear` (2001–2024),
plus CFP ownership. Hansen flags **stand-replacing** disturbance only, on pixels with ≥ 30% canopy in
2010; it is not a harvest inventory.

```r
shiny::runApp(".")      # local
source("deploy.R")      # deploy to shinyapps.io (overwrites kee_tree_cover)
```

The app reads the CSVs listed in `deploy.R` from `output_csvs/` and PMTiles from `www/tiles/`
(also served from GitHub and Cloudflare R2). Rebuild tiles with scripts `02`–`05`
(`UPLOAD_R2=1 Rscript scripts/02_rebuild_tcc_decline_tiles.R` to upload).

GeoTIFFs keep their CRS; ArcGIS sidecars are git-ignored except LANDFIRE attribute tables
(`.vat.dbf`), which hold the class names.
