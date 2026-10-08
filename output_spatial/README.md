# Spatial data for QGIS / ArcGIS

Every layer below opens directly in QGIS or ArcGIS: GeoTIFFs, shapefiles and GeoPackages, all with
their coordinate system embedded. QGIS reprojects on the fly, so layers in different systems
line up. Rasters with a `.qml` file next to them open with the same colours and labels as the slides.

**Analysis grid:** the USFS Tree Canopy Cover grid (30 m, Albers Conical Equal Area). Every 30 m
cell is 0.2224 acres. LANDFIRE rasters are on LANDFIRE's own 30 m Albers grid.

## Start here: the sale story

| File | What it is |
|:---|:---|
| `cfp_sale_story.gpkg` → `cfp_groups_2020_2026` | CFP land by 2020 → 2026 owner group (e.g. "TRG → Verdant"), dissolved; fields `group`, `color`, `acres` |
| `cfp_sale_story.gpkg` → `cfp_parcels_2020` | 2020 CFP parcels with `group`, owner names 2020/2026, `mean_tcc_2020`, `mean_tcc_2025`, `delta_pp` (canopy change, points), `major_disturbance` (fell > 20 points). Parcels under 20 acres were left out of the analysis and have no group |
| `cfp_sale_story.gpkg` → `keweenaw_heartlands`, `american_songbird`, `counties` | Sale boundaries and counties |
| `cfp_sale_zones.tif` (+ `.qml`) | Same owner groups as a 30 m raster; codes in `output_csvs/cfp_sale_zone_labels.csv` |
| `drop_year_heartlands.tif`, `drop_year_songbird.tif` (+ `.qml`) | Year the canopy dropped. Band 1 `tcc_drop_year`: first year canopy fell 15+ points below its previous-3-year high. Band 2 `hansen_loss_year`: Hansen loss year (through 2024). The style shows band 1; switch the band in Layer Properties for Hansen |
| `tcc_change_2020_2025.tif` (+ `.qml`) | Canopy cover 2025 minus 2020, in points (brown = lost, green = gained) |
| `tcc_change_2010_2025.tif` (+ `.qml`) | Same, 2010 → 2025 |

## Other derived layers (Shiny app pipeline)

| File | What it is |
|:---|:---|
| `hansen_lossyear.tif` | Hansen loss year on the canopy grid (0 = no loss, 1–24 = 2001–2024) |
| `hansen_treecover2000.tif` | Hansen 2000 tree cover (%) on the canopy grid |
| `tcc_decline_pp_2010_2025.tif`, `tcc_decline_2010_2025.gpkg` | Canopy drops of 15+ points, 2010 → 2025, as a raster (points) and polygons |
| `loss_by_year.gpkg` | Hansen loss patches by year (polygons) |
| `landfire_fdist_agent.tif`, `landfire_fdist_by_agent.gpkg` | Older LANDFIRE disturbance (FDist) grouped by agent; labels in `output_csvs/landfire_fdist_by_agent.csv` |
| `cfp_owner_2020.tif`, `cfp_owner_2026.tif`, `cfp_owner_*.gpkg` | CFP owner type; codes in `output_csvs/cfp_owner_labels.csv` |
| `cfp_name_2020.tif`, `cfp_name_2026.tif` | CFP owner name; codes in `output_csvs/cfp_name_labels.csv` |
| `cfp_owner_change_pair.tif`, `cfp_owner_change.gpkg` | Owner type 2020 → 2026 pairs |
| `counties.gpkg` | Houghton and Keweenaw counties |
| `tiles/*.pmtiles` | Vector tiles for the web map (QGIS 3.32+ can open these too) |

## Source data (in `inputs/`)

| Folder | What it is |
|:---|:---|
| `inputs/boundaries/` | Counties, Keweenaw Heartlands and American Songbird (17,315 acres) boundaries |
| `inputs/cfp/` | Michigan DNR Commercial Forest parcels, 2020 and 2026 |
| `inputs/tcc/` | USFS Tree Canopy Cover, 2010–2025 (254/255 = no data) |
| `inputs/hansen/` | Hansen Global Forest Change clips (WGS 84) |
| `inputs/landfire/annual_disturbance/LFyyyy_DistNN/` | LANDFIRE Annual Disturbance, one folder per year; class names (type, severity) are in the attribute table (`.vat.dbf`), which QGIS reads |
| `inputs/landfire/LF2020_BPS/`, `LF2024_EVT/` | LANDFIRE Biophysical Settings and Existing Vegetation Type, with attribute tables |
| `inputs/landfire/lf_hist_dist.tif` | Older LANDFIRE Historical Disturbance (FDist) |
