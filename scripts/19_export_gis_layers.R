# Export the sale-story layers for QGIS / ArcGIS, and write QGIS style files (.qml)
# next to the rasters so they open with the same colours and labels as the slides.
#
# Writes to output_spatial/:
#   cfp_sale_story.gpkg  layers: counties, keweenaw_heartlands, american_songbird,
#                        cfp_groups_2020_2026 (owner groups, dissolved),
#                        cfp_parcels_2020 (2020 parcels with group, owners, canopy change)
#   tcc_change_2020_2025.tif  canopy cover 2025 minus 2020 (points)
#   cfp_sale_zones.qml, drop_year_heartlands.qml, drop_year_songbird.qml,
#   tcc_change_2020_2025.qml, tcc_change_2010_2025.qml
#
# Needs scripts/10, 11 and 17 to have run first.
# Run from project root: Rscript scripts/19_export_gis_layers.R

Sys.setenv(PROJ_NETWORK = "OFF")
suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(terra)
})
terraOptions(progress = 0)

root <- if (dir.exists("output_csvs") && dir.exists("inputs")) "." else
  stop("Run from the kee_trees project root.")
csv_dir <- file.path(root, "output_csvs")
gis_dir <- file.path(root, "output_spatial")
gpkg <- file.path(gis_dir, "cfp_sale_story.gpkg")

# Same wording and colours as the slides (scripts/charts/00_setup.R).
GROUPS <- data.frame(
  zone = c("TRG/Verdant retained", "Heartlands (TRG to TNC)", "Songbird (TRG to Songbird, 2026)",
           "TRG other sale / left CFP", "MWF", "Lyme", "Non-institutional kept", "Non-institutional sold / left"),
  group = c("TRG → Verdant", "TRG → The Nature Conservancy (Heartlands)", "TRG → Verdant → American Songbird (2026)",
            "TRG → other owners / left CFP", "Molpus → Molpus (mostly kept)", "Lyme → Lyme",
            "Other private → same owner", "Other private → new owner / left CFP"),
  color = c("#7570b3", "#1b9e77", "#d95f02", "#56b4e9", "#e7298a", "#f0c419", "#d2b48c", "#5c3a1e")
)

zones <- rast(file.path(gis_dir, "cfp_sale_zones.tif"))
labels <- read.csv(file.path(csv_dir, "cfp_sale_zone_labels.csv")) |> left_join(GROUPS, by = "zone")
crs_ref <- crs(zones)

read_in <- function(p) st_transform(st_read(file.path(root, "inputs", p), quiet = TRUE), crs_ref)
write_layer <- function(x, layer) st_write(x, gpkg, layer = layer, delete_layer = TRUE, quiet = TRUE)

if (file.exists(gpkg)) file.remove(gpkg)
write_layer(read_in("boundaries/houghton_keweenaw_counties.shp"), "counties")
write_layer(read_in("boundaries/keweenaw_heartlands.shp"), "keweenaw_heartlands")
write_layer(read_in("boundaries/american_songbird.shp"), "american_songbird")

# ---- Owner groups, dissolved from the zone raster ----
groups_sf <- st_as_sf(as.polygons(subst(zones, 0, NA), dissolve = TRUE)) |>
  left_join(labels, by = "zone_code") |>
  mutate(acres = as.numeric(st_area(geometry)) / 4046.8564224) |>
  select(zone_code, group, zone, color, acres)
write_layer(groups_sf, "cfp_groups_2020_2026")

# ---- 2020 parcels with group, owners and canopy change ----
parcel_attrs <- read.csv(file.path(csv_dir, "cfp_institutional_sellers_keepers_parcels.csv"), stringsAsFactors = FALSE) |>
  mutate(parid = as.character(parid)) |>
  select(parid, institutional_firm, name_2020, name_2026, owner_type_2020, owner_type_2026,
         mean_tcc_2020, mean_tcc_2025, delta_pp, major_disturbance, exit_bucket, cohort_primary)
parcels <- read_in("cfp/cfp_hk_2020.shp") |>
  st_make_valid() |>
  transmute(parid = as.character(parid), county = County_Nam, acres = as.numeric(acres)) |>
  left_join(parcel_attrs, by = "parid")
modal_zone <- terra::extract(zones, vect(parcels), fun = function(v, ...) {
  v <- v[!is.na(v) & v > 0]
  if (length(v)) as.integer(names(which.max(table(v)))) else NA_integer_
})
parcels$zone_code <- modal_zone[[2]]
parcels <- parcels |>
  left_join(labels |> select(zone_code, group), by = "zone_code") |>
  relocate(group, .after = acres)
write_layer(parcels, "cfp_parcels_2020")

# ---- Canopy change 2020 -> 2025 ----
read_tcc <- function(y) subst(rast(file.path(root, "inputs/tcc", sprintf("HK_TCC_%d.tif", y))), c(254, 255), NA)
chg <- read_tcc(2025) - read_tcc(2020)
names(chg) <- "tcc_change_pp"
writeRaster(chg, file.path(gis_dir, "tcc_change_2020_2025.tif"), overwrite = TRUE,
            wopt = list(datatype = "INT2S", NAflag = -32768, gdal = c("COMPRESS=DEFLATE")))

# ---- QGIS styles ----
qml <- function(renderer) {
  c("<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>",
    "<qgis version=\"3.28\" styleCategories=\"Symbology\">", "  <pipe>", renderer, "  </pipe>", "</qgis>")
}
paletted <- function(values, colors, labels_txt, band = 1) {
  esc <- function(x) gsub("&", "&amp;", gsub("\"", "&quot;", x))
  c(sprintf("    <rasterrenderer type=\"paletted\" band=\"%d\" opacity=\"1\" alphaBand=\"-1\">", band),
    "      <colorPalette>",
    sprintf("        <paletteEntry value=\"%s\" color=\"%s\" alpha=\"255\" label=\"%s\"/>", values, colors, esc(labels_txt)),
    "      </colorPalette>", "    </rasterrenderer>")
}
diverging <- function(lo, hi) {
  c(sprintf("    <rasterrenderer type=\"singlebandpseudocolor\" band=\"1\" opacity=\"1\" alphaBand=\"-1\" classificationMin=\"%d\" classificationMax=\"%d\">", lo, hi),
    "      <rastershader>",
    sprintf("        <colorrampshader colorRampType=\"INTERPOLATED\" classificationMode=\"1\" clip=\"0\" minimumValue=\"%d\" maximumValue=\"%d\">", lo, hi),
    sprintf("          <item value=\"%d\" color=\"#8c510a\" alpha=\"255\" label=\"%d (canopy lost)\"/>", lo, lo),
    "          <item value=\"0\" color=\"#f6f6f6\" alpha=\"255\" label=\"0\"/>",
    sprintf("          <item value=\"%d\" color=\"#01665e\" alpha=\"255\" label=\"+%d (canopy gained)\"/>", hi, hi),
    "        </colorrampshader>", "      </rastershader>", "    </rasterrenderer>")
}

writeLines(qml(paletted(labels$zone_code, labels$color, labels$group)), file.path(gis_dir, "cfp_sale_zones.qml"))
for (f in c("tcc_change_2020_2025", "tcc_change_2010_2025")) {
  writeLines(qml(diverging(-60, 40)), file.path(gis_dir, paste0(f, ".qml")))
}
# Year ramp on band 1 (USFS canopy drop year); switch to band 2 for Hansen loss year.
yrs <- 2011:2025
yr_cols <- substr(viridisLite::turbo(length(yrs), begin = 0.05, end = 0.95), 1, 7)
for (f in c("drop_year_heartlands", "drop_year_songbird")) {
  writeLines(qml(paletted(yrs, yr_cols, yrs)), file.path(gis_dir, paste0(f, ".qml")))
}

message("Wrote ", gpkg, " (", paste(st_layers(gpkg)$name, collapse = ", "), "), tcc_change_2020_2025.tif and .qml styles")
