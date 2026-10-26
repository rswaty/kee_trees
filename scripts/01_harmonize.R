# Line up Hansen Global Forest Change data with the tree canopy cover (TCC) grid
# and write the county-level summary tables and map layers used by the dashboard.
# Run from the project root: Rscript scripts/01_harmonize.R
# NOTE: Keep this outside an app-root R/ folder — shiny::runApp(".") auto-sources R/*.R.

# Stop the PROJ library from trying to download projection files from the internet.
Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
  library(tidyr)
})

# Show progress bars for slow raster operations.
terraOptions(progress = 1)

# ---- Folders and settings ----
# All paths are relative to the project folder, so stop early if run from elsewhere.
root <- if (dir.exists("inputs/tcc")) {
  "."
} else {
  stop("Run this script from the kee_trees project root.")
}

tcc_dir <- file.path(root, "inputs/tcc")
hansen_dir <- file.path(root, "inputs/hansen")
csv_dir <- file.path(root, "output_csvs")
gis_dir <- file.path(root, "output_spatial")
for (d in c(csv_dir, gis_dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# Compression settings for output GeoTIFFs (smaller files, no loss of information).
gdal_opts <- c("COMPRESS=DEFLATE", "ZLEVEL=9", "TILED=YES")

# ---- Load the TCC grid and county boundaries ----
# The first yearly TCC raster serves as the "template": every other layer is
# reprojected onto its 30 m cells so all layers line up cell for cell.
message("Loading TCC template and counties...")
tcc_files <- sort(list.files(tcc_dir, pattern = "^HK_TCC_[0-9]{4}\\.tif$", full.names = TRUE))
stopifnot(length(tcc_files) > 0)
tcc_template <- rast(tcc_files[[1]])

# Read Houghton and Keweenaw county outlines, put them in the TCC map projection,
# and give each county a simple number (1, 2) for use in raster summaries.
counties <- st_read(file.path(root, "inputs/boundaries/houghton_keweenaw_counties.shp"), quiet = TRUE) |>
  dplyr::select(COUNTY, FIPS, AREA_SQMI)
counties <- st_transform(counties, crs(tcc_template))
counties_v <- vect(counties)
counties_v$county_id <- seq_len(nrow(counties_v))

st_write(counties, file.path(gis_dir, "counties.gpkg"), delete_dsn = TRUE, quiet = TRUE)

# ---- Put Hansen layers on the TCC grid ----
# Hansen "lossyear" gives the year a cell lost tree cover; "treecover2000" is
# percent canopy in 2000. Nearest-neighbour resampling keeps the original values
# (no averaging), which matters because loss year is a code, not a quantity.
message("Reprojecting Hansen lossyear and treecover2000 onto the TCC grid...")
loss_src <- rast(file.path(hansen_dir, "HK_Loss_2024.tif"))
cover_src <- rast(file.path(hansen_dir, "HK_Cover_2024.tif"))

loss <- project(loss_src, tcc_template, method = "near")
cover <- project(cover_src, tcc_template, method = "near")
names(loss) <- "lossyear"
names(cover) <- "treecover2000"

# Mask to the two counties so water / bbox padding drops out of summaries.
county_mask <- rasterize(counties_v, tcc_template, field = 1)
loss <- mask(loss, county_mask)
cover <- mask(cover, county_mask)

# Save as whole numbers 0-255 (INT1U), which is all these layers need.
writeRaster(loss, file.path(gis_dir, "hansen_lossyear.tif"),
            overwrite = TRUE, wopt = list(datatype = "INT1U", gdal = gdal_opts))
writeRaster(cover, file.path(gis_dir, "hansen_treecover2000.tif"),
            overwrite = TRUE, wopt = list(datatype = "INT1U", gdal = gdal_opts))

# ---- TCC change, 2010 to 2025 ----
# TCC values 254 and 255 are no-data codes (for example, water or outside the
# mapped area), so they are set to missing before any arithmetic.
message("Building TCC change (2025 - 2010)...")
tcc_2010 <- rast(file.path(tcc_dir, "HK_TCC_2010.tif"))
tcc_2025 <- rast(file.path(tcc_dir, "HK_TCC_2025.tif"))
tcc_2010 <- subst(tcc_2010, c(254, 255), NA)
tcc_2025 <- subst(tcc_2025, c(254, 255), NA)
tcc_2010 <- mask(tcc_2010, county_mask)
tcc_2025 <- mask(tcc_2025, county_mask)
# Change in percentage points; negative means canopy was lost.
tcc_change <- tcc_2025 - tcc_2010
names(tcc_change) <- "tcc_change_2010_2025"
# Stored as signed integers (INT2S) because change can be negative.
writeRaster(tcc_change, file.path(gis_dir, "tcc_change_2010_2025.tif"),
            overwrite = TRUE, wopt = list(datatype = "INT2S", gdal = gdal_opts))

# Acres per cell: cell area in square metres (30 m x 30 m = 900) divided by
# 4046.856 square metres per acre, about 0.22 acres per cell.
px_acres <- prod(res(tcc_template)) / 4046.8564224
# Forest mask matches the analysis start year: NLCD TCC 2010 >= 30%.
forest <- tcc_2010 >= 30
# A raster of county numbers, plus a small table to turn numbers back into names.
county_id <- rasterize(counties_v, tcc_template, field = "county_id")
id_lookup <- data.frame(
  county_id = counties_v$county_id,
  county = counties_v$COUNTY
)

# ---- Hansen loss acres by county and year ----
# Keep loss only on cells that were forest in 2010. Hansen loss year 0 means
# "no loss", so it is dropped too.
message("Summarizing Hansen loss by county and year...")
loss_forest <- ifel(forest, loss, NA)
loss_forest <- ifel(loss_forest == 0, NA, loss_forest)
# Count cells for every combination of loss year and county.
ct <- terra::crosstab(c(loss_forest, county_id), long = TRUE, useNA = FALSE)
names(ct) <- c("lossyear", "county_id", "n_pixels")
# Hansen codes years as 1 = 2001 ... 24 = 2024, so add 2000 to get the calendar year.
loss_stats <- ct |>
  mutate(
    year = as.integer(lossyear) + 2000,
    acres = n_pixels * px_acres
  ) |>
  left_join(id_lookup, by = "county_id") |>
  filter(year >= 2001, year <= 2024, !is.na(county)) |>
  select(year, county, n_pixels, acres) |>
  arrange(year, county)

write.csv(loss_stats, file.path(csv_dir, "loss_by_county_year.csv"), row.names = FALSE)

# ---- Mean TCC by county and year ----
# Stack every yearly TCC raster into one multi-layer object, clean out no-data
# codes, and name each layer by its year (taken from the file name).
message("Summarizing mean TCC by county and year...")
tcc_stack <- rast(tcc_files)
tcc_stack <- subst(tcc_stack, c(254, 255), NA)
tcc_stack <- mask(tcc_stack, county_mask)
years <- as.integer(gsub(".*HK_TCC_([0-9]{4})\\.tif$", "\\1", tcc_files))
names(tcc_stack) <- as.character(years)

# Average only over cells that were forest (TCC >= 30%) in 2010, so every year
# is measured on the same set of cells.
tcc_stack <- mask(tcc_stack, ifel(forest, 1, NA))
tcc_extract <- terra::extract(tcc_stack, counties_v, fun = mean, na.rm = TRUE, ID = TRUE)
# Reshape from one column per year to one row per county and year.
tcc_stats <- tcc_extract |>
  mutate(county = counties_v$COUNTY[ID]) |>
  select(-ID) |>
  tidyr::pivot_longer(-county, names_to = "year", values_to = "mean_tcc") |>
  mutate(year = as.integer(year)) |>
  arrange(year, county)

write.csv(tcc_stats, file.path(csv_dir, "tcc_by_county_year.csv"), row.names = FALSE)

# ---- Sizes of loss patches, 2010-2024 ----
# For each loss year, group touching loss cells into patches. "8-neighbor" means
# cells that touch only at a corner (diagonally) still count as the same patch.
message("Patch sizes by loss year (8-neighbor, forest mask)...")
px_m2 <- prod(res(tcc_template))
patch_size_list <- list()
patch_sum_list <- list()
# Hansen codes 10 to 24 are the years 2010 to 2024.
for (code in 10:24) {
  yr <- code + 2000
  message("  patches ", yr)
  m <- ifel(loss_forest == code, 1, NA)
  p <- patches(m, directions = 8, zeroAsNA = TRUE)
  nmax <- minmax(p)[2]
  # If a year had no loss at all, record zero patches and move to the next year.
  if (is.na(nmax) || nmax < 1) {
    patch_sum_list[[as.character(yr)]] <- data.frame(
      year = yr, n_patches = 0, median_acres = NA_real_,
      mean_acres = NA_real_, max_acres = NA_real_
    )
    next
  }
  # Count cells in each patch and convert to acres.
  freq <- freq(p)
  freq <- freq[!is.na(freq$value), ]
  acres <- freq$count * px_m2 / 4046.8564224
  patch_size_list[[as.character(yr)]] <- data.frame(year = yr, acres = acres)
  patch_sum_list[[as.character(yr)]] <- data.frame(
    year = yr,
    n_patches = length(acres),
    median_acres = stats::median(acres),
    mean_acres = mean(acres),
    max_acres = max(acres)
  )
}
# One file with summary statistics per year, one with every individual patch size.
write.csv(bind_rows(patch_sum_list), file.path(csv_dir, "loss_patch_stats.csv"), row.names = FALSE)
write.csv(bind_rows(patch_size_list), file.path(csv_dir, "loss_patch_sizes.csv"), row.names = FALSE)

# ---- Loss map polygons for the dashboard ----
# Turn 2010-2024 loss cells into map shapes, merging all cells of the same year
# into one feature, then convert to latitude/longitude (EPSG:4326) for web maps.
message("Polygonizing 2010–2024 loss by year for the dashboard map...")
loss_map <- ifel(loss_forest >= 10 & loss_forest <= 24, loss_forest, NA)
loss_poly <- as.polygons(loss_map, dissolve = TRUE, na.rm = TRUE)
loss_sf <- sf::st_as_sf(loss_poly)
loss_sf$year <- as.integer(loss_sf[[1]]) + 2000
loss_sf <- loss_sf[, "year"]
loss_sf <- sf::st_make_valid(sf::st_transform(loss_sf, 4326))
sf::st_write(loss_sf, file.path(gis_dir, "loss_by_year.gpkg"), delete_dsn = TRUE, quiet = TRUE)
# Report the number of features and corner points, a rough guide to map file size.
message(
  "Map polygons (native 30 m): ", nrow(loss_sf), " year features, ~",
  format(as.integer(sum(lengths(sf::st_coordinates(sf::st_geometry(loss_sf))) / 2)), big.mark = ","),
  " vertices"
)

# ---- TCC canopy-decline layer ----
message("Building TCC canopy-decline layer (2010 to 2025, drop >= 15 pp)...")
# County-masked only (no 30% forest gate) so already-open / partial-canopy
# clearings that still lost >=15 pp are not hidden.
# Dissolve by rounded drop (pp) so tiles can ramp by magnitude (not one flat blob).
# "pp" = percentage points of canopy cover.
tcc_drop <- tcc_2010 - tcc_2025
decline_mag <- ifel(!is.na(tcc_2010) & (tcc_drop >= 15), round(tcc_drop), NA)
names(decline_mag) <- "drop_pp"
writeRaster(
  decline_mag, file.path(gis_dir, "tcc_decline_pp_2010_2025.tif"),
  overwrite = TRUE, wopt = list(datatype = "INT1U", gdal = gdal_opts)
)
# Total acres of decline, and a table of acres for each size of drop.
decline_acres <- global(!is.na(decline_mag), "sum", na.rm = TRUE)[1, 1] * px_acres
decline_freq <- terra::freq(decline_mag)
decline_freq <- decline_freq[!is.na(decline_freq$value), c("value", "count")]
names(decline_freq) <- c("drop_pp", "n_pixels")
decline_freq$acres <- decline_freq$n_pixels * px_acres
write.csv(decline_freq, file.path(csv_dir, "tcc_decline_by_drop_pp.csv"), row.names = FALSE)

# Turn decline cells into map shapes (one feature per drop size), attach acres
# and a readable label, and save in latitude/longitude for the web map.
decline_poly <- as.polygons(decline_mag, dissolve = TRUE, na.rm = TRUE)
decline_sf <- sf::st_as_sf(decline_poly)
decline_sf$drop_pp <- as.integer(decline_sf$drop_pp)
decline_sf <- decline_sf |>
  dplyr::left_join(decline_freq[, c("drop_pp", "acres")], by = "drop_pp")
decline_sf$label <- paste0("TCC drop ", decline_sf$drop_pp, " pp (2010\u20132025)")
decline_sf <- decline_sf[, c("drop_pp", "acres", "label")]
decline_sf <- sf::st_make_valid(sf::st_transform(decline_sf, 4326))
sf::st_write(decline_sf, file.path(gis_dir, "tcc_decline_2010_2025.gpkg"), delete_dsn = TRUE, quiet = TRUE)
message(
  "TCC decline map layer: ~", format(round(decline_acres), big.mark = ","),
  " acres, ", nrow(decline_sf), " drop classes (native 30 m), ~",
  format(as.integer(sum(lengths(sf::st_coordinates(sf::st_geometry(decline_sf))) / 2)), big.mark = ","),
  " vertices"
)

# ---- Final check ----
# Confirm the Hansen, TCC and change rasters all share exactly the same grid.
aligned <- compareGeom(loss, cover, tcc_template, tcc_change, stopOnError = FALSE)
message("Hansen/TCC/change share TCC grid: ", aligned)
message("Wrote ", normalizePath(csv_dir), " and ", normalizePath(gis_dir))
message("Done.")
