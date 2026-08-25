# Align Hansen to the NLCD TCC grid and write dashboard summaries.
# Run from the project root: Rscript scripts/01_harmonize.R
# NOTE: Keep this outside an app-root R/ folder — shiny::runApp(".") auto-sources R/*.R.

Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
  library(tidyr)
})

terraOptions(progress = 1)

root <- if (dir.exists("data/TCC_Houghton_Keweenaw")) {
  "."
} else {
  stop("Run this script from the kee_trees project root.")
}

tcc_dir <- file.path(root, "data/TCC_Houghton_Keweenaw")
hansen_dir <- file.path(root, "data/Hansen_Houghton_Keweenaw")
out_dir <- file.path(root, "data/processed")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

gdal_opts <- c("COMPRESS=DEFLATE", "ZLEVEL=9", "TILED=YES")

message("Loading TCC template and counties...")
tcc_files <- sort(list.files(tcc_dir, pattern = "^HK_TCC_[0-9]{4}\\.tif$", full.names = TRUE))
stopifnot(length(tcc_files) > 0)
tcc_template <- rast(tcc_files[[1]])

counties <- st_read(file.path(root, "data/houghton_keweenaw_counties.shp"), quiet = TRUE) |>
  dplyr::select(COUNTY, FIPS, AREA_SQMI)
counties <- st_transform(counties, crs(tcc_template))
counties_v <- vect(counties)
counties_v$county_id <- seq_len(nrow(counties_v))

st_write(counties, file.path(out_dir, "counties.gpkg"), delete_dsn = TRUE, quiet = TRUE)

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

writeRaster(loss, file.path(out_dir, "hansen_lossyear.tif"),
            overwrite = TRUE, wopt = list(datatype = "INT1U", gdal = gdal_opts))
writeRaster(cover, file.path(out_dir, "hansen_treecover2000.tif"),
            overwrite = TRUE, wopt = list(datatype = "INT1U", gdal = gdal_opts))

message("Building TCC change (2025 - 2010)...")
tcc_2010 <- rast(file.path(tcc_dir, "HK_TCC_2010.tif"))
tcc_2025 <- rast(file.path(tcc_dir, "HK_TCC_2025.tif"))
tcc_2010 <- subst(tcc_2010, c(254, 255), NA)
tcc_2025 <- subst(tcc_2025, c(254, 255), NA)
tcc_2010 <- mask(tcc_2010, county_mask)
tcc_2025 <- mask(tcc_2025, county_mask)
tcc_change <- tcc_2025 - tcc_2010
names(tcc_change) <- "tcc_change_2010_2025"
writeRaster(tcc_change, file.path(out_dir, "tcc_change_2010_2025.tif"),
            overwrite = TRUE, wopt = list(datatype = "INT2S", gdal = gdal_opts))

px_acres <- prod(res(tcc_template)) / 4046.8564224
# Forest mask matches the analysis start year: NLCD TCC 2010 >= 30%.
forest <- tcc_2010 >= 30
county_id <- rasterize(counties_v, tcc_template, field = "county_id")
id_lookup <- data.frame(
  county_id = counties_v$county_id,
  county = counties_v$COUNTY
)

message("Summarizing Hansen loss by county and year...")
loss_forest <- ifel(forest, loss, NA)
loss_forest <- ifel(loss_forest == 0, NA, loss_forest)
ct <- terra::crosstab(c(loss_forest, county_id), long = TRUE, useNA = FALSE)
names(ct) <- c("lossyear", "county_id", "n_pixels")
loss_stats <- ct |>
  mutate(
    year = as.integer(lossyear) + 2000,
    acres = n_pixels * px_acres
  ) |>
  left_join(id_lookup, by = "county_id") |>
  filter(year >= 2001, year <= 2024, !is.na(county)) |>
  select(year, county, n_pixels, acres) |>
  arrange(year, county)

write.csv(loss_stats, file.path(out_dir, "loss_by_county_year.csv"), row.names = FALSE)

message("Summarizing mean TCC by county and year...")
tcc_stack <- rast(tcc_files)
tcc_stack <- subst(tcc_stack, c(254, 255), NA)
tcc_stack <- mask(tcc_stack, county_mask)
years <- as.integer(gsub(".*HK_TCC_([0-9]{4})\\.tif$", "\\1", tcc_files))
names(tcc_stack) <- as.character(years)

tcc_stack <- mask(tcc_stack, ifel(forest, 1, NA))
tcc_extract <- terra::extract(tcc_stack, counties_v, fun = mean, na.rm = TRUE, ID = TRUE)
tcc_stats <- tcc_extract |>
  mutate(county = counties_v$COUNTY[ID]) |>
  select(-ID) |>
  tidyr::pivot_longer(-county, names_to = "year", values_to = "mean_tcc") |>
  mutate(year = as.integer(year)) |>
  arrange(year, county)

write.csv(tcc_stats, file.path(out_dir, "tcc_by_county_year.csv"), row.names = FALSE)

message("Patch sizes by loss year (8-neighbor, forest mask)...")
px_m2 <- prod(res(tcc_template))
patch_size_list <- list()
patch_sum_list <- list()
for (code in 10:24) {
  yr <- code + 2000
  message("  patches ", yr)
  m <- ifel(loss_forest == code, 1, NA)
  p <- patches(m, directions = 8, zeroAsNA = TRUE)
  nmax <- minmax(p)[2]
  if (is.na(nmax) || nmax < 1) {
    patch_sum_list[[as.character(yr)]] <- data.frame(
      year = yr, n_patches = 0, median_acres = NA_real_,
      mean_acres = NA_real_, max_acres = NA_real_
    )
    next
  }
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
write.csv(bind_rows(patch_sum_list), file.path(out_dir, "loss_patch_stats.csv"), row.names = FALSE)
write.csv(bind_rows(patch_size_list), file.path(out_dir, "loss_patch_sizes.csv"), row.names = FALSE)

message("Polygonizing 2010–2024 loss by year for the dashboard map...")
loss_map <- ifel(loss_forest >= 10 & loss_forest <= 24, loss_forest, NA)
loss_poly <- as.polygons(loss_map, dissolve = TRUE, na.rm = TRUE)
loss_sf <- sf::st_as_sf(loss_poly)
loss_sf$year <- as.integer(loss_sf[[1]]) + 2000
loss_sf <- loss_sf[, "year"]
loss_sf <- sf::st_make_valid(sf::st_transform(loss_sf, 4326))
sf::st_write(loss_sf, file.path(out_dir, "loss_by_year.gpkg"), delete_dsn = TRUE, quiet = TRUE)
saveRDS(loss_sf, file.path(out_dir, "loss_by_year.rds"), compress = "xz")
message(
  "Map polygons (native 30 m): ", nrow(loss_sf), " year features, ~",
  format(as.integer(sum(lengths(sf::st_coordinates(sf::st_geometry(loss_sf))) / 2)), big.mark = ","),
  " vertices"
)

message("Building TCC canopy-decline layer (2010 to 2025, drop >= 15 pp)...")
# County-masked only (no 30% forest gate) so already-open / partial-canopy
# clearings that still lost >=15 pp are not hidden.
# Dissolve by rounded drop (pp) so tiles can ramp by magnitude (not one flat blob).
tcc_drop <- tcc_2010 - tcc_2025
decline_mag <- ifel(!is.na(tcc_2010) & (tcc_drop >= 15), round(tcc_drop), NA)
names(decline_mag) <- "drop_pp"
writeRaster(
  decline_mag, file.path(out_dir, "tcc_decline_pp_2010_2025.tif"),
  overwrite = TRUE, wopt = list(datatype = "INT1U", gdal = gdal_opts)
)
decline_acres <- global(!is.na(decline_mag), "sum", na.rm = TRUE)[1, 1] * px_acres
decline_freq <- terra::freq(decline_mag)
decline_freq <- decline_freq[!is.na(decline_freq$value), c("value", "count")]
names(decline_freq) <- c("drop_pp", "n_pixels")
decline_freq$acres <- decline_freq$n_pixels * px_acres
write.csv(decline_freq, file.path(out_dir, "tcc_decline_by_drop_pp.csv"), row.names = FALSE)

decline_poly <- as.polygons(decline_mag, dissolve = TRUE, na.rm = TRUE)
decline_sf <- sf::st_as_sf(decline_poly)
decline_sf$drop_pp <- as.integer(decline_sf$drop_pp)
decline_sf <- decline_sf |>
  dplyr::left_join(decline_freq[, c("drop_pp", "acres")], by = "drop_pp")
decline_sf$label <- paste0("TCC drop ", decline_sf$drop_pp, " pp (2010\u20132025)")
decline_sf <- decline_sf[, c("drop_pp", "acres", "label")]
decline_sf <- sf::st_make_valid(sf::st_transform(decline_sf, 4326))
sf::st_write(decline_sf, file.path(out_dir, "tcc_decline_2010_2025.gpkg"), delete_dsn = TRUE, quiet = TRUE)
saveRDS(decline_sf, file.path(out_dir, "tcc_decline_2010_2025.rds"), compress = "xz")
message(
  "TCC decline map layer: ~", format(round(decline_acres), big.mark = ","),
  " acres, ", nrow(decline_sf), " drop classes (native 30 m), ~",
  format(as.integer(sum(lengths(sf::st_coordinates(sf::st_geometry(decline_sf))) / 2)), big.mark = ","),
  " vertices"
)

aligned <- compareGeom(loss, cover, tcc_template, tcc_change, stopOnError = FALSE)
message("Hansen/TCC/change share TCC grid: ", aligned)
message("Wrote ", normalizePath(out_dir))
message("Done.")
