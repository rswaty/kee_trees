# Rebuild TCC decline polygons (by drop_pp) + PMTiles, optionally upload to R2.
# From project root: Rscript scripts/02_rebuild_tcc_decline_tiles.R
# Set UPLOAD_R2=1 to push tcc_decline.pmtiles to the kee-trees-tiles bucket.

Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
  library(freestiler)
})

root <- if (dir.exists("data/TCC_Houghton_Keweenaw")) {
  normalizePath(".")
} else {
  stop("Run from the kee_trees project root.")
}

tcc_dir <- file.path(root, "data/TCC_Houghton_Keweenaw")
out_dir <- file.path(root, "data/processed")
tile_dir <- file.path(root, "data/tiles")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tile_dir, showWarnings = FALSE, recursive = TRUE)

gdal_opts <- c("COMPRESS=DEFLATE", "ZLEVEL=9", "TILED=YES")

message("Loading counties + TCC 2010/2025...")
tcc_template <- rast(file.path(tcc_dir, "HK_TCC_2010.tif"))
counties <- st_read(file.path(root, "data/houghton_keweenaw_counties.shp"), quiet = TRUE) |>
  dplyr::select(COUNTY, FIPS, AREA_SQMI) |>
  st_transform(crs(tcc_template))
counties_v <- vect(counties)
county_mask <- rasterize(counties_v, tcc_template, field = 1)

tcc_2010 <- subst(rast(file.path(tcc_dir, "HK_TCC_2010.tif")), c(254, 255), NA)
tcc_2025 <- subst(rast(file.path(tcc_dir, "HK_TCC_2025.tif")), c(254, 255), NA)
tcc_2010 <- mask(tcc_2010, county_mask)
tcc_2025 <- mask(tcc_2025, county_mask)

px_acres <- prod(res(tcc_template)) / 4046.8564224
tcc_drop <- tcc_2010 - tcc_2025
decline_mag <- ifel(!is.na(tcc_2010) & (tcc_drop >= 15), round(tcc_drop), NA)
names(decline_mag) <- "drop_pp"

message("Writing decline magnitude raster + CSV...")
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

message("Polygonizing by drop_pp (dissolve same magnitude)...")
decline_poly <- as.polygons(decline_mag, dissolve = TRUE, na.rm = TRUE)
decline_sf <- st_as_sf(decline_poly)
decline_sf$drop_pp <- as.integer(decline_sf$drop_pp)
decline_sf <- decline_sf |>
  left_join(decline_freq[, c("drop_pp", "acres")], by = "drop_pp")
decline_sf$label <- paste0("TCC drop ", decline_sf$drop_pp, " pp (2010\u20132025)")
decline_sf <- decline_sf[, c("drop_pp", "acres", "label")]
decline_sf <- st_make_valid(st_transform(decline_sf, 4326))
st_write(decline_sf, file.path(out_dir, "tcc_decline_2010_2025.gpkg"), delete_dsn = TRUE, quiet = TRUE)
saveRDS(decline_sf, file.path(out_dir, "tcc_decline_2010_2025.rds"), compress = "xz")
message(
  "TCC decline: ~", format(round(decline_acres), big.mark = ","),
  " acres, ", nrow(decline_sf), " drop classes"
)

pm_path <- file.path(tile_dir, "tcc_decline.pmtiles")
message("Building PMTiles -> ", pm_path)
freestile(
  decline_sf,
  output = pm_path,
  layer_name = "tcc_decline",
  min_zoom = 8,
  max_zoom = 14
)
message("PMTiles size: ", format(file.info(pm_path)$size, big.mark = ","), " bytes")

if (identical(Sys.getenv("UPLOAD_R2"), "1")) {
  suppressPackageStartupMessages(library(pmtiles))
  message("Uploading tcc_decline.pmtiles to R2...")
  pm_upload(
    pm_path,
    remote = "tcc_decline.pmtiles",
    bucket = r2_bucket(
      "kee-trees-tiles",
      account_id = Sys.getenv("CLOUDFLARE_ACCOUNT_ID"),
      access_key = Sys.getenv("CLOUDFLARE_KEE_TREES_KEY"),
      secret_key = Sys.getenv("CLOUDFLARE_KEE_TREES_SECRET")
    )
  )
} else {
  message("Skip R2 upload (set UPLOAD_R2=1 to push).")
}

message("Done.")
