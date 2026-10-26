# Rebuild the tree canopy cover (TCC) decline map layer: shapes grouped by
# size of canopy drop (drop_pp), plus a PMTiles web-map file; optionally upload to R2.
# This repeats the decline step of scripts/01_harmonize.R without redoing everything else.
# From project root: Rscript scripts/02_rebuild_tcc_decline_tiles.R
# Set UPLOAD_R2=1 to push tcc_decline.pmtiles to the kee-trees-tiles bucket.

# Stop the PROJ library from trying to download projection files from the internet.
Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
  library(freestiler)
})

# ---- Folders and settings ----
# All paths are relative to the project folder, so stop early if run from elsewhere.
root <- if (dir.exists("inputs/tcc")) {
  normalizePath(".")
} else {
  stop("Run from the kee_trees project root.")
}

tcc_dir <- file.path(root, "inputs/tcc")
csv_dir <- file.path(root, "output_csvs")
gis_dir <- file.path(root, "output_spatial")
tile_dir <- file.path(root, "output_spatial/tiles")
for (d in c(csv_dir, gis_dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)
dir.create(tile_dir, showWarnings = FALSE, recursive = TRUE)

# Compression settings for output GeoTIFFs (smaller files, no loss of information).
gdal_opts <- c("COMPRESS=DEFLATE", "ZLEVEL=9", "TILED=YES")

# ---- Load counties and TCC for 2010 and 2025 ----
# The 2010 TCC raster is the template grid (30 m cells) that everything lines up to.
message("Loading counties + TCC 2010/2025...")
tcc_template <- rast(file.path(tcc_dir, "HK_TCC_2010.tif"))
counties <- st_read(file.path(root, "inputs/boundaries/houghton_keweenaw_counties.shp"), quiet = TRUE) |>
  dplyr::select(COUNTY, FIPS, AREA_SQMI) |>
  st_transform(crs(tcc_template))
counties_v <- vect(counties)
county_mask <- rasterize(counties_v, tcc_template, field = 1)

# TCC values 254 and 255 are no-data codes, so set them to missing, then keep
# only cells inside Houghton and Keweenaw counties.
tcc_2010 <- subst(rast(file.path(tcc_dir, "HK_TCC_2010.tif")), c(254, 255), NA)
tcc_2025 <- subst(rast(file.path(tcc_dir, "HK_TCC_2025.tif")), c(254, 255), NA)
tcc_2010 <- mask(tcc_2010, county_mask)
tcc_2025 <- mask(tcc_2025, county_mask)

# ---- Find cells that lost 15 or more percentage points of canopy ----
# Acres per cell: 30 m x 30 m = 900 square metres, divided by 4046.856 square
# metres per acre (about 0.22 acres).
px_acres <- prod(res(tcc_template)) / 4046.8564224
# Positive drop = canopy lost. No 30% forest cutoff is applied, so clearings in
# already-open or partly wooded cells still show up. Drops are rounded to whole
# percentage points (pp) so the map can shade by how big the drop was.
tcc_drop <- tcc_2010 - tcc_2025
decline_mag <- ifel(!is.na(tcc_2010) & (tcc_drop >= 15), round(tcc_drop), NA)
names(decline_mag) <- "drop_pp"

# ---- Save the decline raster and an acres-by-drop table ----
message("Writing decline magnitude raster + CSV...")
writeRaster(
  decline_mag, file.path(gis_dir, "tcc_decline_pp_2010_2025.tif"),
  overwrite = TRUE, wopt = list(datatype = "INT1U", gdal = gdal_opts)
)
# Total decline acres, then the number of cells and acres for each drop size.
decline_acres <- global(!is.na(decline_mag), "sum", na.rm = TRUE)[1, 1] * px_acres
decline_freq <- terra::freq(decline_mag)
decline_freq <- decline_freq[!is.na(decline_freq$value), c("value", "count")]
names(decline_freq) <- c("drop_pp", "n_pixels")
decline_freq$acres <- decline_freq$n_pixels * px_acres
write.csv(decline_freq, file.path(csv_dir, "tcc_decline_by_drop_pp.csv"), row.names = FALSE)

# ---- Convert decline cells to map shapes ----
# All cells with the same drop size are merged into one feature. Each feature
# gets its acres and a readable label, and is converted to latitude/longitude
# (EPSG:4326) for web maps.
message("Polygonizing by drop_pp (dissolve same magnitude)...")
decline_poly <- as.polygons(decline_mag, dissolve = TRUE, na.rm = TRUE)
decline_sf <- st_as_sf(decline_poly)
decline_sf$drop_pp <- as.integer(decline_sf$drop_pp)
decline_sf <- decline_sf |>
  left_join(decline_freq[, c("drop_pp", "acres")], by = "drop_pp")
decline_sf$label <- paste0("TCC drop ", decline_sf$drop_pp, " pp (2010\u20132025)")
decline_sf <- decline_sf[, c("drop_pp", "acres", "label")]
decline_sf <- st_make_valid(st_transform(decline_sf, 4326))
st_write(decline_sf, file.path(gis_dir, "tcc_decline_2010_2025.gpkg"), delete_dsn = TRUE, quiet = TRUE)
message(
  "TCC decline: ~", format(round(decline_acres), big.mark = ","),
  " acres, ", nrow(decline_sf), " drop classes"
)

# ---- Build the web-map tile file ----
# PMTiles is a single-file format of pre-cut map tiles. Zoom 8 is roughly a
# regional view; zoom 14 is close enough to see individual clearings.
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

# ---- Optional upload to Cloudflare R2 storage ----
# Only runs when UPLOAD_R2=1 is set. Account details and keys are read from
# environment variables so they are never written in the script.
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
