# Build CFP owner-type + change polygons and PMTiles (default min parcel acres = 20).
# From project root: Rscript scripts/05_cfp_owner_tiles.R
#
# Requires rasters from scripts/04_cfp_ownership_change.R:
#   data/processed/cfp_owner_2020.tif, cfp_owner_2026.tif, cfp_owner_labels.csv
#
# Output:
#   data/processed/cfp_owner_{2020,2026,change}.{gpkg,rds}
#   www/tiles/cfp_owner_{2020,2026,change}.pmtiles (+ copies under data/tiles/)

Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
  library(freestiler)
})

root <- if (dir.exists("data/processed") && dir.exists("data/cfp_data")) {
  normalizePath(".")
} else {
  stop("Run from the kee_trees project root.")
}

out_dir <- file.path(root, "data/processed")
tile_dir <- file.path(root, "data/tiles")
www_tile_dir <- file.path(root, "www/tiles")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tile_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(www_tile_dir, showWarnings = FALSE, recursive = TRUE)

r20_path <- file.path(out_dir, "cfp_owner_2020.tif")
r26_path <- file.path(out_dir, "cfp_owner_2026.tif")
lut_path <- file.path(out_dir, "cfp_owner_labels.csv")
if (!all(file.exists(r20_path, r26_path, lut_path))) {
  stop(
    "Missing CFP owner rasters/labels. Run first:\n",
    "  Rscript scripts/04_cfp_ownership_change.R"
  )
}

NON_CFP <- "non-CFP"
ENTERED <- "entered"
EXITED <- "exited"
STABLE <- "stable"
CHANGED <- "type_changed"

change_labels <- c(
  stable = "Stable (same owner type)",
  type_changed = "Owner type changed",
  entered = "Entered CFP (was non-CFP in 2020)",
  exited = "Left CFP (non-CFP in 2026)"
)

message("Loading owner rasters + labels...")
r20 <- rast(r20_path)
r26 <- rast(r26_path)
lut <- read.csv(lut_path, stringsAsFactors = FALSE)
code_to_label <- setNames(lut$label, lut$code)
px_acres <- prod(res(r20)) / 4046.8564224

label_for_code <- function(code) {
  ifelse(
    is.na(code) | code <= 0,
    NON_CFP,
    unname(code_to_label[as.character(code)])
  )
}

polygonize_owner_year <- function(r, year_label) {
  message("Polygonizing owner type ", year_label, "...")
  poly <- as.polygons(r, dissolve = TRUE, na.rm = TRUE)
  sf_poly <- st_as_sf(poly)
  code_col <- names(sf_poly)[1]
  sf_poly$owner_code <- as.integer(sf_poly[[code_col]])
  sf_poly$owner_type <- unname(code_to_label[as.character(sf_poly$owner_code)])
  sf_poly$owner_type[is.na(sf_poly$owner_type)] <- "(missing owner type)"
  fr <- terra::freq(r)
  fr <- fr[!is.na(fr$value), c("value", "count")]
  names(fr) <- c("owner_code", "n_pixels")
  fr$acres <- fr$n_pixels * px_acres
  sf_poly <- sf_poly |>
    left_join(fr[, c("owner_code", "acres")], by = "owner_code")
  sf_poly$year <- as.integer(year_label)
  sf_poly$label <- paste0(year_label, " · ", sf_poly$owner_type)
  sf_poly <- sf_poly[, c("year", "owner_type", "owner_code", "acres", "label")]
  st_make_valid(st_transform(sf_poly, 4326))
}

write_pmtiles <- function(sf_obj, stem, layer_name) {
  pm_www <- file.path(www_tile_dir, paste0(stem, ".pmtiles"))
  message("Building PMTiles -> ", pm_www)
  freestile(
    sf_obj,
    output = pm_www,
    layer_name = layer_name,
    min_zoom = 8,
    max_zoom = 14
  )
  file.copy(pm_www, file.path(tile_dir, paste0(stem, ".pmtiles")), overwrite = TRUE)
  message("  size: ", format(file.info(pm_www)$size, big.mark = ","), " bytes")
  invisible(pm_www)
}

sf20 <- polygonize_owner_year(r20, 2020)
sf26 <- polygonize_owner_year(r26, 2026)
st_write(sf20, file.path(out_dir, "cfp_owner_2020.gpkg"), delete_dsn = TRUE, quiet = TRUE)
st_write(sf26, file.path(out_dir, "cfp_owner_2026.gpkg"), delete_dsn = TRUE, quiet = TRUE)
saveRDS(sf20, file.path(out_dir, "cfp_owner_2020.rds"), compress = "xz")
saveRDS(sf26, file.path(out_dir, "cfp_owner_2026.rds"), compress = "xz")
message("2020 features: ", nrow(sf20), " | 2026 features: ", nrow(sf26))

message("Building change pair raster...")
# 0 = non-CFP sentinel so pairs encode enter/exit.
c20 <- ifel(is.na(r20), 0, r20)
c26 <- ifel(is.na(r26), 0, r26)
pair <- ifel((c20 == 0) & (c26 == 0), NA, c20 * 1000 + c26)
names(pair) <- "pair_id"
writeRaster(
  pair, file.path(out_dir, "cfp_owner_change_pair.tif"),
  overwrite = TRUE,
  wopt = list(datatype = "INT4S", gdal = c("COMPRESS=DEFLATE", "ZLEVEL=9", "TILED=YES"))
)

message("Polygonizing change pairs...")
chg_poly <- as.polygons(pair, dissolve = TRUE, na.rm = TRUE)
sf_chg <- st_as_sf(chg_poly)
sf_chg$pair_id <- as.integer(sf_chg$pair_id)
sf_chg$from_code <- as.integer(sf_chg$pair_id %/% 1000L)
sf_chg$to_code <- as.integer(sf_chg$pair_id %% 1000L)
sf_chg$from_type <- label_for_code(sf_chg$from_code)
sf_chg$to_type <- label_for_code(sf_chg$to_code)
sf_chg$change_class <- dplyr::case_when(
  sf_chg$from_code == 0L & sf_chg$to_code > 0L ~ ENTERED,
  sf_chg$from_code > 0L & sf_chg$to_code == 0L ~ EXITED,
  sf_chg$from_code > 0L & sf_chg$to_code > 0L & sf_chg$from_code == sf_chg$to_code ~ STABLE,
  TRUE ~ CHANGED
)
sf_chg$change_label <- unname(change_labels[sf_chg$change_class])

fr_pair <- terra::freq(pair)
fr_pair <- fr_pair[!is.na(fr_pair$value), c("value", "count")]
names(fr_pair) <- c("pair_id", "n_pixels")
fr_pair$acres <- fr_pair$n_pixels * px_acres
sf_chg <- sf_chg |>
  left_join(fr_pair[, c("pair_id", "acres")], by = "pair_id")

sf_chg$label <- paste0(sf_chg$from_type, " → ", sf_chg$to_type)
sf_chg <- sf_chg[, c(
  "change_class", "change_label", "from_type", "to_type",
  "from_code", "to_code", "acres", "label", "pair_id"
)]
sf_chg <- st_make_valid(st_transform(sf_chg, 4326))

st_write(sf_chg, file.path(out_dir, "cfp_owner_change.gpkg"), delete_dsn = TRUE, quiet = TRUE)
saveRDS(sf_chg, file.path(out_dir, "cfp_owner_change.rds"), compress = "xz")

chg_summary <- sf_chg |>
  st_drop_geometry() |>
  group_by(change_class, change_label) |>
  summarise(acres = sum(acres, na.rm = TRUE), n_polys = n(), .groups = "drop") |>
  arrange(desc(acres))
write.csv(chg_summary, file.path(out_dir, "cfp_owner_change_summary.csv"), row.names = FALSE)
message("Change acres:")
print(as.data.frame(chg_summary), row.names = FALSE)
message("Change features: ", nrow(sf_chg))

write_pmtiles(sf20, "cfp_owner_2020", "cfp_owner_2020")
write_pmtiles(sf26, "cfp_owner_2026", "cfp_owner_2026")
write_pmtiles(sf_chg, "cfp_owner_change", "cfp_owner_change")

message("Done.")
