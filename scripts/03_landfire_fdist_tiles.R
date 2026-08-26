# Process LANDFIRE FDist-coded hist raster → agent polygons + PMTiles.
# From project root: Rscript scripts/03_landfire_fdist_tiles.R
#
# Input:  data/lf_hist_dist.tif  (FDist codes 111–733 despite "hist" name)
# Output: data/processed/landfire_fdist_*.{rds,gpkg,csv,tif}
#         www/tiles/landfire_fdist.pmtiles (+ copy under data/tiles/)
#
# Map features are dissolved by disturbance agent + years-since bin
# (severity is ignored / collapsed).

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

src_tif <- file.path(root, "data/lf_hist_dist.tif")
if (!file.exists(src_tif)) stop("Missing ", src_tif)

tcc_dir <- file.path(root, "data/TCC_Houghton_Keweenaw")
out_dir <- file.path(root, "data/processed")
tile_dir <- file.path(root, "data/tiles")
www_tile_dir <- file.path(root, "www/tiles")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tile_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(www_tile_dir, showWarnings = FALSE, recursive = TRUE)

gdal_opts <- c("COMPRESS=DEFLATE", "ZLEVEL=9", "TILED=YES")

# Collapsed agents for the map (drop Mech Add / Mastication / 0).
agent_from_code <- function(code) {
  type <- as.integer(code) %/% 100L
  dplyr::case_when(
    type == 3L ~ "harvest_remove",
    type == 6L ~ "mech_unknown",
    type == 5L ~ "insects",
    type == 1L ~ "fire",
    TRUE ~ NA_character_
  )
}

# Ones digit = LANDFIRE FDist years-since bin (not a calendar year).
years_since_from_code <- function(code) {
  as.integer(code) %% 10L
}

agent_label <- c(
  harvest_remove = "Timber harvest or clearing",
  mech_unknown = "Other mechanical change",
  insects = "Insects or disease",
  fire = "Fire"
)

years_since_label <- c(
  "1" = "About 1 year since disturbance",
  "2" = "About 2–5 years since disturbance",
  "3" = "About 6–10 years since disturbance",
  "4" = "About 11+ years since disturbance"
)

id_lookup <- c(harvest_remove = 1L, mech_unknown = 2L, insects = 3L, fire = 4L)

message("Loading TCC template + counties...")
tcc_template <- rast(file.path(tcc_dir, "HK_TCC_2010.tif"))
counties <- st_read(file.path(root, "data/houghton_keweenaw_counties.shp"), quiet = TRUE) |>
  dplyr::select(COUNTY, FIPS, AREA_SQMI) |>
  st_transform(crs(tcc_template))
counties_v <- vect(counties)
county_mask <- rasterize(counties_v, tcc_template, field = 1)

message("Reprojecting FDist raster onto TCC Albers grid...")
fdist_src <- rast(src_tif)
fdist <- project(fdist_src, tcc_template, method = "near")
fdist <- mask(fdist, county_mask)

codes <- as.integer(unique(freq(fdist)$value))
codes <- codes[!is.na(codes) & codes > 0]
keep <- codes[!is.na(agent_from_code(codes))]
drop <- setdiff(codes, keep)
if (length(drop)) {
  message("Dropping FDist codes (Add/Mastication/etc.): ", paste(sort(drop), collapse = ", "))
}

# class_id = agent_id * 10 + years_since_bin (severity collapsed).
class_id_from_code <- function(code) {
  agent <- agent_from_code(code)
  agent_id <- unname(id_lookup[agent])
  ys <- years_since_from_code(code)
  as.integer(agent_id * 10L + ys)
}

rcl <- cbind(keep, vapply(keep, class_id_from_code, integer(1)))
class_ras <- classify(fdist, rcl = rcl, others = NA)
names(class_ras) <- "class_id"
writeRaster(
  class_ras, file.path(out_dir, "landfire_fdist_agent.tif"),
  overwrite = TRUE, wopt = list(datatype = "INT1U", gdal = gdal_opts)
)

px_acres <- prod(res(tcc_template)) / 4046.8564224
fr_class <- terra::freq(class_ras)
fr_class <- fr_class[!is.na(fr_class$value), c("value", "count")]
names(fr_class) <- c("class_id", "n_pixels")
fr_class$agent_id <- as.integer(fr_class$class_id %/% 10L)
fr_class$years_since_bin <- as.integer(fr_class$class_id %% 10L)
fr_class$agent <- names(id_lookup)[match(fr_class$agent_id, id_lookup)]
fr_class$label <- unname(agent_label[fr_class$agent])
fr_class$years_since <- unname(years_since_label[as.character(fr_class$years_since_bin)])
fr_class$acres <- fr_class$n_pixels * px_acres
fr_class <- fr_class[order(fr_class$agent_id, fr_class$years_since_bin), ]

# Chart / summary still by agent (all time bins summed).
fr_agent <- fr_class |>
  group_by(agent_id, agent, label) |>
  summarise(
    n_pixels = sum(n_pixels),
    acres = sum(acres),
    .groups = "drop"
  ) |>
  arrange(desc(acres))
write.csv(fr_agent, file.path(out_dir, "landfire_fdist_by_agent.csv"), row.names = FALSE)
write.csv(fr_class, file.path(out_dir, "landfire_fdist_by_agent_years.csv"), row.names = FALSE)
message("Acres by agent:")
print(as.data.frame(fr_agent[, c("agent", "label", "acres", "n_pixels")]), row.names = FALSE)
message("Acres by agent + years since:")
print(as.data.frame(fr_class[, c("agent", "years_since", "acres", "n_pixels")]), row.names = FALSE)

message("Polygonizing by agent + years since...")
poly <- as.polygons(class_ras, dissolve = TRUE, na.rm = TRUE)
sf_poly <- st_as_sf(poly)
sf_poly$class_id <- as.integer(sf_poly$class_id)
sf_poly <- sf_poly |>
  left_join(
    fr_class[, c("class_id", "agent", "label", "years_since", "acres", "agent_id")],
    by = "class_id"
  )
sf_poly <- sf_poly[, c("agent", "label", "years_since", "acres", "agent_id", "class_id")]
sf_poly <- st_make_valid(st_transform(sf_poly, 4326))
st_write(sf_poly, file.path(out_dir, "landfire_fdist_by_agent.gpkg"), delete_dsn = TRUE, quiet = TRUE)
saveRDS(sf_poly, file.path(out_dir, "landfire_fdist_by_agent.rds"), compress = "xz")
message("Features: ", nrow(sf_poly))

pm_path <- file.path(www_tile_dir, "landfire_fdist.pmtiles")
message("Building PMTiles -> ", pm_path)
freestile(
  sf_poly,
  output = pm_path,
  layer_name = "landfire_fdist",
  min_zoom = 8,
  max_zoom = 14
)
file.copy(pm_path, file.path(tile_dir, "landfire_fdist.pmtiles"), overwrite = TRUE)
message("PMTiles size: ", format(file.info(pm_path)$size, big.mark = ","), " bytes")
message("Done.")
