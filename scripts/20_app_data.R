# Data for the Shiny app's story tabs.
#   output_spatial/app/drop_year.geojson: year-the-canopy-dropped polygons for the
#     Heartlands and Songbird footprints, one feature per footprint x source x year.
#     Sources: "tcc" (USFS Tree Canopy Cover fell 15+ points), "hansen" (Hansen
#     forest cleared), "both" (both sources flagged the cell within a year;
#     coloured by the canopy year).
#   output_spatial/app/footprints.geojson: the two sale boundaries.
#   output_csvs/app_sale_dates.csv: when each owner took over most of its parcels
#     (middle 80% of OwnerParce dates in the 2026 Commercial Forest list).
# Requires scripts 11, 12, 16 and 17.
# Run from project root: Rscript scripts/20_app_data.R

# ---- Setup ----
# Stop the PROJ library from trying to download projection grids from the internet.
Sys.setenv(PROJ_NETWORK = "OFF")

# terra reads rasters, sf handles polygons and GeoJSON, dplyr builds tables.
suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
})

# Make sure the script is being run from the project folder so relative paths work.
root <- if (dir.exists("output_spatial") && dir.exists("inputs/cfp")) {
  "."
} else {
  stop("Run from the kee_trees project root.")
}

# Input and output folders; app files go in output_spatial/app/.
csv_dir <- file.path(root, "output_csvs")
gis_dir <- file.path(root, "output_spatial")
app_dir <- file.path(gis_dir, "app")
dir.create(app_dir, showWarnings = FALSE, recursive = TRUE)

# Footprint boundaries and the year windows used in script 17.
footprints <- list(
  Heartlands = list(path = "inputs/boundaries/keweenaw_heartlands.shp", years = 2012:2022),
  Songbird = list(path = "inputs/boundaries/american_songbird.shp", years = 2015:2025)
)

# One colour per year from the "turbo" palette (blue = early, red = late).
year_colors <- function(years) {
  setNames(viridisLite::turbo(length(years), begin = 0.05, end = 0.95), years)
}

# ---- Drop-year polygons ----
# Helper: turn a drop-year raster into polygons, one (possibly multi-part)
# polygon per year. Years outside the window are cleared first. Each polygon
# gets its area in acres (4046.856 square metres per acre), labels and a
# colour, and is converted to latitude/longitude (EPSG:4326) for web maps.
to_polys <- function(r, fp, source, years) {
  r[!(r %in% years)] <- NA
  p <- st_as_sf(as.polygons(r, dissolve = TRUE))
  names(p)[1] <- "year"
  p$acres <- as.numeric(st_area(p)) / 4046.8564224
  p |>
    mutate(footprint = fp, source = source, year = as.integer(year),
           color = unname(year_colors(years)[as.character(year)])) |>
    st_transform(4326)
}

# For each footprint, read the two-layer raster from script 17 (TCC drop year
# and Hansen loss year). "both" keeps cells flagged by both sources with
# years no more than 1 apart, and uses the TCC year.
drop <- bind_rows(lapply(names(footprints), function(fp) {
  yrs <- footprints[[fp]]$years
  r <- rast(file.path(gis_dir, sprintf("drop_year_%s.tif", tolower(fp))))
  tcc <- r[["tcc_drop_year"]]
  han <- r[["hansen_loss_year"]]
  both <- ifel(!is.na(tcc) & !is.na(han) & abs(tcc - han) <= 1, tcc, NA)
  bind_rows(
    to_polys(tcc, fp, "tcc", yrs),
    to_polys(han, fp, "hansen", yrs),
    to_polys(both, fp, "both", yrs)
  )
})) |>
  select(footprint, source, year, acres, color)

# Write the GeoJSON. Any old file is deleted first because the GeoJSON driver
# will not overwrite. Coordinates are rounded to 6 decimals (about 10 cm) to
# keep the file small.
drop_path <- file.path(app_dir, "drop_year.geojson")
unlink(drop_path)
st_write(drop, drop_path, quiet = TRUE, layer_options = "COORDINATE_PRECISION=6")

# ---- Footprint outlines ----
# Read each boundary, drop any elevation values, fix invalid geometry, merge
# into a single outline, and convert to latitude/longitude.
fp_sf <- bind_rows(lapply(names(footprints), function(fp) {
  st_read(file.path(root, footprints[[fp]]$path), quiet = TRUE) |>
    st_zm() |>
    st_make_valid() |>
    st_union() |>
    st_as_sf() |>
    mutate(footprint = fp)
})) |>
  st_transform(4326)
st_geometry(fp_sf) <- "geometry"
fp_path <- file.path(app_dir, "footprints.geojson")
unlink(fp_path)
st_write(fp_sf, fp_path, quiet = TRUE, layer_options = "COORDINATE_PRECISION=6")

# ---- Sale dates by owner ----
# Read the 2026 Commercial Forest parcel list (attributes only). OwnerParce
# is the date the current owner acquired the parcel.
p26 <- st_read(file.path(root, "inputs/cfp/cfp_hk_2026.shp"), quiet = TRUE) |> st_drop_geometry()
p26$date <- as.Date(p26$OwnerParce)
# For one owner (matched by name in FullLegalN), sort its acquisition dates
# and take the 10th, 50th and 90th percentiles. The middle 80% shows when
# most parcels changed hands without being skewed by a few odd dates.
# Placeholder dates before 1900 are ignored.
owner_dates <- function(owner, pattern) {
  d <- sort(p26$date[grepl(pattern, p26$FullLegalN, ignore.case = TRUE) &
                       !is.na(p26$date) & p26$date > as.Date("1900-01-01")])
  q <- d[c(max(1, ceiling(0.1 * length(d))), ceiling(0.5 * length(d)), ceiling(0.9 * length(d)))]
  data.frame(owner = owner, date_start = q[1], date_median = q[2], date_end = q[3])
}
# Name patterns for each owner. "\\b" marks a whole word, so "Lyme" or "MWF"
# does not match inside a longer word.
dates <- bind_rows(
  owner_dates("Verdant", "Verdant"),
  owner_dates("The Nature Conservancy", "Nature Conservancy"),
  owner_dates("Molpus", "Molpus|\\bMWF\\b"),
  owner_dates("Lyme", "\\bLyme\\b")
)
write.csv(dates, file.path(csv_dir, "app_sale_dates.csv"), row.names = FALSE)

# Print a quick check: number of years and total acres per footprint and source.
print(drop |> st_drop_geometry() |> group_by(footprint, source) |>
        summarise(years = n(), acres = round(sum(acres)), .groups = "drop"))
print(dates)
message("Wrote ", drop_path, " (", round(file.size(drop_path) / 1e6, 1), " MB), ", fp_path,
        " and app_sale_dates.csv")
