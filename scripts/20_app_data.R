# Data for the Shiny app's story tabs.
#   output_spatial/app/drop_year.geojson: year-the-canopy-dropped polygons for the
#     Heartlands and Songbird footprints, one feature per footprint x source x year.
#     Sources: "tcc" (USFS canopy fell 15+ points), "hansen" (Hansen forest cleared),
#     "both" (both sources flagged the cell within a year; coloured by the canopy year).
#   output_spatial/app/footprints.geojson: the two sale boundaries.
#   output_csvs/app_sale_dates.csv: when each owner took over most of its parcels
#     (middle 80% of OwnerParce dates in the 2026 Commercial Forest list).
# Requires scripts 11, 12, 16 and 17.
# Run from project root: Rscript scripts/20_app_data.R

Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
})

root <- if (dir.exists("output_spatial") && dir.exists("inputs/cfp")) {
  "."
} else {
  stop("Run from the kee_trees project root.")
}

csv_dir <- file.path(root, "output_csvs")
gis_dir <- file.path(root, "output_spatial")
app_dir <- file.path(gis_dir, "app")
dir.create(app_dir, showWarnings = FALSE, recursive = TRUE)

footprints <- list(
  Heartlands = list(path = "inputs/boundaries/keweenaw_heartlands.shp", years = 2012:2022),
  Songbird = list(path = "inputs/boundaries/american_songbird.shp", years = 2015:2025)
)

year_colors <- function(years) {
  setNames(viridisLite::turbo(length(years), begin = 0.05, end = 0.95), years)
}

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

drop_path <- file.path(app_dir, "drop_year.geojson")
unlink(drop_path)
st_write(drop, drop_path, quiet = TRUE, layer_options = "COORDINATE_PRECISION=6")

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

p26 <- st_read(file.path(root, "inputs/cfp/cfp_hk_2026.shp"), quiet = TRUE) |> st_drop_geometry()
p26$date <- as.Date(p26$OwnerParce)
owner_dates <- function(owner, pattern) {
  d <- sort(p26$date[grepl(pattern, p26$FullLegalN, ignore.case = TRUE) &
                       !is.na(p26$date) & p26$date > as.Date("1900-01-01")])
  q <- d[c(max(1, ceiling(0.1 * length(d))), ceiling(0.5 * length(d)), ceiling(0.9 * length(d)))]
  data.frame(owner = owner, date_start = q[1], date_median = q[2], date_end = q[3])
}
dates <- bind_rows(
  owner_dates("Verdant", "Verdant"),
  owner_dates("The Nature Conservancy", "Nature Conservancy"),
  owner_dates("Molpus", "Molpus|\\bMWF\\b"),
  owner_dates("Lyme", "\\bLyme\\b")
)
write.csv(dates, file.path(csv_dir, "app_sale_dates.csv"), row.names = FALSE)

print(drop |> st_drop_geometry() |> group_by(footprint, source) |>
        summarise(years = n(), acres = round(sum(acres)), .groups = "drop"))
print(dates)
message("Wrote ", drop_path, " (", round(file.size(drop_path) / 1e6, 1), " MB), ", fp_path,
        " and app_sale_dates.csv")
