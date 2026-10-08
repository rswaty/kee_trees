# Year canopy dropped, by 30 m cell, inside the Keweenaw Heartlands (2012-2022)
# and American Songbird (2015-2025) footprints, from two sources:
#   TCC: first year in the window where canopy is >= DROP_PP points below the
#        highest value of the previous LOOKBACK years (fewer at the start of
#        the TCC record, which begins in 2010).
#   Hansen: Global Forest Change loss year (record ends 2024).
# Outputs a two-layer GeoTIFF per footprint, acres per year by source, and
# agreement between the two sources.
# Run from project root: Rscript scripts/17_footprint_drop_year_maps.R

Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
  library(tidyr)
})

root <- if (dir.exists("output_csvs") && dir.exists("inputs/tcc")) {
  "."
} else {
  stop("Run from the kee_trees project root.")
}

tcc_dir <- file.path(root, "inputs/tcc")
csv_dir <- file.path(root, "output_csvs")
gis_dir <- file.path(root, "output_spatial")

DROP_PP <- 15
LOOKBACK <- 3
TCC_FIRST_YEAR <- 2010

footprints <- list(
  Heartlands = list(
    path = "inputs/boundaries/keweenaw_heartlands.shp",
    years = 2012:2022
  ),
  Songbird = list(
    path = "inputs/boundaries/american_songbird.shp",
    years = 2015:2025
  )
)

tcc_path <- function(y) file.path(tcc_dir, sprintf("HK_TCC_%d.tif", y))
template <- rast(tcc_path(TCC_FIRST_YEAR))
px_acres <- prod(res(template)) / 4046.8564224
hansen <- rast(file.path(gis_dir, "hansen_lossyear.tif"))

summaries <- list()
agreement <- list()

for (fp in names(footprints)) {
  cfg <- footprints[[fp]]
  v <- vect(st_transform(st_read(file.path(root, cfg$path), quiet = TRUE), crs(template)))
  grid <- crop(template, v)
  inside <- !is.na(values(rasterize(v, grid), mat = FALSE))

  read_years <- max(TCC_FIRST_YEAR, min(cfg$years) - LOOKBACK):max(cfg$years)
  tcc <- vapply(read_years, function(y) {
    x <- values(crop(rast(tcc_path(y)), grid), mat = FALSE)
    x[x > 100] <- NA
    as.numeric(x)
  }, numeric(ncell(grid)))
  colnames(tcc) <- read_years

  tcc_year <- rep(NA_integer_, ncell(grid))
  for (y in cfg$years) {
    prior <- as.character(max(TCC_FIRST_YEAR, y - LOOKBACK):(y - 1))
    prior_max <- suppressWarnings(apply(tcc[, prior, drop = FALSE], 1, max, na.rm = TRUE))
    prior_max[!is.finite(prior_max)] <- NA
    hit <- is.na(tcc_year) & !is.na(tcc[, as.character(y)]) & !is.na(prior_max) &
      (prior_max - tcc[, as.character(y)]) >= DROP_PP
    tcc_year[hit] <- y
  }
  tcc_year[!inside] <- NA

  hy <- values(crop(hansen, grid), mat = FALSE)
  hansen_year <- ifelse(!is.na(hy) & hy > 0, 2000L + as.integer(hy), NA_integer_)
  hansen_year[!(hansen_year %in% cfg$years) | !inside] <- NA

  out <- rast(grid, nlyrs = 2)
  values(out) <- cbind(tcc_year, hansen_year)
  names(out) <- c("tcc_drop_year", "hansen_loss_year")
  writeRaster(out, file.path(gis_dir, sprintf("drop_year_%s.tif", tolower(fp))),
              overwrite = TRUE, datatype = "INT2S")

  fp_acres <- sum(inside) * px_acres
  summaries[[fp]] <- bind_rows(
    data.frame(source = "TCC drop >= 15 points", year = tcc_year[!is.na(tcc_year)]),
    data.frame(source = "Hansen loss", year = hansen_year[!is.na(hansen_year)])
  ) |>
    count(source, year, name = "n_pixels") |>
    complete(source, year = cfg$years, fill = list(n_pixels = 0)) |>
    mutate(
      footprint = fp,
      acres = n_pixels * px_acres,
      pct_of_footprint = 100 * acres / fp_acres
    )

  t_hit <- !is.na(tcc_year)
  h_hit <- !is.na(hansen_year)
  agreement[[fp]] <- data.frame(
    footprint = fp,
    window = paste0(min(cfg$years), "-", max(cfg$years)),
    footprint_acres = fp_acres,
    acres_tcc = sum(t_hit) * px_acres,
    acres_hansen = sum(h_hit) * px_acres,
    acres_both = sum(t_hit & h_hit) * px_acres,
    pct_hansen_also_tcc = 100 * sum(t_hit & h_hit) / sum(h_hit),
    pct_tcc_also_hansen = 100 * sum(t_hit & h_hit) / sum(t_hit),
    pct_both_same_year_pm1 = 100 * sum(t_hit & h_hit & abs(tcc_year - hansen_year) <= 1) /
      sum(t_hit & h_hit)
  )
}

by_year <- bind_rows(summaries) |> select(footprint, source, year, n_pixels, acres, pct_of_footprint)
agree <- bind_rows(agreement)
write.csv(by_year, file.path(csv_dir, "footprint_drop_year_by_source.csv"), row.names = FALSE)
write.csv(agree, file.path(csv_dir, "footprint_drop_year_agreement.csv"), row.names = FALSE)

print(by_year |> select(-n_pixels) |> mutate(across(c(acres, pct_of_footprint), ~ round(.x, 1))), n = Inf)
print(agree |> mutate(across(where(is.numeric), ~ round(.x, 1))))
