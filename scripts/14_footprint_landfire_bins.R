# Acres disturbed in each LANDFIRE years-since-disturbance bin, by disturbance
# type, inside the American Songbird and Keweenaw Heartlands sale footprints.
# LANDFIRE FDist codes (inputs/landfire/lf_hist_dist.tif): hundreds = type, tens = severity
# (ignored here), ones = years-since bin. Approximate calendar years assume a
# 2024 LANDFIRE reference year.
# Run from project root: Rscript scripts/14_footprint_landfire_bins.R

# ---- Setup ----
# Stop the PROJ library from trying to download projection grids from the internet.
Sys.setenv(PROJ_NETWORK = "OFF")

# terra reads rasters, sf reads shapefiles, dplyr/tidyr reshape tables, ggplot2 draws the chart.
suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

# Make sure the script is being run from the project folder so relative paths work.
root <- if (dir.exists("output_csvs") && dir.exists("inputs/tcc")) {
  "."
} else {
  stop("Run from the kee_trees project root.")
}

# Output folders.
csv_dir <- file.path(root, "output_csvs")

gis_dir <- file.path(root, "output_spatial")
fig_dir <- file.path(root, "output_visuals")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# ---- LANDFIRE code lookups ----
# Disturbance type, keyed by the hundreds digit of the LANDFIRE code.
LF_TYPE <- c(
  "1" = "Fire",
  "2" = "Mechanical add",
  "3" = "Harvest / mechanical removal",
  "5" = "Insects or disease",
  "6" = "Unattributed change"
)
# Years-since-disturbance bin, keyed by the ones digit, with approximate
# calendar years assuming a 2024 reference year.
LF_PERIOD <- c(
  "3" = "6–10 years ago\n(~2015–19)",
  "2" = "2–5 years ago\n(~2020–23)",
  "1" = "1 year ago\n(~2024)"
)
# Number of years each bin spans (5, 4 and 1). Used to turn acres into
# acres per year so bins of different lengths can be compared.
LF_PERIOD_YEARS <- c("3" = 5, "2" = 4, "1" = 1)

# Boundary shapefiles for the two sale footprints.
footprints <- c(
  "American Songbird" = "inputs/boundaries/american_songbird.shp",
  "Keweenaw Heartlands" = "inputs/boundaries/keweenaw_heartlands.shp"
)

# ---- Common grid ----
# Use the Tree Canopy Cover (TCC) 30 m grid as the template so acres match the
# other footprint summaries. 4046.856 square metres = 1 acre.
template <- rast(file.path(root, "inputs/tcc/HK_TCC_2020.tif"))
px_acres <- prod(res(template)) / 4046.8564224

# Read both footprints in the TCC projection, then crop the grid to their
# combined extent plus a 1,000 m buffer to keep reprojection fast.
fp_vect <- lapply(footprints, function(p) {
  vect(st_transform(st_read(file.path(root, p), quiet = TRUE), crs(template)))
})
grid <- crop(template, ext(do.call(rbind, unname(fp_vect))) + 1000)

# Move the LANDFIRE fuel disturbance (FDist) raster onto that grid. "near"
# (nearest neighbour) keeps codes intact instead of averaging them.
message("Reprojecting LANDFIRE FDist onto TCC grid...")
lf <- project(rast(file.path(root, "inputs/landfire/lf_hist_dist.tif")), grid, method = "near")

# ---- Decode cells inside each footprint ----
# One row per cell. Codes of zero or below mean "no disturbance" and become NA.
# Integer division by 100 gives the type digit; the remainder after dividing
# by 10 gives the years-since bin. The tens (severity) digit is ignored.
px <- bind_rows(lapply(names(fp_vect), function(fp) {
  v <- lf[cells(lf, fp_vect[[fp]])[, "cell"]][, 1]
  v[!is.na(v) & v <= 0] <- NA
  data.frame(
    footprint = fp,
    lf_type = unname(LF_TYPE[as.character(v %/% 100)]),
    period_code = as.character(v %% 10)
  )
}))

# Total footprint area in acres (all cells, disturbed or not), used as the
# denominator for percentages.
fp_acres <- px |> count(footprint, name = "n") |> mutate(footprint_acres = n * px_acres) |> select(-n)

# ---- Acres by type and years-since bin ----
# Count disturbed cells per footprint, bin and type; fill missing combinations
# with zero; then convert to acres, percent of footprint and acres per year.
bins <- px |>
  filter(!is.na(lf_type), period_code %in% names(LF_PERIOD)) |>
  count(footprint, period_code, lf_type, name = "n_pixels") |>
  complete(footprint, period_code = names(LF_PERIOD), lf_type, fill = list(n_pixels = 0)) |>
  left_join(fp_acres, by = "footprint") |>
  mutate(
    years_since = LF_PERIOD[period_code],
    acres = n_pixels * px_acres,
    pct_of_footprint = 100 * acres / footprint_acres,
    acres_per_year = acres / LF_PERIOD_YEARS[period_code]
  ) |>
  arrange(footprint, desc(period_code), lf_type)

# Save the table. Line breaks in the bin labels (used for chart axes) are
# replaced with spaces for the CSV.
write.csv(
  bins |> mutate(years_since = gsub("\n", " ", years_since)) |> select(-period_code),
  file.path(csv_dir, "cfp_footprint_landfire_bins.csv"),
  row.names = FALSE
)

# Totals across all disturbance types for each bin. These label the bar tops.
totals <- bins |>
  group_by(footprint, period_code, years_since, footprint_acres) |>
  summarise(acres = sum(acres), .groups = "drop") |>
  mutate(
    pct_of_footprint = 100 * acres / footprint_acres,
    acres_per_year = acres / LF_PERIOD_YEARS[period_code]
  )
print(totals |> mutate(years_since = gsub("\n", " ", years_since)) |> select(-period_code), n = Inf)

# ---- Chart ----
# Colours for each disturbance type.
type_cols <- c(
  "Unattributed change" = "#08519c",
  "Harvest / mechanical removal" = "#d95f02",
  "Insects or disease" = "#7570b3",
  "Fire" = "#e31a1c",
  "Mechanical add" = "#66a61e"
)

# Stacked bars of acres by type for each years-since bin, one panel per
# footprint, with total acres and acres per year printed above each bar.
p <- bins |>
  filter(acres > 0) |>
  mutate(
    years_since = factor(years_since, levels = LF_PERIOD),
    lf_type = factor(lf_type, levels = rev(names(type_cols)))
  ) |>
  ggplot(aes(years_since, acres, fill = lf_type)) +
  geom_col(width = 0.6) +
  geom_text(
    data = totals |> mutate(years_since = factor(years_since, levels = LF_PERIOD)),
    aes(years_since, acres, label = paste0(scales::comma(acres, 1), " ac\n(", scales::comma(acres_per_year, 1), " ac/yr)")),
    inherit.aes = FALSE, vjust = -0.15, size = 3.6, lineheight = 0.9
  ) +
  facet_wrap(~footprint, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = type_cols, breaks = names(type_cols)) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.35))) +
  labs(
    title = "Acres disturbed, by years since disturbance",
    subtitle = "LANDFIRE, inside each sale footprint. Years are approximate; bins span 1, 4 and 5 years.",
    x = NULL, y = "Acres", fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 13)
  )

# Save the chart.
ggsave(file.path(fig_dir, "cfp_footprint_landfire_bins.png"), p, width = 9, height = 7, dpi = 150, bg = "white")
message("Wrote cfp_footprint_landfire_bins.csv and figure.")
