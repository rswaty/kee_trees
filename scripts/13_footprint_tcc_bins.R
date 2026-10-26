# Acres of land in each 10-point canopy cover bin, by year, inside the
# American Songbird and Keweenaw Heartlands sale footprints.
# Canopy cover comes from the USFS Tree Canopy Cover (TCC) rasters (30 m cells).
# Bins are [0,10), [10,20), ..., [90,100]; TCC values 254/255 are no-data and are treated as NA.
# Run from project root: Rscript scripts/13_footprint_tcc_bins.R

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

# Input and output folders.
tcc_dir <- file.path(root, "inputs/tcc")
csv_dir <- file.path(root, "output_csvs")
gis_dir <- file.path(root, "output_spatial")
fig_dir <- file.path(root, "output_visuals")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# ---- Settings ----
# Years of TCC to summarize, the two years compared in the chart, and the
# 10-percentage-point canopy cover bins (labels look like "0–10%").
TCC_YEARS <- 2010:2025
COMPARE_YEARS <- c(2020, 2025)
BIN_BREAKS <- seq(0, 100, 10)
BIN_LABELS <- paste0(head(BIN_BREAKS, -1), "–", BIN_BREAKS[-1], "%")

# Boundary shapefiles for the two sale footprints.
footprints <- c(
  "American Songbird" = "inputs/boundaries/american_songbird.shp",
  "Keweenaw Heartlands" = "inputs/boundaries/keweenaw_heartlands.shp"
)

# ---- Footprint cells ----
# All TCC years share one grid, so the first year serves as a template.
# Each 30 m x 30 m cell is 900 square metres; dividing by 4046.856 square
# metres per acre gives about 0.22 acres per cell.
tcc_path <- function(y) file.path(tcc_dir, sprintf("HK_TCC_%d.tif", y))
template <- rast(tcc_path(TCC_YEARS[1]))
px_acres <- prod(res(template)) / 4046.8564224

# For each footprint, reproject the boundary to the TCC grid and list the
# cell numbers that fall inside it. These cell lists are reused for every year.
fp_cells <- lapply(footprints, function(p) {
  v <- vect(st_transform(st_read(file.path(root, p), quiet = TRUE), crs(template)))
  cells(template, v)[, "cell"]
})

# ---- Bin canopy cover by year ----
# For each year and footprint, pull the canopy values of the footprint cells,
# set values above 100 (the 254/255 no-data codes) to NA, and assign each
# cell to a canopy cover bin.
message("Reading annual TCC inside footprints...")
bins <- bind_rows(lapply(TCC_YEARS, function(y) {
  r <- rast(tcc_path(y))
  bind_rows(lapply(names(fp_cells), function(fp) {
    v <- r[fp_cells[[fp]]][, 1]
    v[v > 100] <- NA
    data.frame(
      footprint = fp,
      year = y,
      bin = cut(v, BIN_BREAKS, labels = BIN_LABELS, right = FALSE, include.lowest = TRUE)
    )
  }))
})) |>
  # Drop no-data cells, count cells per bin (keeping empty bins as zero),
  # convert to acres, and express each bin as a percent of the footprint.
  filter(!is.na(bin)) |>
  count(footprint, year, bin, name = "n_pixels", .drop = FALSE) |>
  mutate(acres = n_pixels * px_acres) |>
  group_by(footprint, year) |>
  mutate(pct_of_footprint = 100 * acres / sum(acres)) |>
  ungroup()

write.csv(bins, file.path(csv_dir, "cfp_footprint_tcc_bins_by_year.csv"), row.names = FALSE)

# ---- 2020 vs. 2025 comparison ----
# Put the two comparison years side by side and compute the change in acres
# for each bin (later year minus earlier year).
compare <- bins |>
  filter(year %in% COMPARE_YEARS) |>
  select(footprint, bin, year, acres) |>
  pivot_wider(names_from = year, values_from = acres, names_prefix = "acres_") |>
  mutate(change_acres = .data[[paste0("acres_", COMPARE_YEARS[2])]] - .data[[paste0("acres_", COMPARE_YEARS[1])]])

write.csv(compare, file.path(csv_dir, "cfp_footprint_tcc_bins_2020_2025.csv"), row.names = FALSE)
print(compare, n = Inf)

# ---- Chart ----
# Grouped bar chart: acres per canopy bin, 2020 next to 2025, one panel per footprint.
p <- bins |>
  filter(year %in% COMPARE_YEARS) |>
  mutate(year = factor(year)) |>
  ggplot(aes(bin, acres, fill = year)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  facet_wrap(~footprint, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("2020" = "#9ecae1", "2025" = "#08519c")) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Acres in each canopy cover bin, 2020 vs. 2025",
    subtitle = "USFS Tree Canopy Cover, 30 m cells inside each sale footprint",
    x = "Canopy cover", y = "Acres", fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 13)
  )

# Save the chart.
ggsave(file.path(fig_dir, "cfp_footprint_tcc_bins_2020_2025.png"), p, width = 9, height = 7, dpi = 150, bg = "white")
message("Wrote cfp_footprint_tcc_bins_by_year.csv, cfp_footprint_tcc_bins_2020_2025.csv and figure.")
