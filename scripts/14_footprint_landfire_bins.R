# Acres disturbed in each LANDFIRE years-since-disturbance bin, by disturbance
# type, inside the American Songbird and Keweenaw Heartlands sale footprints.
# LANDFIRE FDist codes (inputs/landfire/lf_hist_dist.tif): hundreds = type, tens = severity
# (ignored here), ones = years-since bin. Approximate calendar years assume a
# 2024 LANDFIRE reference year.
# Run from project root: Rscript scripts/14_footprint_landfire_bins.R

Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

root <- if (dir.exists("output_csvs") && dir.exists("inputs/tcc")) {
  "."
} else {
  stop("Run from the kee_trees project root.")
}

csv_dir <- file.path(root, "output_csvs")

gis_dir <- file.path(root, "output_spatial")
fig_dir <- file.path(root, "output_visuals")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

LF_TYPE <- c(
  "1" = "Fire",
  "2" = "Mechanical add",
  "3" = "Harvest / mechanical removal",
  "5" = "Insects or disease",
  "6" = "Unattributed change"
)
LF_PERIOD <- c(
  "3" = "6–10 years ago\n(~2015–19)",
  "2" = "2–5 years ago\n(~2020–23)",
  "1" = "1 year ago\n(~2024)"
)
LF_PERIOD_YEARS <- c("3" = 5, "2" = 4, "1" = 1)

footprints <- c(
  "American Songbird" = "inputs/boundaries/american_songbird.shp",
  "Keweenaw Heartlands" = "inputs/boundaries/keweenaw_heartlands.shp"
)

template <- rast(file.path(root, "inputs/tcc/HK_TCC_2020.tif"))
px_acres <- prod(res(template)) / 4046.8564224

fp_vect <- lapply(footprints, function(p) {
  vect(st_transform(st_read(file.path(root, p), quiet = TRUE), crs(template)))
})
grid <- crop(template, ext(do.call(rbind, unname(fp_vect))) + 1000)

message("Reprojecting LANDFIRE FDist onto TCC grid...")
lf <- project(rast(file.path(root, "inputs/landfire/lf_hist_dist.tif")), grid, method = "near")

px <- bind_rows(lapply(names(fp_vect), function(fp) {
  v <- lf[cells(lf, fp_vect[[fp]])[, "cell"]][, 1]
  v[!is.na(v) & v <= 0] <- NA
  data.frame(
    footprint = fp,
    lf_type = unname(LF_TYPE[as.character(v %/% 100)]),
    period_code = as.character(v %% 10)
  )
}))

fp_acres <- px |> count(footprint, name = "n") |> mutate(footprint_acres = n * px_acres) |> select(-n)

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

write.csv(
  bins |> mutate(years_since = gsub("\n", " ", years_since)) |> select(-period_code),
  file.path(csv_dir, "cfp_footprint_landfire_bins.csv"),
  row.names = FALSE
)

totals <- bins |>
  group_by(footprint, period_code, years_since, footprint_acres) |>
  summarise(acres = sum(acres), .groups = "drop") |>
  mutate(
    pct_of_footprint = 100 * acres / footprint_acres,
    acres_per_year = acres / LF_PERIOD_YEARS[period_code]
  )
print(totals |> mutate(years_since = gsub("\n", " ", years_since)) |> select(-period_code), n = Inf)

type_cols <- c(
  "Unattributed change" = "#08519c",
  "Harvest / mechanical removal" = "#d95f02",
  "Insects or disease" = "#7570b3",
  "Fire" = "#e31a1c",
  "Mechanical add" = "#66a61e"
)

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

ggsave(file.path(fig_dir, "cfp_footprint_landfire_bins.png"), p, width = 9, height = 7, dpi = 150, bg = "white")
message("Wrote cfp_footprint_landfire_bins.csv and figure.")
