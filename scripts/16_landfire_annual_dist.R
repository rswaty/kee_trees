# LANDFIRE Final Annual Disturbance (2010-2025) by sale zone and year.
# Inputs: inputs/landfire/annual_disturbance/<layer>/ from scripts/15_get_landfire_annual_dist.py
# and cfp_sale_zones.tif from scripts/11_sale_footprint_pixels.R.
# Each annual layer is reprojected (nearest neighbour) onto the TCC grid and its
# raster attribute table supplies DIST_TYPE and SEVERITY. "No Severity" pixels
# are mapped from disturbance records (e.g. harvest polygons) with no detected
# image change; they are kept and flagged.
# Run from project root: Rscript scripts/16_landfire_annual_dist.R

Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(terra)
  library(foreign)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

root <- if (dir.exists("output_csvs") && dir.exists("inputs/landfire/annual_disturbance")) {
  "."
} else {
  stop("Run from the kee_trees project root after scripts/15_get_landfire_annual_dist.py.")
}

lf_dir <- file.path(root, "inputs/landfire/annual_disturbance")
csv_dir <- file.path(root, "output_csvs")
gis_dir <- file.path(root, "output_spatial")
fig_dir <- file.path(root, "output_visuals")

TYPE_GROUP <- c(
  "Clearcut" = "Clearcut",
  "Harvest" = "Harvest",
  "Thinning" = "Thinning",
  "Mastication" = "Other mechanical",
  "Mechanical Add" = "Other mechanical",
  "Mechanical Remove" = "Other mechanical",
  "Other Mechanical" = "Other mechanical",
  "Insects" = "Insects or disease",
  "Disease" = "Insects or disease",
  "Biological" = "Insects or disease",
  "Wildfire" = "Fire",
  "Prescribed Fire" = "Fire",
  "Unknown" = "Unknown",
  "Unknown/Fire Doubtful" = "Unknown",
  "Unknown/Possibly Fire" = "Unknown",
  "Development" = "Other",
  "Chemical" = "Other",
  "Herbicide" = "Other"
)
TYPE_COLS <- c(
  "Clearcut" = "#8c2d04",
  "Harvest" = "#d95f02",
  "Thinning" = "#fdae6b",
  "Other mechanical" = "#bcbddc",
  "Insects or disease" = "#7570b3",
  "Fire" = "#e31a1c",
  "Unknown" = "#08519c",
  "Other" = "grey60"
)

zone_lab <- c(
  "Songbird (TRG to Songbird, 2026)" = "TRG → American Songbird (2026)",
  "Heartlands (TRG to TNC)" = "TRG → The Nature Conservancy (Heartlands)",
  "TRG/Verdant retained" = "TRG → Verdant (same company, renamed)",
  "TRG other sale / left CFP" = "TRG → other owners / left CFP",
  "MWF" = "Molpus → Molpus (mostly kept)",
  "Lyme" = "Lyme → Lyme",
  "Non-institutional kept" = "Other private → same owner",
  "Non-institutional sold / left" = "Other private → new owner / left CFP"
)
zone_col <- setNames(
  c("#d95f02", "#1b9e77", "#7570b3", "#56b4e9", "#e7298a", "#f0c419", "#d2b48c", "#5c3a1e"),
  zone_lab
)
focus <- zone_lab[c(1, 2, 3, 5, 7)]

zr <- rast(file.path(gis_dir, "cfp_sale_zones.tif"))
labels <- read.csv(file.path(csv_dir, "cfp_sale_zone_labels.csv"), stringsAsFactors = FALSE)
px_acres <- prod(res(zr)) / 4046.8564224

z <- values(zr, mat = FALSE)
idx <- which(!is.na(z) & z %in% labels$zone_code)
zone <- labels$zone[match(z[idx], labels$zone_code)]
zone_acres <- tapply(rep(px_acres, length(idx)), zone, sum)

layers <- sort(list.dirs(lf_dir, recursive = FALSE, full.names = TRUE))
layers <- layers[grepl("_Dist[0-9]{2}$", basename(layers))]

annual <- bind_rows(lapply(layers, function(d) {
  year <- 2000L + as.integer(sub(".*_Dist", "", basename(d)))
  message("Year ", year)
  r <- rast(list.files(d, pattern = "\\.tif$", full.names = TRUE)[1])
  vat <- read.dbf(list.files(d, pattern = "vat\\.dbf$", full.names = TRUE)[1], as.is = TRUE)
  v <- values(project(r, zr, method = "near"), mat = FALSE)[idx]
  hit <- !is.na(v) & v > 0
  m <- match(v[hit], vat$Value)
  data.frame(
    zone = zone[hit],
    year = year,
    dist_type = vat$DIST_TYPE[m],
    no_severity = vat$SEVERITY[m] %in% "No Severity"
  )
})) |>
  mutate(type_group = unname(TYPE_GROUP[dist_type]), type_group = coalesce(type_group, "Other"))

by_type <- annual |>
  count(zone, year, type_group, name = "n_pixels") |>
  mutate(acres = n_pixels * px_acres)

by_zone_year <- annual |>
  group_by(zone, year) |>
  summarise(
    n_pixels = n(),
    n_harvest_types = sum(type_group %in% c("Clearcut", "Harvest", "Thinning")),
    n_no_severity = sum(no_severity),
    .groups = "drop"
  ) |>
  complete(zone = names(zone_acres), year = sort(unique(annual$year)),
           fill = list(n_pixels = 0, n_harvest_types = 0, n_no_severity = 0)) |>
  mutate(
    zone_acres = as.numeric(zone_acres[zone]),
    acres = n_pixels * px_acres,
    pct_of_zone = 100 * acres / zone_acres,
    pct_harvest_types = 100 * n_harvest_types * px_acres / zone_acres,
    pct_no_severity = ifelse(n_pixels > 0, 100 * n_no_severity / n_pixels, NA)
  )

write.csv(
  by_type |> mutate(pct_of_zone = 100 * acres / as.numeric(zone_acres[zone])),
  file.path(csv_dir, "cfp_sale_landfire_annual_by_type.csv"),
  row.names = FALSE
)
write.csv(by_zone_year, file.path(csv_dir, "cfp_sale_landfire_annual_by_zone.csv"), row.names = FALSE)

fp_zones <- c("Songbird (TRG to Songbird, 2026)", "Heartlands (TRG to TNC)")
print(
  by_zone_year |>
    filter(zone %in% fp_zones) |>
    select(zone, year, acres, pct_of_zone, pct_harvest_types, pct_no_severity) |>
    mutate(across(where(is.numeric) & !year, ~ round(.x, 1))),
  n = Inf
)

years <- sort(unique(annual$year))

p_fp <- by_type |>
  filter(zone %in% fp_zones) |>
  mutate(
    footprint = factor(zone_lab[zone], levels = zone_lab[fp_zones]),
    type_group = factor(type_group, levels = rev(names(TYPE_COLS)))
  ) |>
  ggplot(aes(year, acres, fill = type_group)) +
  geom_col(width = 0.8) +
  facet_wrap(~footprint, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = TYPE_COLS, breaks = names(TYPE_COLS), drop = TRUE) +
  scale_x_continuous(breaks = years, limits = range(years) + c(-0.6, 0.6)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Acres disturbed each year, by type",
    subtitle = "LANDFIRE Final Annual Disturbance, inside each sale footprint",
    x = NULL, y = "Acres", fill = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    strip.text = element_text(face = "bold", size = 13)
  ) +
  guides(fill = guide_legend(nrow = 1))

ggsave(file.path(fig_dir, "cfp_footprint_landfire_annual_by_type.png"), p_fp,
       width = 9, height = 7, dpi = 150, bg = "white")

p_groups <- by_zone_year |>
  mutate(group = zone_lab[zone]) |>
  filter(group %in% focus) |>
  mutate(group = factor(group, levels = focus)) |>
  ggplot(aes(year, pct_of_zone, color = group)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8) +
  scale_color_manual(values = zone_col) +
  scale_x_continuous(breaks = seq(2010, 2025, 2)) +
  labs(
    title = "Share of each group's land disturbed each year",
    subtitle = "LANDFIRE Final Annual Disturbance, all types",
    x = NULL, y = "% of area disturbed", color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "right")

ggsave(file.path(fig_dir, "cfp_sale_landfire_annual_by_group.png"), p_groups,
       width = 10, height = 5, dpi = 150, bg = "white")

message("Wrote annual LANDFIRE CSVs and figures.")
