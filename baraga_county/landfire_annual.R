# LANDFIRE Final Annual Disturbance on Baraga Commercial Forest parcels, 2010-2025.
# Uses the Houghton-Keweenaw downloads in inputs/landfire/annual_disturbance/
# (the bounding box already covers Baraga) and the 2020 parcel destinations
# from analyze_baraga_cfp.R. Writes only under baraga_county/.
# Run from the project root: Rscript baraga_county/landfire_annual.R

Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(foreign)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})
terraOptions(progress = 0)
sf_use_s2(FALSE)

root <- if (file.exists("data/cfp_data/cfp_2020_mi.shp")) "." else stop("Run from the kee_trees project root.")
lf_dir <- file.path(root, "inputs/landfire/annual_disturbance")
fig_dir <- file.path(root, "baraga_county/figures")
tab_dir <- file.path(root, "baraga_county/tables")

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

message("Rasterizing 2020 parcels...")
tcc <- rast(file.path(root, "baraga_county/tcc/Baraga_TCC_2020.tif"))
parcels <- st_read(file.path(root, "data/cfp_data/cfp_2020_mi.shp"), quiet = TRUE) |>
  filter(County_Nam == "Baraga") |>
  st_transform(crs(tcc)) |>
  st_make_valid() |>
  st_buffer(0) |>
  st_make_valid()
parcels$parid <- as.character(parcels$parid)
dest <- read.csv(file.path(tab_dir, "parcel_2020_to_2026.csv"), colClasses = c(parid = "character"))
parcels <- parcels |>
  left_join(dest |> select(parid, went_to), by = "parid") |>
  filter(!is.na(went_to))

groups <- parcels |>
  st_drop_geometry() |>
  distinct(went_to) |>
  arrange(went_to) |>
  pull(went_to)
parcels$code <- match(parcels$went_to, groups)
zone <- rasterize(vect(parcels), tcc, field = "code")
px <- prod(res(tcc)) / 4046.8564224
zone_cells <- freq(zone)
zone_acres <- setNames(zone_cells$count * px, groups[zone_cells$value])

layers <- list.dirs(lf_dir, recursive = FALSE, full.names = TRUE)
layers <- layers[grepl("_Dist[0-9]{2}$", basename(layers))]

message("Reading disturbance years...")
annual <- bind_rows(lapply(layers, function(d) {
  year <- 2000L + as.integer(sub(".*_Dist", "", basename(d)))
  message("  ", year)
  r <- rast(list.files(d, pattern = "\\.tif$", full.names = TRUE)[1])
  vat <- read.dbf(list.files(d, pattern = "vat\\.dbf$", full.names = TRUE)[1], as.is = TRUE)
  v <- values(project(r, tcc, method = "near"), mat = FALSE)
  z <- values(zone, mat = FALSE)
  hit <- !is.na(z) & !is.na(v) & v > 0
  if (!any(hit)) return(NULL)
  m <- match(v[hit], vat$Value)
  data.frame(
    went_to = groups[z[hit]],
    year = year,
    dist_type = vat$DIST_TYPE[m],
    no_severity = vat$SEVERITY[m] %in% "No Severity"
  )
})) |>
  mutate(
    type_group = unname(TYPE_GROUP[dist_type]),
    type_group = coalesce(type_group, "Other")
  )

by_type <- annual |>
  count(year, type_group, name = "n_pixels") |>
  complete(year = 2010:2025, type_group = names(TYPE_COLS), fill = list(n_pixels = 0)) |>
  mutate(acres = round(n_pixels * px, 1))
write.csv(by_type, file.path(tab_dir, "landfire_by_year_type.csv"), row.names = FALSE)

by_group <- annual |>
  group_by(went_to, year) |>
  summarise(
    n_pixels = n(),
    n_harvest = sum(type_group %in% c("Clearcut", "Harvest", "Thinning")),
    n_no_severity = sum(no_severity),
    .groups = "drop"
  ) |>
  complete(went_to = groups, year = 2010:2025,
           fill = list(n_pixels = 0, n_harvest = 0, n_no_severity = 0)) |>
  mutate(
    group_acres = round(as.numeric(zone_acres[went_to]), 1),
    acres = round(n_pixels * px, 1),
    harvest_acres = round(n_harvest * px, 1),
    pct_of_group = round(100 * n_pixels * px / as.numeric(zone_acres[went_to]), 2),
    pct_no_severity = ifelse(n_pixels > 0, round(100 * n_no_severity / n_pixels, 1), NA)
  )
write.csv(by_group, file.path(tab_dir, "landfire_by_destination_year.csv"), row.names = FALSE)

mapped <- round(sum(zone_acres), 0)
dist_acres <- round(sum(annual$year >= 0) * px, 0)
harvest_acres <- round(sum(annual$type_group %in% c("Clearcut", "Harvest", "Thinning")) * px, 0)
recent <- annual$year >= 2020
message(
  "Mapped acres ", mapped,
  "  disturbed cell-acres 2010-2025 ", dist_acres,
  "  of which harvest types ", harvest_acres,
  "  2020-2025 ", round(sum(recent) * px, 0)
)
print(
  by_type |>
    group_by(type_group) |>
    summarise(acres = sum(acres), .groups = "drop") |>
    arrange(desc(acres))
)

font <- "sans"
years <- 2010:2025
p_type <- by_type |>
  mutate(type_group = factor(type_group, levels = rev(names(TYPE_COLS)))) |>
  ggplot(aes(year, acres, fill = type_group)) +
  geom_col(width = 0.82) +
  scale_fill_manual(values = TYPE_COLS, breaks = names(TYPE_COLS), drop = FALSE) +
  scale_x_continuous(breaks = years) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.06)), labels = scales::comma) +
  labs(
    title = "Acres disturbed each year on Baraga Commercial Forest land",
    subtitle = "LANDFIRE Final Annual Disturbance, 2010–2025, inside 2020 enrolled parcels",
    x = NULL, y = "Acres", fill = NULL
  ) +
  theme_minimal(base_size = 12, base_family = font) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 13)
  ) +
  guides(fill = guide_legend(nrow = 2))
ggsave(file.path(fig_dir, "05_landfire_by_year.png"), p_type, width = 9.2, height = 4.8, dpi = 160, bg = "white")

focus <- c(
  "Weyerhaeuser and Great Lakes Forests to Lyme",
  "TRG to Verdant",
  "Molpus kept",
  "The Nature Conservancy kept",
  "Other owners kept",
  "Other owners sold or left"
)
focus_col <- c(
  "Weyerhaeuser and Great Lakes Forests to Lyme" = "#1c3a2e",
  "TRG to Verdant" = "#d97706",
  "Molpus kept" = "#8c4a32",
  "The Nature Conservancy kept" = "#2f5d8c",
  "Other owners kept" = "#6d9484",
  "Other owners sold or left" = "#c4a06a"
)
p_group <- by_group |>
  filter(went_to %in% focus) |>
  mutate(went_to = factor(went_to, levels = focus)) |>
  ggplot(aes(year, pct_of_group, color = went_to)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  scale_color_manual(values = focus_col) +
  scale_x_continuous(breaks = seq(2010, 2025, 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Share of each group's land disturbed each year",
    subtitle = "All disturbance types. Percent of that group's mapped acres.",
    x = NULL, y = "Percent of acres", color = NULL
  ) +
  theme_minimal(base_size = 12, base_family = font) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 13)
  )
ggsave(file.path(fig_dir, "06_landfire_by_destination.png"), p_group, width = 9.2, height = 4.6, dpi = 160, bg = "white")
message("Wrote LANDFIRE figures")
