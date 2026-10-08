# Biophysical Settings (LF2020 BpS), Existing Vegetation Type (LF2024 EVT), and
# historical vs. current annual disturbance for all land in Houghton and
# Keweenaw counties (mainland; Isle Royale is outside the LANDFIRE AOI).
#
# Historical: expected share of each BpS disturbed per year =
#   sum over succession classes of (reference % of class) x (annual probability
#   of each disturbance transition from that class), from the BpS models
#   (inputs/bps_models/, exported from the LANDFIRE BpS database).
#   Alternative Succession and Competition/Maintenance are not disturbances.
# Current: LANDFIRE Final Annual Disturbance, 2010-2025 (inputs/landfire/annual_disturbance/).
# Open water is excluded from all areas.
# Run from project root: Rscript scripts/18_bps_evt_disturbance_regimes.R

Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(foreign)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

root <- if (dir.exists("inputs/landfire") && dir.exists("inputs/landfire/annual_disturbance")) {
  "."
} else {
  stop("Run from the kee_trees project root.")
}

veg_dir <- file.path(root, "inputs/landfire")
dist_dir <- file.path(root, "inputs/landfire/annual_disturbance")
model_dir <- file.path(root, "inputs/bps_models")
csv_dir <- file.path(root, "output_csvs")
gis_dir <- file.path(root, "output_spatial")
fig_dir <- file.path(root, "output_visuals")

ACRES_PER_PX <- 900 / 4046.8564224
TOP_N <- 10

HIST_GROUP <- c(
  "Replacement Fire" = "Fire",
  "Mixed Fire" = "Fire",
  "Surface Fire" = "Fire",
  "Wind or Weather or Stress" = "Wind, weather or stress",
  "Insects or Disease" = "Insects or disease",
  "Native Grazing" = "Other",
  "Optional 1" = "Other",
  "Optional 2" = "Other"
)
CUR_GROUP <- c(
  "Clearcut" = "Harvest or mechanical",
  "Harvest" = "Harvest or mechanical",
  "Thinning" = "Harvest or mechanical",
  "Mastication" = "Harvest or mechanical",
  "Mechanical Add" = "Harvest or mechanical",
  "Mechanical Remove" = "Harvest or mechanical",
  "Other Mechanical" = "Harvest or mechanical",
  "Wildfire" = "Fire",
  "Prescribed Fire" = "Fire",
  "Insects" = "Insects or disease",
  "Disease" = "Insects or disease",
  "Biological" = "Insects or disease",
  "Unknown" = "Unknown cause",
  "Unknown/Fire Doubtful" = "Unknown cause",
  "Unknown/Possibly Fire" = "Unknown cause",
  "Development" = "Other",
  "Chemical" = "Other",
  "Herbicide" = "Other"
)
GROUP_COLS <- c(
  "Fire" = "#e31a1c",
  "Wind, weather or stress" = "#41b6c4",
  "Insects or disease" = "#7570b3",
  "Harvest or mechanical" = "#d95f02",
  "Unknown cause" = "#08519c",
  "Other" = "grey65"
)

read_lf <- function(dir) {
  r <- rast(list.files(dir, pattern = "\\.tif$", full.names = TRUE)[1])
  vat <- read.dbf(list.files(dir, pattern = "vat\\.dbf$", full.names = TRUE)[1], as.is = TRUE)
  list(r = r, vat = vat)
}

bps <- read_lf(file.path(veg_dir, "LF2020_BPS"))
evt <- read_lf(file.path(veg_dir, "LF2024_EVT"))

counties <- st_read(file.path(root, "inputs/boundaries/houghton_keweenaw_counties.shp"), quiet = TRUE) |>
  st_transform(crs(bps$r))
inside <- !is.na(values(rasterize(vect(counties), bps$r), mat = FALSE))

bps_v <- values(bps$r, mat = FALSE)
evt_v <- values(evt$r, mat = FALSE)
water <- bps$vat$Value[bps$vat$BPS_NAME == "Open Water"]
land <- inside & !is.na(bps_v) & !(bps_v %in% water)
land_acres <- sum(land) * ACRES_PER_PX

# ---- BpS and EVT summaries ----
bps_tab <- data.frame(Value = bps_v[land]) |>
  count(Value, name = "n") |>
  left_join(bps$vat |> select(Value, BPS_MODEL, BPS_NAME, GROUPVEG, FRI_ALLFIR), by = "Value") |>
  mutate(acres = n * ACRES_PER_PX)

bps_by_name <- bps_tab |>
  group_by(BPS_NAME, GROUPVEG) |>
  summarise(acres = sum(acres), .groups = "drop") |>
  mutate(pct = 100 * acres / land_acres) |>
  arrange(desc(acres))

evt_by_name <- data.frame(Value = evt_v[land]) |>
  count(Value, name = "n") |>
  left_join(evt$vat |> select(Value, EVT_NAME, EVT_PHYS), by = "Value") |>
  filter(EVT_NAME != "Open Water") |>
  group_by(EVT_NAME, EVT_PHYS) |>
  summarise(acres = sum(n) * ACRES_PER_PX, .groups = "drop") |>
  mutate(pct = 100 * acres / land_acres) |>
  arrange(desc(acres))

write.csv(bps_by_name, file.path(csv_dir, "hk_bps_acres.csv"), row.names = FALSE)
write.csv(evt_by_name, file.path(csv_dir, "hk_evt_acres.csv"), row.names = FALSE)

# ---- Historical annual disturbance from BpS models ----
ref <- read.csv(file.path(model_dir, "ref_con_long.csv"), stringsAsFactors = FALSE) |>
  filter(ref_label %in% LETTERS[1:5]) |>
  select(bps_model_id, ref_label, ref_percent)
scls <- read.csv(file.path(model_dir, "scls_descriptions.csv"), stringsAsFactors = FALSE) |>
  select(bps_model_id, ref_label, state_class_id)
prob <- read.csv(file.path(model_dir, "probabilistic.csv"), stringsAsFactors = FALSE) |>
  filter(transition_type_id %in% names(HIST_GROUP))

# class_change = the transition moves the stand to a different succession class
# (structure-changing); otherwise it is a light, maintenance-type disturbance.
model_rates <- prob |>
  inner_join(scls, by = c("bps_model_id", "state_class_source" = "state_class_id")) |>
  inner_join(ref, by = c("bps_model_id", "ref_label")) |>
  mutate(type_group = HIST_GROUP[transition_type_id],
         class_change = state_class_source != state_class_to,
         annual_frac = coalesce(ref_percent, 0) / 100 * probability) |>
  group_by(bps_model_id, type_group, class_change) |>
  summarise(annual_frac = sum(annual_frac), .groups = "drop")

hist_by_bps <- bps_tab |>
  group_by(BPS_MODEL, BPS_NAME) |>
  summarise(acres = sum(acres), .groups = "drop") |>
  inner_join(model_rates, by = c("BPS_MODEL" = "bps_model_id"), relationship = "many-to-many") |>
  mutate(annual_acres = acres * annual_frac)

HIST_ALL <- "Historical: all disturbances"
HIST_CHG <- "Historical: structure-changing only"
hist_by_type <- bind_rows(
  hist_by_bps |> mutate(period = HIST_ALL),
  hist_by_bps |> filter(class_change) |> mutate(period = HIST_CHG)
) |>
  group_by(period, type_group) |>
  summarise(annual_acres = sum(annual_acres), .groups = "drop")

write.csv(hist_by_bps, file.path(csv_dir, "hk_historical_disturbance_by_bps.csv"), row.names = FALSE)

# ---- Current annual disturbance from LANDFIRE ----
layers <- sort(list.dirs(dist_dir, recursive = FALSE, full.names = TRUE))
layers <- layers[grepl("_Dist[0-9]{2}$", basename(layers))]

cur_by_year <- bind_rows(lapply(layers, function(d) {
  x <- read_lf(d)
  v <- values(x$r, mat = FALSE)
  hit <- land & !is.na(v) & v > 0
  data.frame(Value = v[hit]) |>
    count(Value, name = "n") |>
    left_join(x$vat |> select(Value, DIST_TYPE), by = "Value") |>
    mutate(year = 2000L + as.integer(sub(".*_Dist", "", basename(d))))
})) |>
  mutate(type_group = coalesce(unname(CUR_GROUP[DIST_TYPE]), "Other")) |>
  group_by(year, type_group) |>
  summarise(annual_acres = sum(n) * ACRES_PER_PX, .groups = "drop")

write.csv(cur_by_year, file.path(csv_dir, "hk_current_disturbance_by_year.csv"), row.names = FALSE)

cur_years <- range(cur_by_year$year)
cur_mean <- cur_by_year |>
  complete(year = cur_years[1]:cur_years[2], type_group, fill = list(annual_acres = 0)) |>
  group_by(type_group) |>
  summarise(annual_acres = mean(annual_acres), .groups = "drop") |>
  mutate(period = sprintf("Current (LANDFIRE average, %d–%d)", cur_years[1], cur_years[2]))

summary_tab <- bind_rows(hist_by_type, cur_mean) |>
  mutate(pct_of_land = 100 * annual_acres / land_acres)
write.csv(summary_tab, file.path(csv_dir, "hk_disturbance_historical_vs_current.csv"), row.names = FALSE)

totals <- summary_tab |>
  group_by(period) |>
  summarise(annual_acres = sum(annual_acres), pct_of_land = sum(pct_of_land), .groups = "drop") |>
  mutate(rotation_years = 100 / pct_of_land)
cur_periods <- cur_by_year |>
  group_by(year) |>
  summarise(annual_acres = sum(annual_acres), .groups = "drop") |>
  mutate(period = ifelse(year < 2020, "Current (LANDFIRE average, 2010–2019)", "Current (LANDFIRE average, 2020–2025)")) |>
  group_by(period) |>
  summarise(annual_acres = mean(annual_acres), .groups = "drop") |>
  mutate(pct_of_land = 100 * annual_acres / land_acres, rotation_years = 100 / pct_of_land)
totals <- bind_rows(totals, cur_periods)
write.csv(totals, file.path(csv_dir, "hk_disturbance_totals.csv"), row.names = FALSE)
totals <- totals |> filter(period %in% summary_tab$period)

message(sprintf("Land area (excluding open water): %s acres", format(round(land_acres), big.mark = ",")))
print(bps_by_name |> head(TOP_N) |> mutate(across(c(acres, pct), ~ round(.x, 1))))
print(evt_by_name |> head(TOP_N) |> mutate(across(c(acres, pct), ~ round(.x, 1))))
print(summary_tab |> mutate(across(c(annual_acres, pct_of_land), ~ round(.x, 2))))
print(totals |> mutate(across(where(is.numeric), ~ round(.x, 2))))

# ---- Figures ----
theme_fig <- theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), plot.title.position = "plot",
        plot.title = element_text(face = "bold"), plot.subtitle = element_text(color = "grey35"))

PHYS_COLS <- c(
  "Hardwood" = "#66a61e",
  "Conifer" = "#1b7837",
  "Hardwood-Conifer" = "#a6d96a",
  "Conifer-Hardwood" = "#a6d96a",
  "Riparian" = "#41b6c4",
  "Agricultural" = "#e6ab02",
  "Developed-Roads" = "grey55",
  "Developed" = "grey55",
  "Exotic Herbaceous" = "#d95f02",
  "Shrubland" = "#a6761d",
  "Grassland" = "#fdd49e",
  "Sparsely Vegetated" = "#d9d9d9"
)

bar_chart <- function(d, name_col, fill_col, title, subtitle, file) {
  d <- head(d, TOP_N) |> mutate(name = reorder(.data[[name_col]], acres))
  p <- ggplot(d, aes(acres, name, fill = .data[[fill_col]])) +
    geom_col(width = 0.7) +
    geom_text(aes(label = paste0(round(pct, 1), "%")), hjust = -0.1, size = 3.8) +
    scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
    scale_y_discrete(labels = function(x) stringr::str_wrap(x, 45)) +
    scale_fill_manual(values = PHYS_COLS) +
    labs(title = title, subtitle = subtitle, x = "Acres", y = NULL, fill = NULL) +
    theme_fig + theme(legend.position = "bottom", panel.grid.major.y = element_blank())
  ggsave(file.path(fig_dir, file), p, width = 10, height = 6, dpi = 150, bg = "white")
}

bar_chart(bps_by_name, "BPS_NAME", "GROUPVEG",
          "Top Biophysical Settings: what the land would naturally support",
          "LANDFIRE 2020 BpS, Houghton and Keweenaw counties (mainland), percent of land area",
          "hk_bps_top10.png")
bar_chart(evt_by_name, "EVT_NAME", "EVT_PHYS",
          "Top Existing Vegetation Types: what's there now",
          "LANDFIRE 2024 EVT, Houghton and Keweenaw counties (mainland), percent of land area",
          "hk_evt_top10.png")

hist_all <- totals$annual_acres[totals$period == HIST_ALL]
hist_chg <- totals$annual_acres[totals$period == HIST_CHG]
period_levels <- c(HIST_ALL, HIST_CHG, unique(cur_mean$period))
period_labels <- setNames(stringr::str_wrap(period_levels, 20), period_levels)

p_cmp <- summary_tab |>
  mutate(
    period = factor(period, levels = period_levels),
    type_group = factor(type_group, levels = rev(names(GROUP_COLS)))
  ) |>
  ggplot(aes(period, annual_acres, fill = type_group)) +
  geom_col(width = 0.6) +
  geom_text(
    data = totals |> mutate(period = factor(period, levels = period_levels)),
    aes(period, annual_acres, label = paste0(scales::comma(annual_acres, 1), " acres/yr\n(", round(pct_of_land, 2), "% of land)")),
    inherit.aes = FALSE, vjust = -0.2, size = 4
  ) +
  scale_x_discrete(labels = period_labels) +
  scale_fill_manual(values = GROUP_COLS, breaks = names(GROUP_COLS)) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.2))) +
  labs(
    title = "Disturbance per year: historical vs. current",
    subtitle = "All land, Houghton and Keweenaw counties (mainland). Historical: BpS models; current: LANDFIRE.",
    x = NULL, y = "Acres disturbed per year", fill = NULL
  ) +
  theme_fig + theme(legend.position = "right", panel.grid.major.x = element_blank())
ggsave(file.path(fig_dir, "hk_disturbance_historical_vs_current.png"), p_cmp, width = 10, height = 6, dpi = 150, bg = "white")

p_year <- cur_by_year |>
  mutate(type_group = factor(type_group, levels = rev(names(GROUP_COLS)))) |>
  ggplot(aes(year, annual_acres, fill = type_group)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = hist_all, linetype = "dotted", linewidth = 0.8) +
  geom_hline(yintercept = hist_chg, linetype = "dashed", linewidth = 0.8) +
  annotate("label", x = cur_years[1] - 0.4, y = hist_all, hjust = 0, size = 3.6, fill = "white", linewidth = 0,
           label = paste0("Historical, all disturbances: ", scales::comma(hist_all, 1), " acres/yr")) +
  annotate("label", x = cur_years[1] - 0.4, y = hist_chg, hjust = 0, size = 3.6, fill = "white", linewidth = 0,
           label = paste0("Historical, structure-changing: ", scales::comma(hist_chg, 1), " acres/yr")) +
  scale_fill_manual(values = GROUP_COLS, breaks = names(GROUP_COLS), drop = TRUE) +
  scale_x_continuous(breaks = seq(cur_years[1], cur_years[2], 1)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Acres disturbed each year vs. the historical expectation",
    subtitle = "LANDFIRE Annual Disturbance, all land, Houghton and Keweenaw counties (mainland)",
    x = NULL, y = "Acres disturbed", fill = NULL
  ) +
  theme_fig + theme(legend.position = "right", panel.grid.major.x = element_blank(),
                    axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(fig_dir, "hk_disturbance_by_year_vs_historical.png"), p_year, width = 10, height = 5.5, dpi = 150, bg = "white")

message("Wrote BpS/EVT and disturbance CSVs and figures.")
