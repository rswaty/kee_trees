# LANDFIRE disturbance cause/severity and annual tree canopy cover (TCC) trajectories
# by sale zone.
# Zones come from scripts/11_sale_footprint_pixels.R (cfp_sale_zones.tif).
# LANDFIRE FDist codes (inputs/landfire/lf_hist_dist.tif): hundreds = type, tens = severity,
# ones = years-since bin (1, 2-5, 6-10 years). Approximate calendar years in
# figure labels assume a 2024 LANDFIRE reference year.
# TCC disturbance year = first year canopy falls >= TCC_DROP_PP below the max of
# the previous TCC_LOOKBACK years.
# Hansen = Hansen Global Forest Change loss year. All rasters are 30 m cells.
# Run from project root: Rscript scripts/12_sale_footprint_landfire_tcc.R
# Does not touch Shiny.

# Stop the PROJ map-projection library from trying to download grid files.
Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(tidyr)
})

# Figures are optional: skip them if ggplot2 is not installed.
has_ggplot <- requireNamespace("ggplot2", quietly = TRUE)
if (has_ggplot) {
  suppressPackageStartupMessages(library(ggplot2))
}

# ---- Setup ----
# Paths are relative to the project folder, so stop early if run from elsewhere.
root <- if (dir.exists("output_csvs") && dir.exists("inputs/tcc")) {
  "."
} else {
  stop("Run from the kee_trees project root.")
}

tcc_dir <- file.path(root, "inputs/tcc")
csv_dir <- file.path(root, "output_csvs")
gis_dir <- file.path(root, "output_spatial")
fig_dir <- file.path(root, "output_visuals")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# The zone raster must already exist (made by script 11).
zone_path <- file.path(gis_dir, "cfp_sale_zones.tif")
if (!file.exists(zone_path)) {
  stop("Missing ", zone_path, " — run scripts/11_sale_footprint_pixels.R first.")
}

# Annual TCC years to read, and the rule for flagging a disturbance year: a drop
# of at least 20 percentage points below the highest value of the previous 3
# years. Comparing with a recent maximum, rather than last year only, also
# catches canopy loss that shows up over two or three years.
TCC_YEARS <- 2010:2025
TCC_DROP_PP <- 20
TCC_LOOKBACK <- 3

# Lookup tables for decoding LANDFIRE disturbance codes. Example: 332 = harvest
# (3), high severity (3), 2-5 years ago (2).
LF_TYPE <- c(
  "1" = "Fire",
  "2" = "Mechanical add",
  "3" = "Harvest / mechanical removal",
  "5" = "Insects or disease",
  "6" = "Unattributed change"
)
LF_SEVERITY <- c("1" = "Low", "2" = "Moderate", "3" = "High")
LF_PERIOD <- c("1" = "1 yr ago", "2" = "2-5 yrs ago", "3" = "6-10 yrs ago")
# Length of each period in years, used to turn totals into per-year rates so
# the unequal periods can be compared.
LF_PERIOD_YEARS <- c("1 yr ago" = 1, "2-5 yrs ago" = 4, "6-10 yrs ago" = 5)
# Axis labels with approximate calendar years (assuming a 2024 reference year).
LF_PERIOD_AXIS <- c(
  "6-10 yrs ago" = "6-10 yrs ago\n(~2015-19)",
  "2-5 yrs ago" = "2-5 yrs ago\n(~2020-23)",
  "1 yr ago" = "1 yr ago\n(~2024)"
)

# ---- Load zones ----
# Zone raster, its code-to-name table, and the area of one cell in acres
# (cell area in square metres divided by 4046.856 square metres per acre).
zr <- rast(zone_path)
labels <- read.csv(file.path(csv_dir, "cfp_sale_zone_labels.csv"), stringsAsFactors = FALSE)
px_acres <- prod(res(zr)) / 4046.8564224

# Put LANDFIRE on the same grid as the zones. "near" (nearest neighbour) keeps the
# codes intact; averaging would produce meaningless code values.
message("Reprojecting LANDFIRE FDist onto TCC grid...")
lf <- project(rast(file.path(root, "inputs/landfire/lf_hist_dist.tif")), zr, method = "near")

# Work only with cells inside a named zone (code 0, outside the parcels, is
# not in the label table and is dropped). idx holds those cell positions.
z <- values(zr, mat = FALSE)
idx <- which(!is.na(z) & z %in% labels$zone_code)
zone <- labels$zone[match(z[idx], labels$zone_code)]
zone_acres <- tapply(rep(px_acres, length(idx)), zone, sum)

# Decode LANDFIRE codes into type, severity and period. Zero or negative values
# mean no recorded disturbance and become NA.
lfv <- values(lf, mat = FALSE)[idx]
lfv[!is.na(lfv) & lfv <= 0] <- NA
lf_type <- unname(LF_TYPE[as.character(lfv %/% 100)])
lf_sev <- unname(LF_SEVERITY[as.character((lfv %/% 10) %% 10)])
lf_period <- unname(LF_PERIOD[as.character(lfv %% 10)])

# Hansen loss year: stored as 1-24 for 2001-2024, 0 for no loss.
hy <- values(rast(file.path(gis_dir, "hansen_lossyear.tif")), mat = FALSE)[idx]
hansen_year <- ifelse(!is.na(hy) & hy > 0, 2000L + as.integer(hy), NA_integer_)

# ---- Annual canopy and TCC disturbance year ----
# Matrix of canopy values: one row per zone cell, one column per year. Values of
# 254 and above are no-data codes and become NA.
message("Reading annual TCC for zone pixels...")
tcc <- vapply(TCC_YEARS, function(y) {
  v <- values(rast(file.path(tcc_dir, sprintf("HK_TCC_%d.tif", y))), mat = FALSE)[idx]
  v[v >= 254] <- NA
  as.numeric(v)
}, numeric(length(idx)))
colnames(tcc) <- TCC_YEARS

# First year with a TCC_DROP_PP drop below the prior TCC_LOOKBACK-year max
# The loop starts in the first year that has a full lookback window (2013).
# Once a cell is flagged it keeps that first year.
tcc_dist_year <- rep(NA_integer_, length(idx))
for (j in (TCC_LOOKBACK + 1):length(TCC_YEARS)) {
  prior_max <- suppressWarnings(
    apply(tcc[, (j - TCC_LOOKBACK):(j - 1), drop = FALSE], 1, max, na.rm = TRUE)
  )
  prior_max[!is.finite(prior_max)] <- NA
  hit <- is.na(tcc_dist_year) & !is.na(tcc[, j]) & !is.na(prior_max) &
    (prior_max - tcc[, j]) >= TCC_DROP_PP
  tcc_dist_year[hit] <- TCC_YEARS[j]
}

# One row per zone cell with all the signals side by side.
px <- data.frame(
  zone = zone,
  lf_type = lf_type,
  lf_sev = lf_sev,
  lf_period = lf_period,
  hansen_year = hansen_year,
  tcc_dist_year = tcc_dist_year,
  tcc_2020 = tcc[, "2020"],
  tcc_2025 = tcc[, "2025"],
  stringsAsFactors = FALSE
)

# ---- Summaries ----
# Adds the zone's total acres and converts acres to percent of the zone.
pct_of_zone <- function(d) {
  d |> mutate(zone_acres = as.numeric(zone_acres[zone]), pct_of_zone = 100 * acres / zone_acres)
}

# LANDFIRE-disturbed area per zone by cause, and by cause x severity x period.
lf_by_type <- px |>
  filter(!is.na(lf_type)) |>
  count(zone, lf_type, name = "n") |>
  mutate(acres = n * px_acres) |>
  pct_of_zone()

lf_by_type_sev_period <- px |>
  filter(!is.na(lf_type)) |>
  count(zone, lf_type, lf_sev, lf_period, name = "n") |>
  mutate(acres = n * px_acres) |>
  pct_of_zone()

# What LANDFIRE calls the pixels Hansen and TCC flag as recent loss
# A cell can be counted under both signals. pct_of_signal is the share of each
# signal's flagged cells in that zone given each LANDFIRE cause.
attribution <- bind_rows(
  px |>
    filter(!is.na(hansen_year), hansen_year >= 2020) |>
    mutate(signal = "Hansen loss 2020-2024"),
  px |>
    filter(!is.na(tcc_2020), !is.na(tcc_2025), tcc_2025 - tcc_2020 < -TCC_DROP_PP) |>
    mutate(signal = "TCC drop >20 pp 2020-2025")
) |>
  mutate(lf_type = coalesce(lf_type, "No LANDFIRE disturbance")) |>
  count(signal, zone, lf_type, name = "n") |>
  group_by(signal, zone) |>
  mutate(acres = n * px_acres, pct_of_signal = 100 * n / sum(n)) |>
  ungroup()

# Area first flagged as TCC-disturbed in each year, per zone.
tcc_dist_by_year <- px |>
  filter(!is.na(tcc_dist_year)) |>
  count(zone, year = tcc_dist_year, name = "n") |>
  mutate(acres = n * px_acres) |>
  pct_of_zone()

# Mean canopy per zone per year, 2010-2025, ignoring no-data cells.
tcc_mean_by_year <- data.frame(zone = zone, tcc, check.names = FALSE) |>
  pivot_longer(-zone, names_to = "year", values_to = "tcc") |>
  mutate(year = as.integer(year)) |>
  group_by(zone, year) |>
  summarise(mean_tcc = mean(tcc, na.rm = TRUE), .groups = "drop")

# Agreement between the three disturbance signals, 2019-2024
# LANDFIRE "recent" = the 1-year and 2-5-year periods (roughly 2020-2024). The
# last two columns ask: of the cells Hansen (or TCC) flags, what percent does
# LANDFIRE also flag? pmax(..., 1) avoids dividing by zero in empty zones.
agree_window <- function(y) !is.na(y) & y >= 2019 & y <= 2024
agreement <- px |>
  mutate(
    hansen = agree_window(hansen_year),
    tcc_flag = agree_window(tcc_dist_year),
    lf_recent = !is.na(lf_period) & lf_period %in% c("2-5 yrs ago", "1 yr ago")
  ) |>
  group_by(zone) |>
  summarise(
    pct_hansen = 100 * mean(hansen),
    pct_tcc = 100 * mean(tcc_flag),
    pct_lf_recent = 100 * mean(lf_recent),
    pct_any = 100 * mean(hansen | tcc_flag | lf_recent),
    pct_all_three = 100 * mean(hansen & tcc_flag & lf_recent),
    pct_hansen_also_lf = 100 * sum(hansen & lf_recent) / pmax(sum(hansen), 1),
    pct_tcc_also_lf = 100 * sum(tcc_flag & lf_recent) / pmax(sum(tcc_flag), 1),
    .groups = "drop"
  )

# Percent of zone area disturbed per year in each LANDFIRE period
# Done for all severities and for high severity only. complete() adds rows of 0
# for zone/period combinations with no disturbance so lines do not have gaps.
lf_by_period <- bind_rows(
  px |> filter(!is.na(lf_period)) |> mutate(severity = "All severities"),
  px |> filter(!is.na(lf_period), lf_sev %in% "High") |> mutate(severity = "High severity")
) |>
  count(zone, severity, lf_period, name = "n") |>
  complete(zone, severity, lf_period, fill = list(n = 0)) |>
  mutate(
    acres = n * px_acres,
    zone_acres = as.numeric(zone_acres[zone]),
    pct_of_zone = 100 * acres / zone_acres,
    pct_of_zone_per_year = pct_of_zone / LF_PERIOD_YEARS[lf_period]
  )

# ---- Write outputs ----
# Each table is written to output_csvs/<name>.csv.
outs <- list(
  cfp_sale_landfire_by_period = lf_by_period,
  cfp_sale_landfire_by_type = lf_by_type,
  cfp_sale_landfire_by_type_severity_period = lf_by_type_sev_period,
  cfp_sale_disturbance_attribution = attribution,
  cfp_sale_tcc_disturbance_by_year = tcc_dist_by_year,
  cfp_sale_tcc_mean_by_year = tcc_mean_by_year,
  cfp_sale_signal_agreement = agreement
)
for (nm in names(outs)) {
  p <- file.path(csv_dir, paste0(nm, ".csv"))
  write.csv(outs[[nm]], p, row.names = FALSE)
  message("Wrote ", p)
}

# ---- Figures ----
# Three charts for the two sale footprints and three comparison zones.
if (has_ggplot) {
  focus <- c(
    "Songbird (TRG to Songbird, 2026)",
    "Heartlands (TRG to TNC)",
    "TRG/Verdant retained",
    "MWF",
    "Non-institutional kept"
  )
  # Figure 1: mean canopy by year.
  g1 <- tcc_mean_by_year |>
    filter(zone %in% focus) |>
    mutate(zone = factor(zone, levels = focus)) |>
    ggplot(aes(year, mean_tcc, color = zone)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.4) +
    scale_x_continuous(breaks = seq(2010, 2025, 2)) +
    labs(
      title = "Mean tree canopy cover by year",
      subtitle = "USFS TCC, 30 m pixels inside each zone",
      x = NULL, y = "Mean TCC (percent)", color = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    guides(color = guide_legend(nrow = 2))
  ggsave(file.path(fig_dir, "cfp_sale_tcc_mean_by_year.png"), g1, width = 8, height = 4.8, dpi = 150)

  # Figure 2: stacked bars of LANDFIRE-disturbed area by cause.
  g2 <- lf_by_type |>
    filter(zone %in% focus) |>
    mutate(zone = factor(zone, levels = rev(focus))) |>
    ggplot(aes(pct_of_zone, zone, fill = lf_type)) +
    geom_col(width = 0.7) +
    labs(
      title = "LANDFIRE disturbance by cause, ~2014-2024",
      subtitle = "Percent of each zone's area",
      x = "Percent of zone area", y = NULL, fill = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    guides(fill = guide_legend(nrow = 2))
  ggsave(file.path(fig_dir, "cfp_sale_landfire_by_type.png"), g2, width = 8, height = 4.2, dpi = 150)

  # Figure 3: LANDFIRE disturbance rate per year, oldest period on the left.
  g3 <- lf_by_period |>
    filter(zone %in% focus, severity == "All severities") |>
    mutate(
      zone = factor(zone, levels = focus),
      lf_period = factor(lf_period, levels = names(LF_PERIOD_AXIS))
    ) |>
    ggplot(aes(lf_period, pct_of_zone_per_year, color = zone, group = zone)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8) +
    scale_x_discrete(labels = LF_PERIOD_AXIS) +
    labs(
      title = "LANDFIRE disturbance by period",
      subtitle = "Percent of each zone's area disturbed per year (periods are 5, 4 and 1 years long)",
      caption = "Approximate years assume a 2024 LANDFIRE reference year.",
      x = NULL, y = "Percent of zone area per year", color = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    guides(color = guide_legend(nrow = 2))
  ggsave(file.path(fig_dir, "cfp_sale_landfire_by_period.png"), g3, width = 8, height = 4.8, dpi = 150)
  message("Wrote figures to ", fig_dir)
}

# ---- Console report ----
# Print a titled table to the console.
show <- function(d, title) {
  message("\n=== ", title, " ===")
  print(as.data.frame(d), row.names = FALSE, digits = 3)
}
show(
  lf_by_type |> select(zone, lf_type, pct_of_zone) |>
    pivot_wider(names_from = lf_type, values_from = pct_of_zone, values_fill = 0),
  "LANDFIRE percent of zone area by type"
)
# Harvest only: total and high-severity share of each zone, by period.
show(
  lf_by_type_sev_period |>
    filter(lf_type == "Harvest / mechanical removal") |>
    group_by(zone, lf_period) |>
    summarise(pct = sum(pct_of_zone), pct_high_sev = sum(pct_of_zone[lf_sev == "High"]), .groups = "drop"),
  "LANDFIRE harvest by period (percent of zone)"
)
show(
  attribution |> select(signal, zone, lf_type, pct_of_signal) |>
    pivot_wider(names_from = lf_type, values_from = pct_of_signal, values_fill = 0),
  "What LANDFIRE calls Hansen / TCC loss pixels (percent of signal)"
)
show(
  lf_by_period |> select(zone, severity, lf_period, pct_of_zone_per_year) |>
    pivot_wider(names_from = lf_period, values_from = pct_of_zone_per_year),
  "LANDFIRE percent of zone area disturbed per year, by period"
)
show(agreement, "Signal agreement 2019-2024 (percent of zone)")
show(
  tcc_dist_by_year |> filter(year >= 2014) |> select(zone, year, pct_of_zone) |>
    pivot_wider(names_from = year, values_from = pct_of_zone, values_fill = 0),
  "TCC-detected disturbance, first year (percent of zone)"
)
message("Done.")
