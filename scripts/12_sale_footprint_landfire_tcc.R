# LANDFIRE disturbance cause/severity and annual TCC trajectories by sale zone.
# Zones come from scripts/11_sale_footprint_pixels.R (cfp_sale_zones.tif).
# LANDFIRE FDist codes (inputs/landfire/lf_hist_dist.tif): hundreds = type, tens = severity,
# ones = years-since bin (1, 2-5, 6-10 years). Approximate calendar years in
# figure labels assume a 2024 LANDFIRE reference year.
# TCC disturbance year = first year canopy falls >= TCC_DROP_PP below the max of
# the previous TCC_LOOKBACK years.
# Run from project root: Rscript scripts/12_sale_footprint_landfire_tcc.R
# Does not touch Shiny.

Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(tidyr)
})

has_ggplot <- requireNamespace("ggplot2", quietly = TRUE)
if (has_ggplot) {
  suppressPackageStartupMessages(library(ggplot2))
}

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

zone_path <- file.path(gis_dir, "cfp_sale_zones.tif")
if (!file.exists(zone_path)) {
  stop("Missing ", zone_path, " — run scripts/11_sale_footprint_pixels.R first.")
}

TCC_YEARS <- 2010:2025
TCC_DROP_PP <- 20
TCC_LOOKBACK <- 3

LF_TYPE <- c(
  "1" = "Fire",
  "2" = "Mechanical add",
  "3" = "Harvest / mechanical removal",
  "5" = "Insects or disease",
  "6" = "Unattributed change"
)
LF_SEVERITY <- c("1" = "Low", "2" = "Moderate", "3" = "High")
LF_PERIOD <- c("1" = "1 yr ago", "2" = "2-5 yrs ago", "3" = "6-10 yrs ago")
LF_PERIOD_YEARS <- c("1 yr ago" = 1, "2-5 yrs ago" = 4, "6-10 yrs ago" = 5)
LF_PERIOD_AXIS <- c(
  "6-10 yrs ago" = "6-10 yrs ago\n(~2015-19)",
  "2-5 yrs ago" = "2-5 yrs ago\n(~2020-23)",
  "1 yr ago" = "1 yr ago\n(~2024)"
)

zr <- rast(zone_path)
labels <- read.csv(file.path(csv_dir, "cfp_sale_zone_labels.csv"), stringsAsFactors = FALSE)
px_acres <- prod(res(zr)) / 4046.8564224

message("Reprojecting LANDFIRE FDist onto TCC grid...")
lf <- project(rast(file.path(root, "inputs/landfire/lf_hist_dist.tif")), zr, method = "near")

z <- values(zr, mat = FALSE)
idx <- which(!is.na(z) & z %in% labels$zone_code)
zone <- labels$zone[match(z[idx], labels$zone_code)]
zone_acres <- tapply(rep(px_acres, length(idx)), zone, sum)

lfv <- values(lf, mat = FALSE)[idx]
lfv[!is.na(lfv) & lfv <= 0] <- NA
lf_type <- unname(LF_TYPE[as.character(lfv %/% 100)])
lf_sev <- unname(LF_SEVERITY[as.character((lfv %/% 10) %% 10)])
lf_period <- unname(LF_PERIOD[as.character(lfv %% 10)])

hy <- values(rast(file.path(gis_dir, "hansen_lossyear.tif")), mat = FALSE)[idx]
hansen_year <- ifelse(!is.na(hy) & hy > 0, 2000L + as.integer(hy), NA_integer_)

message("Reading annual TCC for zone pixels...")
tcc <- vapply(TCC_YEARS, function(y) {
  v <- values(rast(file.path(tcc_dir, sprintf("HK_TCC_%d.tif", y))), mat = FALSE)[idx]
  v[v >= 254] <- NA
  as.numeric(v)
}, numeric(length(idx)))
colnames(tcc) <- TCC_YEARS

# First year with a TCC_DROP_PP drop below the prior TCC_LOOKBACK-year max
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

pct_of_zone <- function(d) {
  d |> mutate(zone_acres = as.numeric(zone_acres[zone]), pct_of_zone = 100 * acres / zone_acres)
}

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

tcc_dist_by_year <- px |>
  filter(!is.na(tcc_dist_year)) |>
  count(zone, year = tcc_dist_year, name = "n") |>
  mutate(acres = n * px_acres) |>
  pct_of_zone()

tcc_mean_by_year <- data.frame(zone = zone, tcc, check.names = FALSE) |>
  pivot_longer(-zone, names_to = "year", values_to = "tcc") |>
  mutate(year = as.integer(year)) |>
  group_by(zone, year) |>
  summarise(mean_tcc = mean(tcc, na.rm = TRUE), .groups = "drop")

# Agreement between the three disturbance signals, 2019-2024
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

if (has_ggplot) {
  focus <- c(
    "Songbird (TRG to Songbird, 2026)",
    "Heartlands (TRG to TNC)",
    "TRG/Verdant retained",
    "MWF",
    "Non-institutional kept"
  )
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

show <- function(d, title) {
  message("\n=== ", title, " ===")
  print(as.data.frame(d), row.names = FALSE, digits = 3)
}
show(
  lf_by_type |> select(zone, lf_type, pct_of_zone) |>
    pivot_wider(names_from = lf_type, values_from = pct_of_zone, values_fill = 0),
  "LANDFIRE percent of zone area by type"
)
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
