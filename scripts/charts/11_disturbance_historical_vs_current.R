# Stacked bars: acres disturbed per year, historical (BpS models: all, and structure-changing
# only) vs current (LANDFIRE average, 2010-2025), all land in both counties.
# Data: output_csvs/hk_disturbance_historical_vs_current.csv, hk_disturbance_totals.csv (scripts/18).
if (!exists("CHART_SETUP_LOADED")) source(file.path(if (dir.exists("scripts")) "." else "../..", "scripts/charts/00_setup.R"))

summary_tab <- csv("hk_disturbance_historical_vs_current.csv")
period_levels <- c("Historical: all disturbances", "Historical: structure-changing only",
                   grep("^Current", unique(summary_tab$period), value = TRUE))
totals <- csv("hk_disturbance_totals.csv") |>
  filter(period %in% period_levels) |>
  mutate(period = factor(period, levels = period_levels))

p <- summary_tab |>
  mutate(
    period = factor(period, levels = period_levels),
    type_group = factor(type_group, levels = rev(names(DIST_COLS)))
  ) |>
  ggplot(aes(period, annual_acres, fill = type_group)) +
  geom_col(width = 0.6) +
  geom_text(
    data = totals,
    aes(period, annual_acres, label = paste0(comma(annual_acres, 1), " acres/yr\n(", round(pct_of_land, 2), "% of land)")),
    inherit.aes = FALSE, vjust = -0.2, size = BASE_SIZE / 4, family = FONT_FAMILY
  ) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, 20)) +
  scale_fill_manual(values = DIST_COLS, breaks = names(DIST_COLS)) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.25))) +
  labs(
    title = "Disturbance per year: historical vs. current",
    subtitle = "All land, Houghton and Keweenaw counties. Historical: BpS models; current: LANDFIRE.",
    x = NULL, y = "Acres disturbed per year", fill = NULL
  ) +
  theme_ppt() +
  theme(panel.grid.major.x = element_blank())

save_chart(p, "11_disturbance_historical_vs_current", height = 6.4)
