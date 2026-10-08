# Line chart: share of each group's land cleared each year (Hansen), 2015-2024.
# Data: output_csvs/cfp_sale_footprint_hansen_by_year.csv (scripts/11).
if (!exists("CHART_SETUP_LOADED")) source(file.path(if (dir.exists("scripts")) "." else "../..", "scripts/charts/00_setup.R"))

d <- csv("cfp_sale_footprint_hansen_by_year.csv") |>
  mutate(group = ZONE_LAB[zone]) |>
  filter(group %in% FOCUS) |>
  mutate(group = factor(group, levels = FOCUS))

p <- ggplot(d, aes(year, pct_of_zone, color = group)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2.2) +
  scale_color_manual(values = GROUP_COLS) +
  scale_x_continuous(breaks = seq(min(d$year), max(d$year), 1)) +
  labs(
    title = "Share of each group's land cleared each year",
    subtitle = "Hansen Global Forest Change (heavy clearing only)",
    x = NULL, y = "% of area cleared", color = NULL
  ) +
  theme_ppt()

save_chart(p, "04_clearing_by_year_hansen")
