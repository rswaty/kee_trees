# Line chart: share of each group's land disturbed each year (LANDFIRE Annual Disturbance), 2010-2025.
# Data: output_csvs/cfp_sale_landfire_annual_by_zone.csv (scripts/16).
if (!exists("CHART_SETUP_LOADED")) source(file.path(if (dir.exists("scripts")) "." else "../..", "scripts/charts/00_setup.R"))

d <- csv("cfp_sale_landfire_annual_by_zone.csv") |>
  mutate(group = ZONE_LAB[zone]) |>
  filter(group %in% FOCUS) |>
  mutate(group = factor(group, levels = FOCUS))

p <- ggplot(d, aes(year, pct_of_zone, color = group)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2.2) +
  scale_color_manual(values = GROUP_COLS) +
  scale_x_continuous(breaks = seq(2010, 2025, 2)) +
  labs(
    title = "Share of each group's land disturbed each year",
    subtitle = "LANDFIRE Annual Disturbance, all types",
    x = NULL, y = "% of area disturbed", color = NULL
  ) +
  theme_ppt()

save_chart(p, "05_landfire_disturbance_by_year")
