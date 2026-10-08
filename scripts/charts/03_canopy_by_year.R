# Line chart: average tree canopy cover by group, 2010-2025.
# Data: output_csvs/cfp_sale_tcc_mean_by_year.csv (scripts/12).
if (!exists("CHART_SETUP_LOADED")) source(file.path(if (dir.exists("scripts")) "." else "../..", "scripts/charts/00_setup.R"))

d <- csv("cfp_sale_tcc_mean_by_year.csv") |>
  mutate(group = ZONE_LAB[zone]) |>
  filter(group %in% FOCUS) |>
  mutate(group = factor(group, levels = FOCUS))

p <- ggplot(d, aes(year, mean_tcc, color = group)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2.2) +
  scale_color_manual(values = GROUP_COLS) +
  scale_x_continuous(breaks = seq(2010, 2025, 2)) +
  scale_y_continuous(
    limits = c(floor(min(d$mean_tcc)) - 5, ceiling(max(d$mean_tcc)) + 5),
    breaks = seq(0, 100, 5)
  ) +
  labs(
    title = "Average tree canopy cover, by group",
    subtitle = "USFS Tree Canopy Cover, all 30 m cells in each group's land",
    x = NULL, y = "Canopy cover (%)", color = NULL
  ) +
  theme_ppt()

save_chart(p, "03_canopy_by_year")
