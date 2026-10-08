# Bar chart: acres in each 10% canopy cover bin, 2020 vs 2025, Heartlands and Songbird.
# Data: output_csvs/cfp_footprint_tcc_bins_2020_2025.csv (scripts/13).
if (!exists("CHART_SETUP_LOADED")) source(file.path(if (dir.exists("scripts")) "." else "../..", "scripts/charts/00_setup.R"))

YEAR_COLS <- c("2020" = "#9ecae1", "2025" = "#08519c")

d <- csv("cfp_footprint_tcc_bins_2020_2025.csv") |>
  mutate(bin = factor(bin, levels = unique(bin))) |>
  pivot_longer(c(acres_2020, acres_2025), names_to = "year", names_prefix = "acres_", values_to = "acres")

p <- ggplot(d, aes(bin, acres, fill = year)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  facet_wrap(~footprint, nrow = 1, scales = "free_y") +
  scale_fill_manual(values = YEAR_COLS) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Acres in each canopy cover bin, 2020 vs. 2025",
    subtitle = "USFS Tree Canopy Cover, 30 m cells inside each sale footprint",
    x = "Canopy cover", y = "Acres", fill = NULL
  ) +
  theme_ppt() +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

save_chart(p, "09_canopy_bins_2020_2025")
