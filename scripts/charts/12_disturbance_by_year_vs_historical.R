# Stacked bars: acres disturbed each year by cause (LANDFIRE, 2010-2025), with the two
# historical rates (BpS models) as reference lines. All land in both counties.
# Data: output_csvs/hk_current_disturbance_by_year.csv, hk_disturbance_totals.csv (scripts/18).
if (!exists("CHART_SETUP_LOADED")) source(file.path(if (dir.exists("scripts")) "." else "../..", "scripts/charts/00_setup.R"))

by_year <- csv("hk_current_disturbance_by_year.csv")
totals <- csv("hk_disturbance_totals.csv")
hist_all <- totals$annual_acres[totals$period == "Historical: all disturbances"]
hist_chg <- totals$annual_acres[totals$period == "Historical: structure-changing only"]
yrs <- range(by_year$year)

ref_label <- function(y, text) {
  annotate("label", x = yrs[1] - 0.4, y = y, hjust = 0, size = BASE_SIZE / 4.5, family = FONT_FAMILY,
           fill = "white", linewidth = 0, label = paste0(text, ": ", comma(y, 1), " acres/yr"))
}

p <- by_year |>
  mutate(type_group = factor(type_group, levels = rev(names(DIST_COLS)))) |>
  ggplot(aes(year, annual_acres, fill = type_group)) +
  geom_col(width = 0.8) +
  geom_hline(yintercept = hist_all, linetype = "dotted", linewidth = 0.9) +
  geom_hline(yintercept = hist_chg, linetype = "dashed", linewidth = 0.9) +
  ref_label(hist_all, "Historical, all disturbances") +
  ref_label(hist_chg, "Historical, structure-changing") +
  scale_fill_manual(values = DIST_COLS, breaks = names(DIST_COLS)) +
  scale_x_continuous(breaks = seq(yrs[1], yrs[2], 1)) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Acres disturbed each year vs. the historical expectation",
    subtitle = "LANDFIRE Annual Disturbance, all land, Houghton and Keweenaw counties",
    x = NULL, y = "Acres disturbed", fill = NULL
  ) +
  theme_ppt() +
  theme(panel.grid.major.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

save_chart(p, "12_disturbance_by_year_vs_historical", height = 6.2)
