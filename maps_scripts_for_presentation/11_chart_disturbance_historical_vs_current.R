# Acres disturbed per year across all land in Houghton and Keweenaw counties:
# historical (LANDFIRE BpS models, all disturbances and structure-changing only)
# vs. current (LANDFIRE Annual Disturbance, 2010-2025 average), by cause.
#
# Output: figures/11_chart_disturbance_historical_vs_current.png

if (!exists("SETUP_LOADED")) source(if (file.exists("00_setup.R")) "00_setup.R" else "maps_scripts_for_presentation/00_setup.R")

PERIODS <- c(
  "Historical: all disturbances" = "Historical:\nall disturbances",
  "Historical: structure-changing only" = "Historical:\ndisturbances that\nchange structure",
  "Current (LANDFIRE average, 2010–2025)" = "Current:\nall disturbances,\n2010–2025"
)
TYPE_COLS <- c(
  "Fire" = "#c0392b",
  "Wind, weather or stress" = "#4fa3c7",
  "Insects or disease" = "#8e7cc3",
  "Other" = "grey70",
  "Harvest or mechanical" = "#e08214",
  "Unknown cause" = "#2c4a7a"
)

d <- csv("hk_disturbance_historical_vs_current.csv") |>
  filter(period %in% names(PERIODS), annual_acres > 0.5) |>
  mutate(
    bar = factor(PERIODS[period], levels = PERIODS),
    type_group = factor(type_group, levels = rev(names(TYPE_COLS)))
  )

tot <- csv("hk_disturbance_totals.csv") |>
  filter(period %in% names(PERIODS)) |>
  mutate(bar = factor(PERIODS[period], levels = PERIODS),
         lab = paste0(fmt_n(annual_acres), " ac/yr\n", sprintf("%.2f", pct_of_land), "% of the land a year"))

hist_chg <- tot$annual_acres[tot$period == "Historical: structure-changing only"]
cur <- tot$annual_acres[grepl("^Current", tot$period)]
cur_20s <- csv("hk_disturbance_totals.csv") |> filter(period == "Current (LANDFIRE average, 2020–2025)")

p <- ggplot(d, aes(bar, annual_acres, fill = type_group)) +
  geom_col(width = 0.62, colour = "white", linewidth = 0.3) +
  geom_text(data = tot, aes(bar, annual_acres, label = lab), inherit.aes = FALSE,
            vjust = -0.25, size = BASE_SIZE / 3.2, lineheight = 0.95, family = FONT_FAMILY) +
  scale_fill_manual(values = TYPE_COLS, breaks = names(TYPE_COLS), name = NULL) +
  scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Acres Disturbed per Year: Historical vs. Current",
    subtitle = paste0(
      "All land in Houghton and Keweenaw counties. Today's rate is about ",
      round(cur / hist_chg), "× the historical rate of structure-changing disturbance."
    ),
    x = NULL, y = "Acres per year",
    caption = paste0(
      "Historical: LANDFIRE Biophysical Settings models (before European settlement). \"Change structure\" = moves a stand ",
      "to a different age or structure class;\nthe rest are light disturbances. ",
      "Current: LANDFIRE Annual Disturbance, average of 2010–2025 (2020–2025 alone: ", fmt_n(cur_20s$annual_acres),
      " ac/yr).\nHarvest has no historical counterpart; \"unknown cause\" is likely mostly harvest."
    )
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_chart() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = BASE_SIZE, colour = "grey15", lineheight = 1))

save_chart(p, "11_chart_disturbance_historical_vs_current.png")
