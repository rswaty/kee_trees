# Acres disturbed per year across all land in Houghton and Keweenaw counties:
# historical (LANDFIRE BpS models, all disturbances and structure-changing only)
# vs. current (2020 onward): all disturbances by cause (LANDFIRE Annual Disturbance), and
# structure-changing disturbance as a range across three measures (scripts/21).
# 11b: the same comparison in one colour, without causes, with 2010-2019 and 2020-2025
# as separate bars (as on the Shiny app's "bigger picture" tab).
#
# Outputs: figures/11_chart_disturbance_historical_vs_current.png
#          figures/11b_chart_disturbance_totals_one_colour.png

if (!exists("SETUP_LOADED")) source(if (file.exists("00_setup.R")) "00_setup.R" else "maps_scripts_for_presentation/00_setup.R")

# ---- Labels and colours ----
# Names of the two historical periods in the LANDFIRE Biophysical Settings (BpS)
# table, and the x-axis labels for the four bars (left to right).
HIST_ALL <- "Historical: all disturbances"
HIST_CHG <- "Historical: structure-changing only"
BARS <- c(
  "Historical:\nall disturbances",
  "Historical:\ndisturbances that\nchange structure",
  "Current:\nall disturbances,\n2020–2025",
  "Current:\ndisturbances that\nchange structure,\n2020 onward"
)
# One colour per disturbance cause, in legend order.
TYPE_COLS <- c(
  "Fire" = "#c0392b",
  "Wind, weather or stress" = "#4fa3c7",
  "Insects or disease" = "#8e7cc3",
  "Other" = "grey70",
  "Harvest or mechanical" = "#e08214",
  "Unknown cause" = "#2c4a7a"
)

# ---- Data ----
# Total land area, backed out of the totals table (acres per year / percent of the land).
totals <- csv("hk_disturbance_totals.csv")
land <- with(totals[1, ], annual_acres / pct_of_land * 100)
# Historical acres per year by cause, for the two historical bars.
hist <- csv("hk_disturbance_historical_vs_current.csv") |>
  filter(period %in% c(HIST_ALL, HIST_CHG)) |>
  mutate(bar = BARS[match(period, c(HIST_ALL, HIST_CHG))]) |>
  select(bar, type_group, annual_acres)
# Current acres per year by cause: the 2020–2025 total divided by 6 years.
cur <- csv("hk_current_disturbance_by_year.csv") |>
  filter(year >= 2020) |>
  group_by(type_group) |>
  summarise(annual_acres = sum(annual_acres) / 6, .groups = "drop") |>
  mutate(bar = BARS[3])
# Combine, drop causes too small to see (under half an acre a year), and set the bar
# and stacking order.
d <- bind_rows(hist, cur) |>
  filter(annual_acres > 0.5) |>
  mutate(bar = factor(bar, levels = BARS),
         type_group = factor(type_group, levels = rev(names(TYPE_COLS))))

# Total per bar, labelled with acres per year and percent of all land. hist_chg is the
# historical structure-changing rate that the current rate is compared with.
tot <- d |>
  group_by(bar) |>
  summarise(annual_acres = sum(annual_acres), .groups = "drop") |>
  mutate(lab = paste0(fmt_n(annual_acres), " ac/yr\n", sprintf("%.2f", 100 * annual_acres / land), "% of the land"))
hist_chg <- tot$annual_acres[tot$bar == BARS[2]]

# Structure-changing now: one marker per measure, shaded from the lowest to the highest.
# The three measures (canopy cover falling 20+ points, Hansen forest loss and LANDFIRE
# high severity) get short names and labels; lo and hi are the ends of the range.
sc <- csv("hk_structure_changing_summary.csv") |>
  filter(period == "2020 onward") |>
  mutate(short = c("Canopy fell 20+ points" = "Canopy fell 20+ points",
                   "Hansen stand-replacing loss" = "Hansen forest cleared",
                   "LANDFIRE high severity" = "LANDFIRE high severity")[measure],
         lab = paste0(short, ": ", fmt_n(annual_acres)))
lo <- min(sc$annual_acres)
hi <- max(sc$annual_acres)
# Position of the fourth bar on the x-axis, and the bar width used for every bar.
x4 <- 4
w <- 0.62

# ---- 11: chart by cause ----
# The first three bars are stacked by cause. The fourth bar is drawn by hand: dark grey
# up to the lowest measure (all three agree on at least this much), light grey from
# the lowest to the highest, and a black tick at each measure.
p <- ggplot(d, aes(bar, annual_acres, fill = type_group)) +
  geom_col(width = w, colour = "white", linewidth = 0.3) +
  annotate("rect", xmin = x4 - w / 2, xmax = x4 + w / 2, ymin = 0, ymax = lo, fill = "grey35") +
  annotate("rect", xmin = x4 - w / 2, xmax = x4 + w / 2, ymin = lo, ymax = hi, fill = "grey80") +
  annotate("segment", x = x4 - w / 2, xend = x4 + w / 2, y = sc$annual_acres, yend = sc$annual_acres,
           colour = "grey10", linewidth = 0.7) +
  # Measure labels sit to the right of the fourth bar. The second-highest is nudged up
  # and the lowest nudged down by 250 acres so close values don't overlap.
  annotate("text", x = x4 + w / 2 + 0.05, y = sc$annual_acres + c(0, 250, -250)[rank(-sc$annual_acres)],
           label = sc$lab, hjust = 0, size = BASE_SIZE / 3.6, family = FONT_FAMILY, colour = "grey15") +
  # Above the fourth bar: the range, and how many times the historical
  # structure-changing rate it is. Above the other bars: their totals.
  annotate("text", x = x4, y = hi, vjust = -0.25, size = BASE_SIZE / 3.2, lineheight = 0.95, family = FONT_FAMILY,
           label = paste0(fmt_n(lo), "–", fmt_n(hi), " ac/yr\n", sprintf("%.1f", lo / hist_chg), "–",
                          sprintf("%.1f", hi / hist_chg), "× historical")) +
  geom_text(data = tot, aes(bar, annual_acres, label = lab), inherit.aes = FALSE,
            vjust = -0.25, size = BASE_SIZE / 3.2, lineheight = 0.95, family = FONT_FAMILY) +
  # Keep the empty fourth slot on the axis, and leave extra room on the right for the
  # measure labels (allowed to spill past the panel edge).
  scale_x_discrete(drop = FALSE, expand = expansion(add = c(0.5, 1.5))) +
  coord_cartesian(clip = "off") +
  scale_fill_manual(values = TYPE_COLS, breaks = names(TYPE_COLS), name = NULL) +
  scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Acres Disturbed per Year: Historical vs. Current",
    subtitle = paste0(
      "All land in Houghton and Keweenaw counties. Since 2020, structure-changing disturbance is about ",
      sprintf("%.1f", lo / hist_chg), "–", sprintf("%.1f", hi / hist_chg), "× the historical rate."
    ),
    x = NULL, y = "Acres per year",
    caption = paste0(
      "Historical: LANDFIRE Biophysical Settings models (before European settlement). \"Change structure\" = moves a stand ",
      "to a different age or structure class.\n",
      "Current, all: LANDFIRE Annual Disturbance. Current, structure-changing: LANDFIRE high severity (conservative), ",
      "Hansen stand-replacing loss (through 2024)\nand USFS canopy cover falling 20+ points (includes heavy partial cuts). ",
      "Harvest has no historical counterpart; \"unknown cause\" is likely mostly harvest."
    )
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_chart() +
  theme(panel.grid.major.x = element_blank(), plot.margin = margin(10, 30, 10, 10),
        axis.text.x = element_text(size = BASE_SIZE - 1, colour = "grey15", lineheight = 1))

save_chart(p, "11_chart_disturbance_historical_vs_current.png")

# ---- 11b: one colour, no causes ----
# Blue for all-disturbance bars, grey for structure-changing bars. Five bars: the
# current all-disturbance rate is split into 2010–2019 and 2020–2025.
ONE_COL <- "#2c4a7a"
STRUCT_COL <- "grey55"
BARS_B <- c(
  "Historical:\nall disturbances",
  "Historical:\ndisturbances that\nchange structure",
  "Current:\nall disturbances,\n2010–2019",
  "Current:\nall disturbances,\n2020–2025",
  "Current:\ndisturbances that\nchange structure,\n2020 onward"
)
# Totals for the first four bars, taken straight from the totals table (the historical
# structure-changing total comes from the chart above). Each gets an acres label and
# its fill colour.
tb <- tibble(
  bar = factor(BARS_B[1:4], levels = BARS_B),
  annual_acres = c(totals$annual_acres[totals$period == HIST_ALL],
                   hist_chg,
                   totals$annual_acres[totals$period == "Current (LANDFIRE average, 2010–2019)"],
                   totals$annual_acres[totals$period == "Current (LANDFIRE average, 2020–2025)"])
) |>
  mutate(lab = paste0(fmt_n(annual_acres), " ac/yr"),
         fill = if_else(bar == BARS_B[2], STRUCT_COL, ONE_COL))
# Position of the fifth bar on the x-axis.
x5 <- 5

# Bars for the first four totals. The fifth bar is drawn by hand: grey up to the
# highest measure, with a white line at the lowest measure and the range written above.
p_b <- ggplot(tb, aes(bar, annual_acres)) +
  geom_col(aes(fill = fill), width = w) +
  annotate("rect", xmin = x5 - w / 2, xmax = x5 + w / 2, ymin = 0, ymax = hi, fill = STRUCT_COL) +
  annotate("segment", x = x5 - w / 2, xend = x5 + w / 2, y = lo, yend = lo, colour = "white", linewidth = 0.8) +
  annotate("text", x = x5, y = hi, vjust = -0.4, size = BASE_SIZE / 3.2, family = FONT_FAMILY,
           label = paste0(fmt_n(lo), "–", fmt_n(hi), " ac/yr")) +
  geom_text(aes(label = lab), vjust = -0.4, size = BASE_SIZE / 3.2, family = FONT_FAMILY) +
  # Use the colour names stored in the table as they are, and keep the empty fifth slot.
  scale_fill_identity() +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Acres Disturbed per Year: Historical vs. Current",
    subtitle = "Historical represents modeled pre-European settlement annual disturbances",
    x = NULL, y = "Acres per year",
    caption = paste0(
      "Historical: LANDFIRE Biophysical Settings models. \"Change structure\" = moves a stand to a different age or structure class.\n",
      "Current, all: LANDFIRE Annual Disturbance. Last bar: the range across three measures, from Hansen stand-replacing loss ",
      "(white line;\nLANDFIRE high severity is similar) to USFS canopy cover falling 20+ points (top; includes heavy partial cuts)."
    )
  ) +
  theme_chart() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = BASE_SIZE - 1, colour = "grey15", lineheight = 1))

save_chart(p_b, "11b_chart_disturbance_totals_one_colour.png")
