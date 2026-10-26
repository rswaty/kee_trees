# Corporate vs. other private owners on three independent measures of disturbance,
# 2020 onward.
#
# Output: figures/04_chart_corporate_vs_other_three_measures.png

if (!exists("SETUP_LOADED")) source(if (file.exists("00_setup.R")) "00_setup.R" else "maps_scripts_for_presentation/00_setup.R")

# ---- Data: three measures ----
# Acres and owner class of each analysis zone (sale group).
za <- zone_acres()

# Measure 1 (USFS FIA Tree Canopy Cover): share of acres in parcels whose average canopy fell 20+ points.
heavy <- csv("cfp_sellers_by_owner_class.csv") |>
  mutate(class = if_else(owner_class == "Institutional", CORP, OTHER)) |>
  group_by(class) |>
  summarise(value = 100 * sum(acres_major_disturbance) / sum(acres), .groups = "drop") |>
  mutate(measure = "Tree canopy change, 2020–2025\n(FIA, > 20% loss)")

# Measure 2: share of land Hansen marked as cleared at any point, 2020–2024.
# Yearly loss is added up per zone, then zones are pooled by owner type.
cleared <- csv("cfp_sale_footprint_hansen_by_year.csv") |>
  filter(year >= 2020) |>
  inner_join(select(za, zone, class), by = "zone") |>
  group_by(class, zone) |>
  summarise(loss = sum(loss_acres), acres = first(zone_acres), .groups = "drop") |>
  group_by(class) |>
  summarise(value = 100 * sum(loss) / sum(acres), .groups = "drop") |>
  mutate(measure = "Land cleared, 2020–2024\n(Hansen, stand-replacing loss)")

# Measure 3: share of land LANDFIRE recorded as disturbed each year, averaged over 2020–2025.
disturbed <- csv("cfp_sale_landfire_annual_by_zone.csv") |>
  filter(year >= 2020) |>
  inner_join(select(za, zone, class), by = "zone") |>
  group_by(class, year) |>
  summarise(pct = 100 * sum(acres) / sum(zone_acres), .groups = "drop") |>
  group_by(class) |>
  summarise(value = mean(pct), .groups = "drop") |>
  mutate(measure = "Land disturbed per year, 2020–2025\n(LANDFIRE, average)")

# Stack the three measures into one table, keeping corporate first and the measures
# in the order above.
d <- bind_rows(heavy, cleared, disturbed) |>
  mutate(class = factor(class, levels = c(CORP, OTHER)),
         measure = factor(measure, levels = unique(measure)))

# ---- Chart ----
# One small bar chart per measure, side by side, with the percent written above each bar.
# The panels share a y-axis scale.
p <- ggplot(d, aes(class, value, fill = class)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(fmt_1(value), "%")), vjust = -0.4, size = BASE_SIZE / 2.9, fontface = "bold") +
  facet_wrap(~measure) +
  scale_fill_manual(values = CLASS_COLS[c(CORP, OTHER)], guide = "none") +
  scale_x_discrete(labels = function(x) gsub(" owners", "\nowners", x)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Corporate land was disturbed more than other private land on every measure",
    x = NULL, y = NULL,
    caption = paste0(
      "Sources: USFS FIA Tree Canopy Cover (parcel averages, parcels of 20+ acres); Hansen Global Forest Change; LANDFIRE Annual Disturbance.\n",
      CORP_NOTE, "\n", OTHER_NOTE
    )
  ) +
  theme_chart() +
  theme(panel.grid.major.x = element_blank(), strip.text = element_text(hjust = 0.5, size = BASE_SIZE - 1),
        panel.spacing = unit(0.4, "in"))

save_chart(p, "04_chart_corporate_vs_other_three_measures.png")
