# Corporate owners as a whole vs. other private owners, year by year:
#   03a canopy cover, 03b forest cleared (Hansen), 03c land disturbed (LANDFIRE).
# Each line pools every 30 m cell of that owner type's 2020 land.
#
# Outputs: figures/03a_chart_canopy_by_year_corporate_vs_other.png
#          figures/03b_chart_cleared_by_year_corporate_vs_other.png
#          figures/03c_chart_disturbed_by_year_corporate_vs_other.png

if (!exists("SETUP_LOADED")) source(if (file.exists("00_setup.R")) "00_setup.R" else "maps_scripts_for_presentation/00_setup.R")

za <- zone_acres()
dates <- owner_start_dates()
verdant_txt <- owner_date_text("Verdant")

tcc <- csv("cfp_sale_tcc_mean_by_year.csv") |>
  inner_join(za, by = "zone") |>
  group_by(class, year) |>
  summarise(value = weighted.mean(mean_tcc, zone_acres), .groups = "drop")

hansen <- csv("cfp_sale_footprint_hansen_by_year.csv") |>
  inner_join(select(za, zone, class), by = "zone") |>
  group_by(class, year) |>
  summarise(value = 100 * sum(loss_acres) / sum(zone_acres), .groups = "drop")

landfire <- csv("cfp_sale_landfire_annual_by_zone.csv") |>
  inner_join(select(za, zone, class), by = "zone") |>
  group_by(class, year) |>
  summarise(value = 100 * sum(acres) / sum(zone_acres), .groups = "drop")

class_lines <- function(df, ylab, title, subtitle, source, digits = 1, pad = NULL) {
  df <- mutate(df, class = factor(class, levels = c(CORP, OTHER)))
  last <- df |> group_by(class) |> filter(year == max(year))
  yr <- range(df$year)
  lim <- if (is.null(pad)) c(0, max(df$value) * 1.12) else c(floor(min(df$value)) - pad, ceiling(max(df$value)) + pad)
  ggplot(df, aes(year, value, colour = class)) +
    annotate("segment", x = dates$verdant, xend = dates$verdant, y = lim[1], yend = lim[2],
             linetype = "dashed", colour = "grey45", linewidth = 0.5) +
    annotate("label", x = dates$verdant, y = lim[2], label = paste0("TRG sells to Verdant\n", verdant_txt),
             hjust = 1.04, vjust = 1, size = BASE_SIZE / 3.6, colour = "grey30", fill = "white",
             border.colour = NA, lineheight = 0.95, family = FONT_FAMILY) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 2.4) +
    geom_text(data = last, aes(label = paste0(formatC(value, format = "f", digits = digits), "%")),
              hjust = -0.25, size = BASE_SIZE / 3.2, fontface = "bold", show.legend = FALSE) +
    scale_colour_manual(values = CLASS_COLS[c(CORP, OTHER)], name = NULL) +
    scale_x_continuous(breaks = seq(yr[1], yr[2], if (diff(yr) > 10) 2 else 1),
                       limits = c(yr[1], yr[2] + 0.8)) +
    scale_y_continuous(limits = lim, labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.02))) +
    labs(title = title, subtitle = subtitle, x = NULL, y = ylab,
         caption = paste0(source, "\n", CORP_NOTE, "\n", OTHER_NOTE)) +
    theme_chart()
}

save_chart(
  class_lines(tcc, "Average canopy cover",
              "Tree canopy cover, corporate vs. other private land",
              "Average share of the ground covered by treetops, each year",
              "Source: USFS FIA Tree Canopy Cover, 2010–2025, all 30 m cells on each owner type's 2020 Commercial Forest land.",
              pad = 5),
  "03a_chart_canopy_by_year_corporate_vs_other.png"
)

save_chart(
  class_lines(hansen, "Share of land cleared",
              "Forest cleared each year, corporate vs. other private land",
              "Share of each owner type's land where trees were removed or killed outright",
              "Source: Hansen Global Forest Change (stand-replacing loss only), 2015–2024.",
              digits = 2),
  "03b_chart_cleared_by_year_corporate_vs_other.png"
)

save_chart(
  class_lines(landfire, "Share of land disturbed",
              "Land disturbed each year, corporate vs. other private land",
              "Share of each owner type's land with any disturbance recorded that year",
              "Source: LANDFIRE Annual Disturbance, 2010–2025, all disturbance types."),
  "03c_chart_disturbed_by_year_corporate_vs_other.png"
)
