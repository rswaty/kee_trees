# Corporate owners as a whole vs. other private owners, year by year:
#   03a canopy cover, 03b forest cleared (Hansen), 03c land disturbed (LANDFIRE).
# Each line pools every 30 m cell of that owner type's 2020 land.
#
# Outputs: figures/03a_chart_canopy_by_year_corporate_vs_other.png
#          figures/03b_chart_cleared_by_year_corporate_vs_other.png
#          figures/03c_chart_disturbed_by_year_corporate_vs_other.png

if (!exists("SETUP_LOADED")) source(if (file.exists("00_setup.R")) "00_setup.R" else "maps_scripts_for_presentation/00_setup.R")

za <- zone_acres()

# Yearly values per owner type. The analysis tables are by sale group, so groups are
# combined, weighting each by its acres.
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

# One line chart: corporate vs. other private, with the last year's value labelled at
# the line end. Use pad to zoom the y-axis instead of starting at zero.
class_lines <- function(df, ylab, title, subtitle, source, digits = 1, pad = NULL) {
  df <- mutate(df, class = factor(class, levels = c(CORP, OTHER)))
  last <- df |> group_by(class) |> filter(year == max(year))
  yr <- range(df$year)
  lim <- if (is.null(pad)) c(0, max(df$value) * 1.12) else c(floor(min(df$value)) - pad, ceiling(max(df$value)) + pad)
  ggplot(df, aes(year, value, colour = class)) +
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

# Gap between the two canopy lines every 5 years, drawn as a dark grey bracket with
# its value. The last one is labelled on the left to stay clear of the line-end labels.
GAP_GREY <- "grey25"
gap <- tcc |>
  filter(year %% 5 == 0) |>
  tidyr::pivot_wider(names_from = class, values_from = value) |>
  transmute(year, lo = .data[[CORP]], hi = .data[[OTHER]], diff = hi - lo,
            right = year != max(tcc$year),
            # 2015 label sits higher, clear of the corporate line rising to 2016
            label_y = (lo + hi) / 2 + if_else(year == 2015, 0.7, 0))

p03a <- class_lines(tcc, "Average canopy cover",
                    "Tree canopy cover, corporate vs. other private land",
                    "Average share of the ground covered by treetops, each year",
                    paste0("Source: USFS FIA Tree Canopy Cover, 2010–2025, all 30 m cells on each owner type's 2020 ",
                           "Commercial Forest land. Grey: gap between the lines, in percentage points."),
                    pad = 5) +
  geom_segment(data = gap, aes(x = year, xend = year, y = lo + 0.25, yend = hi - 0.25),
               inherit.aes = FALSE, colour = GAP_GREY, linewidth = 0.6, linetype = "22") +
  geom_text(data = gap, aes(x = year + if_else(right, 0.15, -0.15), y = label_y,
                            label = paste0(fmt_1(diff), "%"), hjust = if_else(right, 0, 1)),
            inherit.aes = FALSE, colour = GAP_GREY, size = BASE_SIZE / 3.4, fontface = "bold",
            family = FONT_FAMILY)

save_chart(p03a, "03a_chart_canopy_by_year_corporate_vs_other.png")

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
