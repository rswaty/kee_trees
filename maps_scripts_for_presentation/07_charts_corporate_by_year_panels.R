# Corporate owners relative to each other, year by year: one panel per corporate
# group, with all other private land as a grey reference line and the dates the
# land changed hands marked.
#   07a canopy cover, 07b forest cleared (Hansen), 07c land disturbed (LANDFIRE).
#
# Outputs: figures/07a_chart_canopy_by_year_corporate_groups.png
#          figures/07b_chart_cleared_by_year_corporate_groups.png
#          figures/07c_chart_disturbed_by_year_corporate_groups.png

if (!exists("SETUP_LOADED")) source(if (file.exists("00_setup.R")) "00_setup.R" else "maps_scripts_for_presentation/00_setup.R")

za <- zone_acres()

# Molpus and Lyme bought most of this land just before the 2020 list, so their panel
# titles give the purchase dates instead of the 2020 -> 2026 wording.
PANEL_TITLE <- setNames(CORP_GROUPS, CORP_GROUPS)
PANEL_TITLE[G$MW] <- "Molpus: bought 2020 (Vulcan) or 2014 (Ned Lake), mostly kept"
PANEL_TITLE[G$LY] <- "Lyme: bought March 2020, kept"

panel_lab <- za |>
  filter(group %in% CORP_GROUPS) |>
  mutate(panel = paste0(vapply(PANEL_TITLE[group], function(g) paste(strwrap(g, 36), collapse = "\n"), ""),
                        "\n(", fmt_n(zone_acres), " acres)")) |>
  select(group, panel)
panel_levels <- panel_lab$panel[match(CORP_GROUPS, panel_lab$group)]

by_group <- function(df, value_fun) {
  corp <- df |>
    mutate(group = ZONE_LAB[zone]) |>
    filter(group %in% CORP_GROUPS) |>
    value_fun() |>
    inner_join(panel_lab, by = "group")
  other <- df |>
    mutate(group = ZONE_LAB[zone]) |>
    filter(group %in% OTHER_GROUPS) |>
    mutate(group = "other") |>
    value_fun() |>
    select(year, value)
  list(corp = mutate(corp, panel = factor(panel, levels = panel_levels)), other = other)
}

panels_chart <- function(dl, ylab, title, subtitle, source, lim) {
  ggplot() +
    geom_line(data = dl$other, aes(year, value), colour = "grey65", linewidth = 1) +
    geom_line(data = dl$corp, aes(year, value, colour = group), linewidth = 1.3) +
    geom_point(data = dl$corp, aes(year, value, colour = group), size = 1.6) +
    facet_wrap(~panel, ncol = 3) +
    scale_colour_manual(values = GROUP_COLS, guide = "none") +
    scale_x_continuous(breaks = function(l) seq(ceiling(l[1] / 2) * 2, floor(l[2]), 4)) +
    scale_y_continuous(limits = lim, labels = function(x) paste0(x, "%")) +
    labs(title = title, subtitle = subtitle, x = NULL, y = ylab,
         caption = paste0(source, " Grey line: all other private Commercial Forest land.")) +
    theme_chart() +
    theme(strip.text = element_text(size = BASE_SIZE - 4, lineheight = 1),
          panel.spacing.x = unit(0.3, "in"), panel.spacing.y = unit(0.2, "in"),
          axis.text = element_text(size = BASE_SIZE - 4))
}

tcc <- by_group(
  inner_join(csv("cfp_sale_tcc_mean_by_year.csv"), select(za, zone, zone_acres), by = "zone"),
  function(d) d |> group_by(group, year) |> summarise(value = weighted.mean(mean_tcc, zone_acres), .groups = "drop")
)
save_chart(
  panels_chart(tcc, "Average canopy cover", "Tree canopy cover by corporate owner group",
               "Average share of the ground covered by treetops, each year",
               "Source: USFS FIA Tree Canopy Cover, 2010–2025.",
               c(floor(min(tcc$corp$value, tcc$other$value)) - 3, ceiling(max(tcc$corp$value, tcc$other$value)) + 4)),
  "07a_chart_canopy_by_year_corporate_groups.png"
)

hansen <- by_group(
  csv("cfp_sale_footprint_hansen_by_year.csv"),
  function(d) d |> group_by(group, year) |> summarise(value = 100 * sum(loss_acres) / sum(zone_acres), .groups = "drop")
)
save_chart(
  panels_chart(hansen, "Share of land cleared", "Forest cleared each year by corporate owner group",
               "Share of each group's land where trees were removed or killed outright",
               "Source: Hansen Global Forest Change (stand-replacing loss only), 2015–2024.",
               c(0, max(hansen$corp$value) * 1.2)),
  "07b_chart_cleared_by_year_corporate_groups.png"
)

landfire <- by_group(
  csv("cfp_sale_landfire_annual_by_zone.csv"),
  function(d) d |> group_by(group, year) |> summarise(value = 100 * sum(acres) / sum(zone_acres), .groups = "drop")
)
save_chart(
  panels_chart(landfire, "Share of land disturbed", "Land disturbed each year by corporate owner group",
               "Share of each group's land with any disturbance recorded that year",
               "Source: LANDFIRE Annual Disturbance, 2010–2025, all disturbance types.",
               c(0, max(landfire$corp$value) * 1.15)),
  "07c_chart_disturbed_by_year_corporate_groups.png"
)
