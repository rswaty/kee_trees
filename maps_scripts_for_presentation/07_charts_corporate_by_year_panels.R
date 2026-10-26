# Corporate owners relative to each other, year by year: one panel per corporate
# group, with all other private land as a grey reference line.
#   07a canopy cover, 07b forest cleared (Hansen), 07c land disturbed (LANDFIRE).
#
# Outputs: figures/07a_chart_canopy_by_year_corporate_groups.png
#          figures/07b_chart_cleared_by_year_corporate_groups.png
#          figures/07c_chart_disturbed_by_year_corporate_groups.png

if (!exists("SETUP_LOADED")) source(if (file.exists("00_setup.R")) "00_setup.R" else "maps_scripts_for_presentation/00_setup.R")

# ---- Panel titles ----
# Acres and owner class of each analysis zone (sale group).
za <- zone_acres()

# Molpus and Lyme bought most of this land just before the 2020 list, so their panel
# titles give the purchase dates instead of the 2020 -> 2026 wording.
PANEL_TITLE <- setNames(CORP_GROUPS, CORP_GROUPS)
PANEL_TITLE[G$MW] <- "Molpus: bought 2020 (Vulcan) or 2014 (Ned Lake), mostly kept"
PANEL_TITLE[G$LY] <- "Lyme: bought March 2020, kept"

# Each panel title is wrapped to about 36 characters and followed by the group's
# acres. Panels are kept in the standard corporate group order.
panel_lab <- za |>
  filter(group %in% CORP_GROUPS) |>
  mutate(panel = paste0(vapply(PANEL_TITLE[group], function(g) paste(strwrap(g, 36), collapse = "\n"), ""),
                        "\n(", fmt_n(zone_acres), " acres)")) |>
  select(group, panel)
panel_levels <- panel_lab$panel[match(CORP_GROUPS, panel_lab$group)]

# ---- Helpers ----
# Splits a by-zone yearly table into (1) one value per corporate group per year and
# (2) one pooled value per year for all other private land, the grey reference line.
# value_fun is the calculation (e.g. weighted average canopy, or percent cleared).
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

# A grid of small charts, three per row, one per corporate group. Each panel shows that
# group's line in its own colour over the same grey other-private line, so every group
# is compared with the same baseline. All panels share the y-axis limits in lim.
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

# ---- 07a canopy cover ----
# USFS FIA Tree Canopy Cover (TCC): zone averages combined per group, weighted by
# acres. The y-axis is zoomed to the data range rather than starting at zero.
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

# ---- 07b forest cleared (single chart) ----
# Hansen forest loss: acres cleared each year as a percent of each group's land.
hansen <- by_group(
  csv("cfp_sale_footprint_hansen_by_year.csv"),
  function(d) d |> group_by(group, year) |> summarise(value = 100 * sum(loss_acres) / sum(zone_acres), .groups = "drop")
)
# 07b: all groups on one chart.
# Legend labels: the group names, with shorter wording for Molpus and Lyme (who bought
# rather than sold), plus a label for the grey other-private line.
LEGEND_LAB <- c(
  setNames(CORP_GROUPS, CORP_GROUPS),
  "Other private" = "All other private Commercial Forest land"
)
LEGEND_LAB[G$KH] <- "TRG → Verdant → The Nature Conservancy (Heartlands)"
LEGEND_LAB[G$MW] <- "Molpus (bought 2014–2020, mostly kept)"
LEGEND_LAB[G$LY] <- "Lyme (bought 2020, kept)"
# Add each corporate group's acres to its legend label.
acres_lab <- setNames(fmt_n(za$zone_acres[match(CORP_GROUPS, za$group)]), CORP_GROUPS)
LEGEND_LAB[CORP_GROUPS] <- paste0(LEGEND_LAB[CORP_GROUPS], " (", acres_lab, " ac)")
# Put the corporate groups and the other-private line in one table, in legend order.
hb <- bind_rows(select(hansen$corp, group, year, value),
                mutate(hansen$other, group = "Other private")) |>
  mutate(group = factor(group, levels = names(LEGEND_LAB)))
# One line per group in the map colours. The other-private line is grey, slightly
# thicker and has no points, so it reads as the baseline. Legend labels are wrapped to
# about 40 characters and the legend sits on the right. The y-axis starts at zero.
p07b <- ggplot(hb, aes(year, value, colour = group)) +
  geom_line(aes(linewidth = group == "Other private")) +
  geom_point(data = filter(hb, group != "Other private"), size = 1.8) +
  scale_colour_manual(values = c(GROUP_COLS[CORP_GROUPS], "Other private" = "grey60"),
                      labels = function(x) vapply(LEGEND_LAB[x], function(s) paste(strwrap(s, 40), collapse = "\n"), ""),
                      name = NULL) +
  scale_linewidth_manual(values = c(`FALSE` = 1.2, `TRUE` = 1.6), guide = "none") +
  scale_x_continuous(breaks = seq(2015, 2024, 1)) +
  scale_y_continuous(limits = c(0, NA), labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Forest cleared each year by corporate owner group",
       subtitle = "Share of each group's land where the trees were cleared",
       x = NULL, y = "Share of land cleared",
       caption = "Source: Hansen Global Forest Change (stand-replacing loss only), 2015–2024.") +
  theme_chart() +
  theme(legend.position = "right", legend.key.height = unit(0.55, "in"), legend.key.width = unit(0.4, "in"),
        legend.text = element_text(size = BASE_SIZE - 3, lineheight = 1),
        panel.grid.minor.x = element_blank())
save_chart(p07b, "07b_chart_cleared_by_year_corporate_groups.png")

# ---- 07c land disturbed ----
# LANDFIRE disturbance: acres with any disturbance each year as a percent of each
# group's land, drawn as panels with the y-axis starting at zero.
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
