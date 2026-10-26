# Alternative designs for figures 07b (forest cleared) and 07c (land disturbed), for
# reviewers. Not run by make_all.R; the main figures in figures/ are unchanged.
#   Heat maps: one row per owner group, one column per year, colour = share of the
#   group's land cleared or disturbed that year.
#   07c lines: all groups on one chart (same style as the current 07b).
#
# Run from the kee_trees folder:
#   Rscript maps_scripts_for_presentation/alternatives/07_alt_cleared_disturbed.R
# Outputs: figures_alternatives/07b_alt_heatmap_cleared.png
#          figures_alternatives/07c_alt_heatmap_disturbed.png
#          figures_alternatives/07c_alt_lines_disturbed.png

source(if (file.exists("00_setup.R")) "00_setup.R" else "maps_scripts_for_presentation/00_setup.R")

# ---- Output folder ----
# Alternatives are saved to their own folder so they don't mix with the main figures.
ALT_DIR <- file.path(HERE, "figures_alternatives")
dir.create(ALT_DIR, showWarnings = FALSE)
save_alt <- function(p, file) {
  ggsave(file.path(ALT_DIR, file), p, width = FIG_W, height = FIG_H, dpi = DPI, device = ragg::agg_png, bg = "white")
  message("Wrote ", file.path(ALT_DIR, file))
}

# ---- Row labels ----
# One row per corporate owner group plus one row for all other private land combined.
# Labels use the same wording as figure 07b, with each row's acres added.
za <- zone_acres()
OTHER_LAB <- "All other private land"
ROW_LAB <- c(setNames(CORP_GROUPS, CORP_GROUPS), setNames(OTHER_LAB, OTHER_LAB))
ROW_LAB[G$KH] <- "TRG → Verdant → The Nature Conservancy (Heartlands)"
ROW_LAB[G$MW] <- "Molpus (bought 2014–2020, mostly kept)"
ROW_LAB[G$LY] <- "Lyme (bought 2020, kept)"
acres <- c(setNames(za$zone_acres[match(CORP_GROUPS, za$group)], CORP_GROUPS),
           setNames(sum(za$zone_acres[za$group %in% OTHER_GROUPS]), OTHER_LAB))
ROW_LAB <- paste0(ROW_LAB, " (", fmt_n(acres[names(ROW_LAB)]), " ac)")
names(ROW_LAB) <- c(CORP_GROUPS, OTHER_LAB)

# ---- Data ----
# Share of each group's land flagged each year; acres_col names the flagged-acres column.
# The two other-private groups are pooled into one row.
yearly <- function(df, acres_col) {
  df |>
    mutate(group = ZONE_LAB[zone], group = if_else(group %in% OTHER_GROUPS, OTHER_LAB, group)) |>
    filter(group %in% names(ROW_LAB)) |>
    group_by(group, year) |>
    summarise(value = 100 * sum(.data[[acres_col]]) / sum(zone_acres), .groups = "drop") |>
    mutate(group = factor(group, levels = rev(names(ROW_LAB))))
}

# Hansen forest loss already carries each zone's acres. The LANDFIRE table gets them
# joined in from the zone list (filling any gaps), so shares can be calculated.
hansen <- yearly(csv("cfp_sale_footprint_hansen_by_year.csv"), "loss_acres")
landfire <- yearly(
  inner_join(csv("cfp_sale_landfire_annual_by_zone.csv"), select(za, zone, zone_acres), by = "zone", suffix = c("", ".z")) |>
    mutate(zone_acres = coalesce(zone_acres, zone_acres.z)),
  "acres"
)

# ---- Heat maps ----
# One tile per group and year, shaded from pale (none) to dark red (the highest value
# in the chart). Each tile shows its percent; text turns white on the darkest tiles
# so it stays readable. Long row labels are wrapped to about 38 characters.
heatmap <- function(d, title, subtitle, caption, fill_name) {
  hi <- max(d$value)
  ggplot(d, aes(year, group, fill = value)) +
    geom_tile(colour = "white", linewidth = 0.8) +
    geom_text(aes(label = ifelse(value < 0.05, "0", sprintf("%.1f", value)),
                  colour = value > hi * 0.55),
              size = BASE_SIZE / 4.2, family = FONT_FAMILY) +
    scale_fill_gradientn(colours = c("#f7f7f7", "#fde0c5", "#f4a261", "#d1495b", "#6a040f"),
                         limits = c(0, hi), name = fill_name,
                         labels = function(x) paste0(x, "%"),
                         guide = guide_colourbar(title.position = "top")) +
    scale_colour_manual(values = c(`FALSE` = "grey20", `TRUE` = "white"), guide = "none") +
    scale_x_continuous(breaks = unique(d$year), expand = c(0, 0)) +
    scale_y_discrete(labels = function(x) vapply(ROW_LAB[x], function(s) paste(strwrap(s, 38), collapse = "\n"), "")) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL, caption = caption) +
    theme_chart() +
    theme(panel.grid = element_blank(), legend.position = "top", legend.justification = "left",
          legend.key.width = unit(0.9, "in"), legend.key.height = unit(0.18, "in"),
          legend.title = element_text(size = BASE_SIZE - 3), legend.text = element_text(size = BASE_SIZE - 4),
          axis.text.y = element_text(size = BASE_SIZE - 3, lineheight = 1, colour = "grey15"),
          axis.text.x = element_text(size = BASE_SIZE - 4))
}

save_alt(
  heatmap(hansen, "Forest cleared each year by corporate owner group",
          "Share of each group's land where the trees were cleared (%). Darker = more clearing.",
          "Source: Hansen Global Forest Change (stand-replacing loss only), 2015–2024.",
          "Share of land cleared"),
  "07b_alt_heatmap_cleared.png"
)
save_alt(
  heatmap(landfire, "Land disturbed each year by corporate owner group",
          "Share of each group's land with any disturbance recorded that year (%). Darker = more disturbance.",
          "Source: LANDFIRE Annual Disturbance, 2010–2025, all disturbance types and severities.",
          "Share of land disturbed"),
  "07c_alt_heatmap_disturbed.png"
)

# ---- 07c as a line chart ----
# 07c as one line chart, matching the current 07b.
# Groups go back to legend order (the heat maps listed them in reverse). The grey
# other-private line is slightly thicker and has no points, so it reads as the baseline.
ld <- landfire |> mutate(group = factor(group, levels = names(ROW_LAB)))
p_lines <- ggplot(ld, aes(year, value, colour = group)) +
  geom_line(aes(linewidth = group == OTHER_LAB)) +
  geom_point(data = filter(ld, group != OTHER_LAB), size = 1.8) +
  scale_colour_manual(values = c(GROUP_COLS[CORP_GROUPS], setNames("grey60", OTHER_LAB)),
                      labels = function(x) vapply(ROW_LAB[x], function(s) paste(strwrap(s, 40), collapse = "\n"), ""),
                      name = NULL) +
  scale_linewidth_manual(values = c(`FALSE` = 1.2, `TRUE` = 1.6), guide = "none") +
  scale_x_continuous(breaks = seq(2010, 2025, 3)) +
  scale_y_continuous(limits = c(0, NA), labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Land disturbed each year by corporate owner group",
       subtitle = "Share of each group's land with any disturbance recorded that year",
       x = NULL, y = "Share of land disturbed",
       caption = "Source: LANDFIRE Annual Disturbance, 2010–2025, all disturbance types and severities.") +
  theme_chart() +
  theme(legend.position = "right", legend.key.height = unit(0.55, "in"), legend.key.width = unit(0.4, "in"),
        legend.text = element_text(size = BASE_SIZE - 3, lineheight = 1), panel.grid.minor.x = element_blank())
save_alt(p_lines, "07c_alt_lines_disturbed.png")
