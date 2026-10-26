# Corporate owners relative to each other (and to other private owners): share of
# acres heavily cut, 2020-2025.
#
# Output: figures/06_chart_corporate_heavily_cut.png

if (!exists("SETUP_LOADED")) source(if (file.exists("00_setup.R")) "00_setup.R" else "maps_scripts_for_presentation/00_setup.R")

# Parcels of 20+ acres with canopy data; heavily cut = parcel average canopy fell
# more than 20 points, 2020 -> 2025.
# Top panel: one bar per corporate owner group, coloured as on the maps.
corp <- read_parcels() |>
  st_drop_geometry() |>
  filter(group %in% CORP_GROUPS, !is.na(major_disturbance)) |>
  group_by(label = group) |>
  summarise(cut = sum(acres[major_disturbance]), acres = sum(acres), .groups = "drop") |>
  mutate(panel = "Corporate owners, by where the land went", fill = GROUP_COLS[label])

# Bottom panel: totals for all corporate and all other private land, for comparison.
# The analysis calls corporate owners "Institutional".
by_class <- csv("cfp_sellers_by_owner_class.csv") |>
  mutate(label = if_else(owner_class == "Institutional", paste("All", tolower(CORP)), paste("All", tolower(OTHER)))) |>
  group_by(label) |>
  summarise(acres = sum(acres), cut = sum(acres_major_disturbance), .groups = "drop") |>
  mutate(panel = "Totals",
         fill = if_else(grepl("corporate", label), CLASS_COLS[[CORP]], CLASS_COLS[[OTHER]]))

# Percent heavily cut for every bar; bars are sorted from lowest to highest.
d <- bind_rows(corp, by_class) |>
  mutate(pct = 100 * cut / acres,
         panel = factor(panel, levels = c("Corporate owners, by where the land went", "Totals")),
         label = reorder(label, pct))

# Horizontal bars labelled with the percent and the acres behind it. The two panels
# are stacked, each only as tall as its number of bars, and the x-axis is extended
# so the labels fit to the right of the longest bar.
p <- ggplot(d, aes(pct, label, fill = fill)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(fmt_1(pct), "%  (", fmt_n(cut), " of ", fmt_n(acres), " ac)")),
            hjust = -0.05, size = BASE_SIZE / 3.5) +
  facet_grid(panel ~ ., scales = "free_y", space = "free_y") +
  scale_fill_identity() +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.55))) +
  labs(
    title = "Share of acres heavily cut, 2020–2025",
    subtitle = "Acres in parcels whose average canopy cover fell more than 20 points",
    x = NULL, y = NULL,
    caption = paste0(
      "Source: USFS FIA Tree Canopy Cover; parcels of 20+ acres in the 2020 Michigan DNR Commercial Forest list. ",
      "Molpus includes 6 parcels it sold or took out of the program.\n",
      CORP_NOTE, "\n", OTHER_NOTE
    )
  ) +
  theme_chart() +
  theme(panel.grid.major.y = element_blank(),
        strip.text.y = element_blank(),
        panel.spacing = unit(0.35, "in"))

save_chart(p, "06_chart_corporate_heavily_cut.png")
