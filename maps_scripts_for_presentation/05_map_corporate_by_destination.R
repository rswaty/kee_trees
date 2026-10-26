# Corporate owners relative to each other: corporate land coloured by where it went,
# 2020 -> 2026. Other private land is shown pale, for reference.
#
# Output: figures/05_map_corporate_by_destination.png

if (!exists("SETUP_LOADED")) source(if (file.exists("00_setup.R")) "00_setup.R" else "maps_scripts_for_presentation/00_setup.R")

parcels <- read_parcels()
counties <- read_mainland()

# One shape per corporate group, with its acres in the legend label. TRG land sold to
# other owners is split by whether the parcel is still enrolled in 2026.
levels05 <- append(setdiff(CORP_GROUPS, G$TO), c(TO_KEPT, TO_LEFT), after = 3)
cols05 <- c(GROUP_COLS[setdiff(CORP_GROUPS, G$TO)], TO_SPLIT_COLS)[levels05]
grp <- parcels |>
  filter(group %in% CORP_GROUPS) |>
  mutate(group = case_when(
    group == G$TO & exit_bucket == "left_cfp" ~ TO_LEFT,
    group == G$TO ~ TO_KEPT,
    TRUE ~ group
  )) |>
  group_by(group) |>
  summarise(acres = sum(acres), .groups = "drop") |>
  mutate(group = factor(group, levels = levels05))
lab <- setNames(paste0(grp$group, " (", fmt_n(grp$acres), " ac)"), as.character(grp$group))
other <- parcels |> filter(class == OTHER) |> summarise()

bb <- pad_bbox(st_bbox(parcels), 0.04)

main <- ggplot() +
  geom_sf(data = counties, fill = "grey97", colour = NA) +
  geom_sf(data = other, fill = "grey85", colour = NA) +
  geom_sf(data = grp, aes(fill = group), colour = NA) +
  geom_sf(data = counties, fill = NA, colour = "grey35", linewidth = 0.4) +
  scale_fill_manual(values = cols05, labels = lab, name = NULL, drop = FALSE) +
  scale_bar(bb$x, bb$y, pos = "bottomright") +
  north_arrow(bb$x, bb$y, pos = "topleft") +
  coord_sf(xlim = bb$x, ylim = bb$y, expand = FALSE, datum = NA) +
  theme_map() +
  theme(legend.position = "right",
        legend.text = element_text(size = BASE_SIZE - 1),
        legend.key.size = unit(0.36, "in"),
        legend.key.spacing.y = unit(0.06, "in"))

# Side-panel timeline. Dates come from the 2026 state list; Molpus bought in two
# rounds, so it gets the middle date rather than a range.
timeline <- paste0(
  "When the land changed hands\n",
  "(dates from the 2026 state list)\n\n",
  "TRG sold to Verdant: ", owner_date_text("Verdant"), "\n",
  "Verdant sold the Heartlands land to\nThe Nature Conservancy: ", owner_date_text("Nature Conservancy"), "\n",
  "Verdant sold land to American Songbird: 2026\n",
  "Molpus bought most of its land: ", month_year(owner_start_dates()$molpus), "\n",
  "Lyme bought its land: ", owner_date_text("\\bLyme\\b"), "\n\n",
  "Grey: other private Commercial Forest land."
)

save_layout(
  "05_map_corporate_by_destination.png",
  panels = list(main + theme(legend.position = "none", plot.margin = margin(4, 4, 4, 40))),
  side = list(get_legend(main), text_block(timeline, size = BASE_SIZE - 2)),
  side_heights = c(1, 1.05),
  side_width = 0.6,
  title_pos = "side",
  title = "Corporate land by owner, 2020 → 2026",
  subtitle = "Where each corporate owner's 2020 land went",
  caption = "Owners from the Michigan DNR Commercial Forest lists, 2020 and 2026, plus the Keweenaw Heartlands and American Songbird sale boundaries."
)
