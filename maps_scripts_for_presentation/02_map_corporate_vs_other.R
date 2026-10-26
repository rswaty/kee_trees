# Who? Corporate owners as a whole vs. other private owners, by 2020 owner.
#
# Output: figures/02_map_corporate_vs_other.png

if (!exists("SETUP_LOADED")) source(if (file.exists("00_setup.R")) "00_setup.R" else "maps_scripts_for_presentation/00_setup.R")

parcels <- read_parcels()
counties <- read_mainland()

# Acres and parcel counts for the legend labels.
cls <- parcels |>
  st_drop_geometry() |>
  group_by(class) |>
  summarise(acres = sum(acres), n = n(), .groups = "drop") |>
  mutate(label = paste0(class, "\n", fmt_n(acres), " acres, ", fmt_n(n), " parcels"))
lab <- setNames(cls$label, cls$class)

# Merge parcels into one shape per owner type (faster to draw, no parcel lines).
by_class <- parcels |>
  group_by(class) |>
  summarise(.groups = "drop")

bb <- pad_bbox(st_bbox(parcels), 0.04)

main <- ggplot() +
  geom_sf(data = counties, fill = "grey96", colour = NA) +
  geom_sf(data = by_class, aes(fill = class), colour = NA) +
  geom_sf(data = counties, fill = NA, colour = "grey35", linewidth = 0.4) +
  scale_fill_manual(values = CLASS_COLS[c(CORP, OTHER)], labels = lab, name = NULL) +
  scale_bar(bb$x, bb$y, pos = "bottomright") +
  north_arrow(bb$x, bb$y, pos = "topleft") +
  coord_sf(xlim = bb$x, ylim = bb$y, expand = FALSE, datum = NA) +
  theme_map() +
  theme(legend.position = "right",
        legend.text = element_text(size = BASE_SIZE + 1, margin = margin(t = 6, b = 6, l = 6)),
        legend.key.size = unit(0.6, "in"),
        legend.key.spacing.y = unit(0.15, "in"))

# The legend goes in the side column, above the notes on who counts as corporate.
notes <- paste(strwrap(CORP_NOTE, 62), collapse = "\n") |>
  paste0("\n\n", paste(strwrap(OTHER_NOTE, 62), collapse = "\n"))

save_layout(
  "02_map_corporate_vs_other.png",
  panels = list(main + theme(legend.position = "none", plot.margin = margin(4, 4, 4, 40))),
  side = list(get_legend(main), text_block(notes, size = BASE_SIZE - 2)),
  side_heights = c(0.9, 1.1),
  side_width = 0.6,
  title_pos = "side",
  title = "Commercial Forest land by owner type, 2020",
  subtitle = "Corporate owners hold just over half of the enrolled land",
  caption = "Owner in the 2020 Michigan DNR Commercial Forest list. All enrolled parcels shown."
)
