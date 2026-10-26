# Where canopy fell 20+ points across the whole landscape, 2025 compared with 2020
# (net change per 30 m cell, not a sum of yearly losses):
#   left, map of loss cells coloured by owner type, with the western Houghton townships
#   that hold the most loss outlined;
#   right, one 100% bar per land type, so the share that fell 20+ points is seen
#   against all of that land.
#
# Output: figures/08_map_canopy_change_county.png

if (!exists("SETUP_LOADED")) source(if (file.exists("00_setup.R")) "00_setup.R" else "maps_scripts_for_presentation/00_setup.R")

LOSS_PP <- -20                         # "heavy loss" cutoff, canopy points
HOT_TOWNSHIPS <- c("Stanton", "Elm River")  # western Houghton County, highest loss

# ---- Data ----
# County outlines, and the 2020 Commercial Forest Program (CFP) parcels merged into
# one shape per owner type (drawn as pale background on the map).
counties <- read_mainland()
parcels <- read_parcels()
by_class <- parcels |> group_by(class) |> summarise(.groups = "drop")

# Canopy change raster (USFS FIA Tree Canopy Cover (TCC), 2025 minus 2020, in canopy
# points), clipped to the mainland. Each cell is given an owner type from the sale
# zones raster: 1 = corporate, 2 = other private, 3 = not enrolled. loss is TRUE
# where canopy fell 20 points or more.
chg <- mask(rast(spatial("tcc_change_2020_2025.tif")), vect(counties))
zones <- rast(spatial("cfp_sale_zones.tif"))
cls <- classify(zones, rbind(cbind(CORP_ZONE_CODES, 1), cbind(OTHER_ZONE_CODES, 2)), others = 3)
cls <- mask(cls, chg)
loss <- chg <= LOSS_PP

# ---- Summaries ----
# Share of each owner type's land where canopy fell 20+ points (the mean of a
# TRUE/FALSE layer is the share of TRUE cells).
class_names <- c(CORP, OTHER, NONCFP)
short <- setNames(c("Corporate", "Other private", "Not enrolled"), class_names)
share <- zonal(loss, cls, fun = "mean", na.rm = TRUE)
names(share) <- c("code", "share")
share$class <- factor(class_names[share$code], levels = class_names)

# Every cell of each land type in one of three change classes (canopy points, 2025 minus 2020).
# Neutral greys, so the chart doesn't compete with the owner colours on the map.
CHANGE_LAB <- c("Fell 20+ points", "Fell 10–20", "No major loss")
CHANGE_COLS <- setNames(c("grey20", "grey60", "grey88"), CHANGE_LAB)
# Sort cells into the three change classes, count cells for every owner type and
# change class combination, and turn the counts into percents of each owner type.
bins <- classify(chg, rbind(c(-Inf, LOSS_PP, 1), c(LOSS_PP, -10, 2), c(-10, Inf, 3)), right = TRUE)
stack <- as.data.frame(crosstab(c(cls, bins), long = TRUE))
names(stack) <- c("code", "bin", "n")
stack <- stack |>
  mutate(class = factor(class_names[as.integer(as.character(code))], levels = class_names),
         change = factor(CHANGE_LAB[as.integer(as.character(bin))], levels = rev(CHANGE_LAB))) |>
  group_by(class) |>
  mutate(pct = 100 * n / sum(n)) |>
  ungroup()

# The outlined townships: their share of all land and of all 20+ point loss.
hot <- read_townships() |>
  filter(county == "Houghton", township %in% HOT_TOWNSHIPS) |>
  summarise()
# Count cells inside the townships against all mainland cells, and loss cells inside
# them against all loss cells.
in_hot <- !is.na(mask(chg, vect(hot)))
hot_land <- global(in_hot, "sum", na.rm = TRUE)[1, 1] / global(!is.na(chg), "sum", na.rm = TRUE)[1, 1]
hot_loss <- global(loss & in_hot, "sum", na.rm = TRUE)[1, 1] / global(loss, "sum", na.rm = TRUE)[1, 1]

# ---- Loss map ----
# Loss shown at 90 m, where a 90 m cell takes the owner type of any 30 m loss cell
# inside it (corporate first). Coarser cells keep small patches visible at slide size
# and make the map faster to draw. The result is a table of cell centres for plotting.
loss_cls <- aggregate(ifel(loss, cls, NA), 3, "min", na.rm = TRUE)
loss_df <- as.data.frame(loss_cls, xy = TRUE, na.rm = TRUE)
names(loss_df)[3] <- "code"
loss_df$class <- factor(class_names[loss_df$code], levels = class_names)

# The township label sits left of the map, in the blank space beside it (clip = "off").
bb <- pad_bbox(st_bbox(counties), 0.02)
hb <- st_bbox(hot)
hot_lab <- paste0(paste(HOT_TOWNSHIPS, collapse = " &\n"), " twps.\n",
                  round(100 * hot_land), "% of the land\n", round(100 * hot_loss), "% of the loss")

# Map layers: pale county land, pale owner-type land, loss cells in strong owner
# colours, county borders, then the dashed township outline and its label.
loss_map <- ggplot() +
  geom_sf(data = counties, fill = "grey97", colour = NA) +
  geom_sf(data = filter(by_class, class == CORP), fill = CLASS_PALE[[CORP]], colour = NA) +
  geom_sf(data = filter(by_class, class == OTHER), fill = CLASS_PALE[[OTHER]], colour = NA) +
  geom_tile(data = loss_df, aes(x, y, fill = class), width = res(loss_cls)[1], height = res(loss_cls)[2]) +
  geom_sf(data = counties, fill = NA, colour = "grey30", linewidth = 0.4) +
  geom_sf(data = hot, fill = NA, colour = "grey10", linewidth = 0.9, linetype = "22") +
  annotate("text", x = hb[["xmin"]] - 3000, y = (hb[["ymin"]] + hb[["ymax"]]) / 2, label = hot_lab,
           hjust = 1, size = BASE_SIZE / 3.2, lineheight = 0.95, fontface = "bold", colour = "grey10",
           family = FONT_FAMILY) +
  scale_fill_manual(values = CLASS_COLS, drop = FALSE) +
  scale_bar(bb$x, bb$y, pos = "bottomright") +
  north_arrow(bb$x, bb$y, pos = "topleft") +
  coord_sf(xlim = bb$x, ylim = bb$y, expand = FALSE, datum = NA, clip = "off") +
  theme_map()

# Side chart: each bar is all of one land type. The owner-type square left of each bar
# is the map legend; the 20+ point share is printed at the bar end.
# "inside" finds the middle of the "No major loss" segment of each bar, where its
# percent is printed.
inside <- stack |>
  group_by(class) |>
  arrange(desc(change), .by_group = TRUE) |>
  mutate(mid = cumsum(pct) - pct / 2) |>
  ungroup() |>
  filter(change == "No major loss")

# Stacked 100% bars, one per land type, listed top to bottom in the order corporate,
# other private, not enrolled. The x-axis runs from -12 to 122 so there is room for
# the coloured square on the left (at -7) and the bold share label on the right (at 103).
share_chart <- ggplot(stack, aes(pct, forcats::fct_rev(class))) +
  geom_col(aes(fill = change), width = 0.62) +
  geom_text(data = inside, aes(mid, forcats::fct_rev(class), label = paste0(round(pct), "%")),
            size = (BASE_SIZE + 2) / 3.8, colour = "grey30", family = FONT_FAMILY) +
  geom_point(data = share, aes(-7, forcats::fct_rev(class), colour = class), shape = 15, size = 9) +
  geom_text(data = share, aes(103, forcats::fct_rev(class), label = paste0(fmt_1(100 * share), "%")),
            hjust = 0, size = (BASE_SIZE + 2) / 3, fontface = "bold", colour = "grey10", family = FONT_FAMILY) +
  scale_fill_manual(values = CHANGE_COLS, breaks = CHANGE_LAB, name = NULL) +
  scale_colour_manual(values = CLASS_COLS, guide = "none") +
  scale_y_discrete(labels = function(x) short[x]) +
  scale_x_continuous(limits = c(-12, 122), expand = expansion(0)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(title = "Share of each land type where canopy fell", x = NULL, y = NULL) +
  theme_chart() +
  theme(
    plot.title = element_text(face = "bold", size = BASE_SIZE + 3),
    axis.text.y = element_text(size = BASE_SIZE + 2, colour = "grey15"),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    legend.text = element_text(size = BASE_SIZE - 1),
    plot.margin = margin(30, 20, 60, 10)
  )

# ---- Save ----
# Map on the left with a wide left margin for the township label; title and bar chart
# in the right-hand column.
save_layout(
  "08_map_canopy_change_county.png",
  panels = list(loss_map + theme(plot.margin = margin(4, 4, 4, 130))),
  side = list(share_chart),
  side_width = 0.45,
  title_pos = "side",
  title = "Where Canopy Cover Fell 20+ Points, 2020–2025",
  subtitle = "Houghton and Keweenaw counties. 2025 canopy compared with 2020, cell by cell",
  caption = paste0(
    "Map: cells where canopy fell 20+ points, coloured by owner type (squares beside the bars). Pale: Commercial Forest land.\n",
    "USFS FIA Tree Canopy Cover, 30 m cells; owner type from the 2020 Michigan DNR Commercial Forest list."
  )
)
