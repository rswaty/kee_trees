# Where canopy fell 20+ points across the whole landscape, 2020 -> 2025:
#   left, map of loss cells coloured by owner type;
#   right, share of each land type where canopy fell 20+ points (doubles as the legend).
#
# Output: figures/08_map_canopy_change_county.png

if (!exists("SETUP_LOADED")) source(if (file.exists("00_setup.R")) "00_setup.R" else "maps_scripts_for_presentation/00_setup.R")

LOSS_PP <- -20  # "heavy loss" cutoff, canopy points

counties <- read_mainland()
parcels <- read_parcels()
by_class <- parcels |> group_by(class) |> summarise(.groups = "drop")

chg <- mask(rast(spatial("tcc_change_2020_2025.tif")), vect(counties))
zones <- rast(spatial("cfp_sale_zones.tif"))
cls <- classify(zones, rbind(cbind(CORP_ZONE_CODES, 1), cbind(OTHER_ZONE_CODES, 2)), others = 3)
cls <- mask(cls, chg)
loss <- chg <= LOSS_PP

# Share and acres of each owner type's land where canopy fell 20+ points.
class_names <- c(CORP, OTHER, NONCFP)
short <- setNames(c("Corporate", "Other private", "Not enrolled"), class_names)
share <- zonal(loss, cls, fun = "mean", na.rm = TRUE)
names(share) <- c("code", "share")
share$acres <- zonal(loss, cls, fun = "sum", na.rm = TRUE)[[2]] * ACRES_PER_PX
share$class <- factor(class_names[share$code], levels = class_names)
all_share <- global(loss, "mean", na.rm = TRUE)[1, 1]
all_acres <- sum(share$acres)

# Loss shown at 90 m, where a 90 m cell takes the owner type of any 30 m loss cell
# inside it (corporate first).
loss_cls <- aggregate(ifel(loss, cls, NA), 3, "min", na.rm = TRUE)
loss_df <- as.data.frame(loss_cls, xy = TRUE, na.rm = TRUE)
names(loss_df)[3] <- "code"
loss_df$class <- factor(class_names[loss_df$code], levels = class_names)

bb <- pad_bbox(st_bbox(counties), 0.02)

loss_map <- ggplot() +
  geom_sf(data = counties, fill = "grey97", colour = NA) +
  geom_sf(data = filter(by_class, class == CORP), fill = CLASS_PALE[[CORP]], colour = NA) +
  geom_sf(data = filter(by_class, class == OTHER), fill = CLASS_PALE[[OTHER]], colour = NA) +
  geom_tile(data = loss_df, aes(x, y, fill = class), width = res(loss_cls)[1], height = res(loss_cls)[2]) +
  geom_sf(data = counties, fill = NA, colour = "grey30", linewidth = 0.4) +
  scale_fill_manual(values = CLASS_COLS, drop = FALSE) +
  scale_bar(bb$x, bb$y, pos = "bottomright") +
  north_arrow(bb$x, bb$y, pos = "topleft") +
  coord_sf(xlim = bb$x, ylim = bb$y, expand = FALSE, datum = NA) +
  theme_map()

share_chart <- ggplot(share, aes(100 * share, forcats::fct_rev(class), fill = class)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(fmt_1(100 * share), "%  (", fmt_n(acres), " ac)")),
            hjust = -0.08, size = BASE_SIZE / 3.2, family = FONT_FAMILY) +
  scale_fill_manual(values = CLASS_COLS, guide = "none") +
  scale_y_discrete(labels = function(x) short[x]) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.75))) +
  labs(title = "Share of each land type where\ncanopy fell 20+ points", x = NULL, y = NULL) +
  theme_chart() +
  theme(
    plot.title = element_text(face = "bold", size = BASE_SIZE + 1),
    axis.text.y = element_text(size = BASE_SIZE, colour = "grey15"),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(60, 30, 10, 10)
  )

summary_text <- paste(strwrap(paste0(
  "Across all land, canopy fell 20+ points on ", fmt_1(100 * all_share), "% (",
  fmt_n(all_acres), " acres). Most of this land is northern hardwoods, ",
  "so anything above 1–5% is notable; corporate land reached ",
  fmt_1(100 * share$share[share$class == CORP]), "%."
), 52), collapse = "\n")

save_layout(
  "08_map_canopy_change_county.png",
  panels = list(loss_map),
  side = list(share_chart, text_block(summary_text, size = BASE_SIZE)),
  side_heights = c(1, 0.55),
  side_width = 0.5,
  title = "Where Canopy Cover Fell 20+ Points, 2020–2025",
  subtitle = "Houghton and Keweenaw counties",
  caption = paste0(
    "Pale colours on the map: Commercial Forest land. Not enrolled: all land outside the program, public and private.\n",
    "USFS FIA Tree Canopy Cover, 30 m cells (map shown at 90 m); owner type from the 2020 Michigan DNR Commercial Forest list."
  )
)
