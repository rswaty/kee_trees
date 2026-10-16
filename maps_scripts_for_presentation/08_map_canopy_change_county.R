# Disturbance across the whole landscape, 2020 -> 2025:
#   left, canopy change on all land with corporate land outlined;
#   right, cells where canopy fell 20+ points, coloured by owner type.
#
# Output: figures/08_map_canopy_change_county.png

if (!exists("SETUP_LOADED")) source(if (file.exists("00_setup.R")) "00_setup.R" else "maps_scripts_for_presentation/00_setup.R")

LOSS_PP <- -20  # "heavy loss" cutoff, canopy points

counties <- read_mainland()
parcels <- read_parcels()
by_class <- parcels |> group_by(class) |> summarise(.groups = "drop")
corp_land <- filter(by_class, class == CORP)

chg <- mask(rast(spatial("tcc_change_2020_2025.tif")), vect(counties))
zones <- rast(spatial("cfp_sale_zones.tif"))
cls <- classify(zones, rbind(cbind(CORP_ZONE_CODES, 1), cbind(OTHER_ZONE_CODES, 2)), others = 3)
cls <- mask(cls, chg)
loss <- chg <= LOSS_PP

# Share of each owner type's land where canopy fell 20+ points.
share <- zonal(loss, cls, fun = "mean", na.rm = TRUE)
names(share) <- c("code", "share")
class_names <- c(CORP, OTHER, NONCFP)
share$class <- class_names[share$code]

# Display grids: canopy change averaged to 120 m; loss shown at 90 m, where a 90 m
# cell takes the owner type of any 30 m loss cell inside it (corporate first).
chg_df <- as.data.frame(aggregate(chg, 4, "mean", na.rm = TRUE), xy = TRUE, na.rm = TRUE)
names(chg_df)[3] <- "v"
chg_df$x <- round(chg_df$x, 1)
chg_df$y <- round(chg_df$y, 1)
loss_cls <- aggregate(ifel(loss, cls, NA), 3, "min", na.rm = TRUE)
loss_df <- as.data.frame(loss_cls, xy = TRUE, na.rm = TRUE)
names(loss_df)[3] <- "code"
loss_df$class <- factor(class_names[loss_df$code], levels = class_names)
short <- setNames(c("Corporate", "Other private", "Not enrolled"), class_names)

bb <- pad_bbox(st_bbox(counties), 0.02)

change_map <- ggplot() +
  geom_sf(data = counties, fill = "grey90", colour = NA) +
  geom_raster(data = chg_df, aes(x, y, fill = v)) +
  geom_sf(data = corp_land, fill = NA, colour = "#1b1f5e", linewidth = 0.18) +
  geom_sf(data = counties, fill = NA, colour = "grey30", linewidth = 0.4) +
  scale_fill_gradient2(low = "#8c510a", mid = "#f5f5f5", high = "#01665e", midpoint = 0,
                       limits = c(-30, 30), breaks = c(-30, -15, 0, 15, 30), oob = squish,
                       labels = c("-30 or less", "-15", "0", "+15", "+30 or more"),
                       name = "Change in canopy cover\n(percentage points)") +
  guides(fill = guide_colourbar(barwidth = unit(3.2, "in"), barheight = unit(0.22, "in"),
                                title.position = "top", direction = "horizontal")) +
  scale_bar(bb$x, bb$y, pos = "bottomright") +
  north_arrow(bb$x, bb$y, pos = "topleft") +
  coord_sf(xlim = bb$x, ylim = bb$y, expand = FALSE, datum = NA) +
  labs(title = "Canopy change, all land") +
  theme_map() +
  theme(legend.position = "bottom", legend.title = element_text(size = BASE_SIZE - 3),
        legend.text = element_text(size = BASE_SIZE - 5))

loss_map <- ggplot() +
  geom_sf(data = counties, fill = "grey97", colour = NA) +
  geom_sf(data = filter(by_class, class == CORP), fill = CLASS_PALE[[CORP]], colour = NA) +
  geom_sf(data = filter(by_class, class == OTHER), fill = CLASS_PALE[[OTHER]], colour = NA) +
  geom_tile(data = loss_df, aes(x, y, fill = class), width = res(loss_cls)[1], height = res(loss_cls)[2]) +
  geom_sf(data = counties, fill = NA, colour = "grey30", linewidth = 0.4) +
  scale_fill_manual(values = CLASS_COLS, drop = FALSE,
                    name = "Share of each land type where\ncanopy fell 20+ points",
                    labels = function(x) {
                      s <- share$share[match(x, share$class)]
                      paste0(short[x], ": ", fmt_1(100 * s), "%")
                    }) +
  guides(fill = guide_legend(ncol = 1)) +
  coord_sf(xlim = bb$x, ylim = bb$y, expand = FALSE, datum = NA) +
  labs(title = "Where canopy fell 20+ points") +
  theme_map() +
  theme(legend.position = "bottom", legend.title = element_text(size = BASE_SIZE - 3),
        legend.text = element_text(size = BASE_SIZE - 4))

notes <- paste0(
  paste(strwrap("Left: brown = canopy lost, green = canopy gained. Dark outlines mark corporate land.", 42), collapse = "\n"),
  "\n\n",
  paste(strwrap(paste0("Right: pale colours show corporate and other private Commercial Forest land; ",
                       "bright colours show where the canopy fell 20 points or more. ",
                       "\"Not enrolled\" is all land outside the Commercial Forest Program, public and private."), 42),
        collapse = "\n")
)

save_layout(
  "08_map_canopy_change_county.png",
  panels = list(change_map + theme(legend.position = "none"), loss_map + theme(legend.position = "none")),
  side = list(get_legend(change_map), get_legend(loss_map), text_block(notes, size = BASE_SIZE - 5)),
  side_heights = c(0.55, 0.75, 1),
  side_width = 0.4,
  title = "Canopy change across Houghton and Keweenaw counties, 2020 → 2025",
  subtitle = "Corporate land had the largest share of heavy canopy loss",
  caption = paste0(
    "Source: USFS FIA Tree Canopy Cover, 2020 and 2025, 30 m cells. Shown at 120 m (left) and 90 m (right) for legibility; shares are computed at 30 m.\n",
    "Owner type from the 2020 Michigan DNR Commercial Forest list. ", CORP_NOTE
  )
)
