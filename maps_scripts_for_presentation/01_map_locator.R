# Where? Houghton and Keweenaw counties in the western Upper Peninsula of Michigan.
# This is the only locator map; the other maps don't repeat it.
#
# Output: figures/01_map_locator.png

if (!exists("SETUP_LOADED")) source(if (file.exists("00_setup.R")) "00_setup.R" else "maps_scripts_for_presentation/00_setup.R")

L <- read_locator()
hk <- read_counties()
parcels <- read_parcels()

to_grid <- function(lon, lat) st_coordinates(st_transform(st_sfc(st_point(c(lon, lat)), crs = 4326), GRID_CRS))
win <- st_bbox(st_transform(st_as_sfc(st_bbox(c(xmin = -92.4, ymin = 45.1, xmax = -84.3, ymax = 48.95), crs = 4326)), GRID_CRS))
xlim <- c(win$xmin, win$xmax)
ylim <- c(win$ymin, win$ymax)

with_xy <- function(df) {
  xy <- t(mapply(to_grid, df$lon, df$lat))
  df$x <- xy[, 1]
  df$y <- xy[, 2]
  df
}
labels <- with_xy(data.frame(
  lon = c(-86.55, -89.9, -91.75, -87.2, -86.55, -86.2),
  lat = c(46.2, 45.45, 47.35, 48.75, 47.75, 45.4),
  label = c("MICHIGAN", "WISCONSIN", "MINNESOTA", "ONTARIO", "Lake Superior", "Lake Michigan"),
  water = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
))
towns <- with_xy(data.frame(
  name = c("Houghton", "Marquette", "Duluth"),
  lon = c(-88.569, -87.395, -92.100),
  lat = c(47.122, 46.544, 46.787)
))
lab_xy <- to_grid(-86.95, 47.25)
tip_xy <- to_grid(-87.75, 47.25)

main <- ggplot() +
  geom_sf(data = L$canada, fill = LAND, colour = "grey60", linewidth = 0.25) +
  geom_sf(data = L$region_states, fill = LAND, colour = "grey50", linewidth = 0.4) +
  geom_sf(data = L$mi_counties, fill = NA, colour = "grey78", linewidth = 0.2) +
  geom_sf(data = L$lakes, fill = WATER, colour = NA) +
  geom_sf(data = hk, fill = HIGHLIGHT, colour = "grey15", linewidth = 0.3) +
  geom_point(data = towns, aes(x, y), size = 2, colour = "grey15") +
  geom_text(data = towns, aes(x, y, label = name), nudge_y = -9000, size = BASE_SIZE / 4.2,
            colour = "grey15", family = FONT_FAMILY) +
  geom_text(data = filter(labels, !water), aes(x, y, label = label), size = BASE_SIZE / 3.6,
            colour = "grey45", fontface = "bold", family = FONT_FAMILY) +
  geom_text(data = filter(labels, water), aes(x, y, label = label), size = BASE_SIZE / 3.4,
            colour = "#4a7aa7", fontface = "italic", family = FONT_FAMILY) +
  annotate("segment", x = lab_xy[1] - 3000, y = lab_xy[2], xend = tip_xy[1], yend = tip_xy[2],
           colour = HIGHLIGHT, linewidth = 0.6) +
  annotate("text", x = lab_xy[1], y = lab_xy[2], label = "Houghton &\nKeweenaw\ncounties",
           hjust = 0, size = BASE_SIZE / 3, colour = HIGHLIGHT, fontface = "bold", lineheight = 0.95,
           family = FONT_FAMILY) +
  scale_bar(xlim, ylim, pos = "bottomleft") +
  north_arrow(xlim, ylim, pos = "topleft") +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE, datum = NA) +
  theme_map() +
  theme(panel.background = element_rect(fill = WATER, colour = NA),
        panel.border = element_rect(fill = NA, colour = "grey40", linewidth = 0.5))

totals <- csv("hk_disturbance_totals.csv")
cur <- totals[totals$period == "Current (LANDFIRE average, 2010–2025)", ]
land_acres <- cur$annual_acres / cur$pct_of_land * 100
cls <- parcels |>
  st_drop_geometry() |>
  group_by(class) |>
  summarise(acres = sum(acres))
cfp_acres <- sum(cls$acres)
corp_acres <- cls$acres[cls$class == CORP]
facts <- paste0(
  "Land area:\n", fmt_n(land_acres), " acres\n\n",
  "Enrolled in the Commercial\nForest Program (2020):\n", fmt_n(cfp_acres), " acres\n(",
  round(100 * cfp_acres / land_acres), "% of the land)\n\n",
  "Owned by corporate owners:\n", fmt_n(corp_acres), " acres\n(",
  round(100 * corp_acres / cfp_acres), "% of enrolled land)"
)

save_layout(
  "01_map_locator.png",
  panels = list(main),
  side = list(text_block(facts, size = BASE_SIZE - 1)),
  side_width = 0.27,
  title = "Study area: Houghton and Keweenaw counties",
  subtitle = "Western Upper Peninsula of Michigan",
  caption = "Boundaries: US Census Bureau (2024), Natural Earth. Land area is the mainland, open water excluded. Enrolled acres: Michigan DNR Commercial Forest list, 2020."
)
