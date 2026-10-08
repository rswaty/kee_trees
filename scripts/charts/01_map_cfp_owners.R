# Map: CFP land by owner group, 2020 -> 2026.
# Data: output_spatial/cfp_sale_zones.tif + output_csvs/cfp_sale_zone_labels.csv (scripts/11).
if (!exists("CHART_SETUP_LOADED")) source(file.path(if (dir.exists("scripts")) "." else "../..", "scripts/charts/00_setup.R"))

zones <- rast(spatial("cfp_sale_zones.tif"))
labels <- csv("cfp_sale_zone_labels.csv")

# Aggregating 5x5 cells keeps the picture light; use fact = 1 for full detail.
zagg <- aggregate(subst(zones, 0, NA), fact = 5, fun = "modal", na.rm = TRUE)
zdf <- as.data.frame(zagg, xy = TRUE, na.rm = FALSE) |>
  mutate(group = factor(ZONE_LAB[labels$zone[match(zone_code, labels$zone_code)]], levels = names(GROUP_COLS)))
counties <- st_transform(st_read(input("boundaries/houghton_keweenaw_counties.shp"), quiet = TRUE), crs(zones))

p <- ggplot() +
  geom_sf(data = counties, fill = "grey97", color = "grey40", linewidth = 0.4) +
  geom_raster(data = zdf, aes(x, y, fill = group), na.rm = TRUE) +
  geom_sf(data = counties, fill = NA, color = "grey40", linewidth = 0.4) +
  scale_fill_manual(values = GROUP_COLS, drop = FALSE, na.value = NA, na.translate = FALSE) +
  coord_sf(datum = NA, xlim = range(zdf$x[!is.na(zdf$group)]) + c(-3000, 3000), ylim = range(zdf$y[!is.na(zdf$group)]) + c(-3000, 3000)) +
  labs(title = "CFP land by owner, 2020 → 2026", fill = NULL, x = NULL, y = NULL) +
  theme_map() +
  theme(legend.position = "right", legend.key.size = unit(0.6, "cm"))

save_chart(p, "01_map_cfp_owners", width = 10, height = 7, editable = FALSE)
