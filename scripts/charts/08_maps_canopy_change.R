# Maps: canopy change 2020 -> 2025 inside the Songbird and Heartlands footprints.
# Data: output_spatial/tcc_change_2020_2025.tif (scripts/19), or computed from inputs/tcc.
if (!exists("CHART_SETUP_LOADED")) source(file.path(if (dir.exists("scripts")) "." else "../..", "scripts/charts/00_setup.R"))

CHANGE_LIMITS <- c(-60, 40)   # points; values beyond are drawn at the end colours

change_file <- spatial("tcc_change_2020_2025.tif")
tcc_change <- if (file.exists(change_file)) rast(change_file) else {
  read_tcc <- function(y) subst(rast(input(sprintf("tcc/HK_TCC_%d.tif", y))), c(254, 255), NA)
  read_tcc(2025) - read_tcc(2020)
}
fp <- read_footprints(crs(tcc_change))

change_map <- function(name) {
  v <- vect(fp[[name]])
  d <- as.data.frame(mask(crop(tcc_change, v), v), xy = TRUE, na.rm = FALSE)
  names(d)[3] <- "v"
  ggplot() +
    geom_raster(data = d, aes(x, y, fill = v), na.rm = TRUE) +
    geom_sf(data = fp[[name]], fill = NA, color = "grey30", linewidth = 0.4) +
    scale_fill_gradient2(
      low = "#8c510a", mid = "#f6f6f6", high = "#01665e", midpoint = 0,
      limits = CHANGE_LIMITS, breaks = c(-60, -30, 0, 30), oob = squish,
      name = "Change in canopy (points)", na.value = NA
    ) +
    guides(fill = guide_colorbar(barwidth = unit(6, "cm"), title.position = "top")) +
    coord_sf(datum = NA) +
    labs(title = name, subtitle = "Canopy change, 2020 → 2025", x = NULL, y = NULL) +
    theme_map()
}

save_chart(change_map("Songbird"), "08a_map_canopy_change_songbird", W_HALF, H_HALF, editable = FALSE)
save_chart(change_map("Heartlands"), "08b_map_canopy_change_heartlands", W_HALF, H_HALF, editable = FALSE)
