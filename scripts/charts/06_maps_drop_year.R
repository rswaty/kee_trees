# Maps: year the canopy dropped, Heartlands (2012-2022) and Songbird (2015-2025),
# from USFS canopy (fell 15+ points) and Hansen (forest cleared). Four maps.
# Data: output_spatial/drop_year_{heartlands,songbird}.tif (scripts/17).
if (!exists("CHART_SETUP_LOADED")) source(file.path(if (dir.exists("scripts")) "." else "../..", "scripts/charts/00_setup.R"))

DROP_YEARS <- list(Heartlands = 2012:2022, Songbird = 2015:2025)
fp <- read_footprints()

drop_map <- function(name, layer, subtitle) {
  yrs <- DROP_YEARS[[name]]
  d <- as.data.frame(rast(spatial(sprintf("drop_year_%s.tif", tolower(name))))[[layer]], xy = TRUE, na.rm = FALSE)
  names(d)[3] <- "year"
  ggplot() +
    geom_sf(data = fp[[name]], fill = "grey92", color = "grey30", linewidth = 0.3) +
    geom_raster(data = d, aes(x, y, fill = year), na.rm = TRUE) +
    scale_fill_viridis_c(
      option = "turbo", begin = 0.05, end = 0.95,
      limits = range(yrs), breaks = seq(min(yrs), max(yrs), 2),
      name = "Year canopy dropped", na.value = NA
    ) +
    guides(fill = guide_colorbar(barwidth = unit(8, "cm"), title.position = "top")) +
    coord_sf(datum = NA) +
    labs(title = name, subtitle = subtitle, x = NULL, y = NULL) +
    theme_map()
}

save_chart(drop_map("Heartlands", "tcc_drop_year", "USFS canopy: fell 15+ points"),
           "06a_map_drop_year_heartlands_tcc", W_HALF, H_HALF, editable = FALSE)
save_chart(drop_map("Heartlands", "hansen_loss_year", "Hansen: forest cleared"),
           "06b_map_drop_year_heartlands_hansen", W_HALF, H_HALF, editable = FALSE)
save_chart(drop_map("Songbird", "tcc_drop_year", "USFS canopy: fell 15+ points"),
           "06c_map_drop_year_songbird_tcc", W_HALF, H_HALF, editable = FALSE)
save_chart(drop_map("Songbird", "hansen_loss_year", "Hansen: forest cleared (through 2024)"),
           "06d_map_drop_year_songbird_hansen", W_HALF, H_HALF, editable = FALSE)
