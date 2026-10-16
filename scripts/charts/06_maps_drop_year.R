# Maps: year the canopy dropped, Heartlands (2012-2022) and Songbird (2015-2025).
# One figure per property: USFS canopy (fell 15+ points) and Hansen (forest cleared) side by
# side, sharing a legend with one cell per year.
# Data: output_spatial/drop_year_{heartlands,songbird}.tif (scripts/17).
if (!exists("CHART_SETUP_LOADED")) source(file.path(if (dir.exists("scripts")) "." else "../..", "scripts/charts/00_setup.R"))

DROP_YEARS <- list(Heartlands = 2012:2022, Songbird = 2015:2025)
year_colors <- function(n) viridisLite::turbo(n, begin = 0.05, end = 0.95)
fp <- read_footprints()

drop_panels <- function(name, title, hansen_label) {
  yrs <- DROP_YEARS[[name]]
  r <- rast(spatial(sprintf("drop_year_%s.tif", tolower(name))))
  panels <- c(tcc_drop_year = "USFS Canopy Loss 15+ Points", hansen_loss_year = hansen_label)
  d <- bind_rows(lapply(names(panels), function(l) {
    x <- as.data.frame(r[[l]], xy = TRUE, na.rm = FALSE)
    names(x)[3] <- "year"
    mutate(x, panel = panels[[l]])
  })) |>
    mutate(panel = factor(panel, levels = panels), year = factor(year, levels = yrs))
  ggplot() +
    geom_sf(data = fp[[name]], fill = "grey92", color = "grey30", linewidth = 0.3) +
    geom_raster(data = d, aes(x, y, fill = year), na.rm = TRUE) +
    facet_wrap(~panel) +
    scale_fill_manual(values = setNames(year_colors(length(yrs)), yrs), drop = FALSE,
                      na.translate = FALSE, name = "Year canopy\ndropped") +
    guides(fill = guide_legend(ncol = 1, override.aes = list(color = "white"))) +
    coord_sf(datum = NA, expand = FALSE) +
    labs(title = title, x = NULL, y = NULL) +
    theme_map() +
    theme(
      strip.text = element_text(face = "bold", margin = margin(b = 6)),
      panel.spacing = unit(0.8, "cm"),
      legend.position = "right",
      plot.margin = margin(10, 10, 10, 10)
    )
}

save_chart(drop_panels("Heartlands", "Heartlands Acres: Canopy Cover Loss, 2012–2022", "Hansen Forest Cleared"),
           "06a_map_drop_year_heartlands", W_FULL, 6.2, editable = FALSE)
save_chart(drop_panels("Songbird", "Songbird Acres: Canopy Cover Loss, 2015–2025", "Hansen Forest Cleared (through 2024)"),
           "06b_map_drop_year_songbird", W_FULL, 6.2, editable = FALSE)
