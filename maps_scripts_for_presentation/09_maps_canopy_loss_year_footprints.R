# Disturbance on the two sale areas: the year the canopy dropped, from two satellite
# records, for the Keweenaw Heartlands (2012-2022) and American Songbird (2015-2025).
# One legend box per year. The small detached Heartlands strip is shown as an inset so
# the main blocks can be drawn larger.
#
# Outputs: figures/09a_map_canopy_loss_year_heartlands.png
#          figures/09b_map_canopy_loss_year_songbird.png

if (!exists("SETUP_LOADED")) source(if (file.exists("00_setup.R")) "00_setup.R" else "maps_scripts_for_presentation/00_setup.R")

verdant_txt <- owner_date_text("Verdant")
tnc_txt <- owner_date_text("Nature Conservancy")

how_coloured <- paste0(
  "How a cell is coloured\n\n",
  paste(strwrap(paste0("Left: the first year its canopy fell 15+ points below its highest value in ",
                       "the previous 3 years."), 44), collapse = "\n"), "\n\n",
  paste(strwrap(paste0("Right: the year Hansen detected stand-replacing loss: trees across the whole ",
                       "30 m cell removed or killed. Hansen is yes or no; it has no partial-loss scale."), 44),
        collapse = "\n")
)

footprint_maps <- function(name, years, file, title, hansen_label, owners) {
  r <- rast(spatial(sprintf("drop_year_%s.tif", tolower(name))))
  fp <- read_footprint(name)
  parts <- suppressWarnings(st_cast(st_cast(fp, "MULTIPOLYGON"), "POLYGON"))
  parts$acres <- as.numeric(st_area(parts)) / 4046.8564224
  big <- filter(parts, acres >= 1000)
  big_box <- st_as_sfc(st_bbox(big)) |> st_buffer(500)
  attached <- lengths(st_intersects(parts, big_box)) > 0
  detached <- parts[!attached, ]
  fp_main <- parts[attached, ]

  main_bb <- st_bbox(fp_main)
  w <- main_bb[["xmax"]] - main_bb[["xmin"]]
  h <- main_bb[["ymax"]] - main_bb[["ymin"]]
  xlim <- c(main_bb[["xmin"]] - 0.02 * w, main_bb[["xmax"]] + 0.02 * w)
  ylim <- c(main_bb[["ymin"]] - 0.12 * h, main_bb[["ymax"]] + 0.02 * h)

  pal <- setNames(viridisLite::turbo(length(years), begin = 0.05, end = 0.95), years)
  layer_df <- function(layer, within) {
    x <- as.data.frame(mask(r[[layer]], vect(within)), xy = TRUE, na.rm = TRUE)
    names(x)[3] <- "year"
    x$x <- round(x$x, 1)
    x$y <- round(x$y, 1)
    x$year <- factor(x$year, levels = years)
    filter(x, !is.na(year))
  }
  fill_scale <- scale_fill_manual(values = pal, drop = FALSE, na.translate = FALSE,
                                  name = "Year canopy\ndropped")

  inset <- function(layer) {
    if (nrow(detached) == 0) return(NULL)
    d <- layer_df(layer, detached)
    db <- st_bbox(detached)
    pad <- 300
    mini <- ggplot() +
      geom_sf(data = detached, fill = "grey90", colour = NA) +
      geom_raster(data = d, aes(x, y, fill = year)) +
      geom_sf(data = detached, fill = NA, colour = "grey30", linewidth = 0.3) +
      fill_scale +
      coord_sf(xlim = c(db[["xmin"]] - pad, db[["xmax"]] + pad), ylim = c(db[["ymin"]] - pad, db[["ymax"]] + pad),
               expand = FALSE, datum = NA) +
      labs(title = paste0("Detached strip, ", fmt_n(sum(detached$acres)), " ac\n(just south of the map; enlarged)")) +
      theme_void(base_size = BASE_SIZE - 6, base_family = FONT_FAMILY) +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5, colour = "grey25", lineheight = 0.95),
            panel.border = element_rect(fill = NA, colour = "grey50", linewidth = 0.4),
            plot.background = element_rect(fill = "white", colour = NA))
    annotation_custom(ggplotGrob(mini),
                      xmin = xlim[1] + 0.60 * diff(xlim), xmax = xlim[2] - 0.01 * diff(xlim),
                      ymin = ylim[1] + 0.13 * diff(ylim), ymax = ylim[1] + 0.47 * diff(ylim))
  }

  panel <- function(layer, ptitle, with_scale) {
    d <- layer_df(layer, fp_main)
    p <- ggplot() +
      geom_sf(data = fp_main, fill = "grey90", colour = NA) +
      geom_raster(data = d, aes(x, y, fill = year)) +
      geom_sf(data = fp_main, fill = NA, colour = "grey30", linewidth = 0.3) +
      fill_scale +
      guides(fill = guide_legend(ncol = 2, byrow = FALSE, override.aes = list(colour = "white"))) +
      coord_sf(xlim = xlim, ylim = ylim, expand = FALSE, datum = NA) +
      labs(title = ptitle) +
      theme_map() +
      theme(legend.position = "right", legend.title = element_text(size = BASE_SIZE - 2),
            legend.text = element_text(size = BASE_SIZE - 3),
            legend.key.size = unit(0.26, "in"))
    if (with_scale) p <- p + scale_bar(xlim, ylim, pos = "bottomleft")
    ins <- inset(layer)
    if (!is.null(ins)) p <- p + ins
    p
  }

  left <- panel("tcc_drop_year", "USFS Canopy Loss 15+ Points", TRUE)
  right <- panel("hansen_loss_year", hansen_label, FALSE)
  notes <- paste0(how_coloured, "\n\n", paste(strwrap(owners, 44), collapse = "\n"))

  save_layout(
    file,
    panels = list(left + theme(legend.position = "none"), right + theme(legend.position = "none")),
    side = list(get_legend(left), text_block(notes, size = BASE_SIZE - 6)),
    side_heights = c(0.8, 1.4),
    side_width = 0.26,
    title = title,
    caption = "Sources: USFS FIA Tree Canopy Cover, 2010–2025; Hansen Global Forest Change, loss year through 2024. Owner dates from the 2026 Michigan DNR Commercial Forest list."
  )
}

footprint_maps(
  "Heartlands", 2012:2022, "09a_map_canopy_loss_year_heartlands.png",
  "Heartlands Acres: Canopy Cover Loss, 2012–2022", "Hansen Forest Cleared",
  paste0("Owners: TRG until ", verdant_txt, "; Verdant until ", tnc_txt,
         "; The Nature Conservancy since.")
)

footprint_maps(
  "Songbird", 2015:2025, "09b_map_canopy_loss_year_songbird.png",
  "Songbird Acres: Canopy Cover Loss, 2015–2025", "Hansen Forest Cleared (through 2024)",
  paste0("Owners: TRG until ", verdant_txt, "; Verdant until the 2026 sale to American Songbird.")
)
