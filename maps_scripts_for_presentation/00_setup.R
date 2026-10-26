# Shared settings, data readers and map helpers for the presentation figures.
#
# Every figure script sources this file, so each one runs on its own from the
# kee_trees project root:
#   Rscript maps_scripts_for_presentation/05_map_corporate_by_destination.R
# or run every figure script at once:
#   Rscript maps_scripts_for_presentation/make_all.R
#
# Figures are written to maps_scripts_for_presentation/figures/ as 300 dpi PNGs
# sized for a full 16:9 slide (13.33 x 7.5 in).

# ---- Packages ----
# Keep the projection library offline (no downloads of datum grids), then load the
# packages for tables (dplyr, tidyr), charts (ggplot2, scales), vector maps (sf),
# rasters (terra) and slide layout (grid).
Sys.setenv(PROJ_NETWORK = "OFF")
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(sf)
  library(terra)
  library(grid)
})
terraOptions(progress = 0)

# ============================== USER SETTINGS ===============================

BASE_SIZE <- 16          # base font size (points)
FONT_FAMILY <- "sans"    # e.g. "Arial", "Calibri" if installed
SHOW_TITLES <- TRUE      # FALSE drops in-figure titles (use PowerPoint slide titles instead)
DPI <- 300
FIG_W <- 13.33           # full 16:9 slide
FIG_H <- 7.5
SHOW_SCALE_BAR <- FALSE  # TRUE puts a scale bar back on every map
SCALE_UNITS <- "mi"      # scale bars in "mi" or "km"

# Owner classes. Change the wording here and every figure follows.
CORP <- "Corporate owners"
OTHER <- "Other private owners"
NONCFP <- "Not in the Commercial Forest Program"
CORP_NOTE <- "Corporate owners = TRG/Verdant, Molpus and Lyme, investment companies that own forest land for investors."
OTHER_NOTE <- paste0(
  "Other private owners = everyone else enrolled, including families, clubs and other timber companies ",
  "(Lake Superior Timberlands alone holds 41% of other private land)."
)

# Colours for the owner classes: strong colours for maps and bars, pale versions for
# backgrounds and fills behind other layers.
CLASS_COLS <- setNames(c("#3a4a9f", "#d09a2c", "grey55"), c(CORP, OTHER, NONCFP))
CLASS_PALE <- setNames(c("#cfd4ee", "#f3e3bd"), c(CORP, OTHER))

# Owner groups, 2020 -> 2026 (same wording as the slide deck). Each group follows
# the 2020 Commercial Forest Program (CFP) parcels of one owner to whoever held them
# in 2026. TNC = The Nature Conservancy.
G <- list(
  TV = "TRG → Verdant",
  KH = "TRG → Verdant → The Nature Conservancy (Heartlands)",
  SB = "TRG → Verdant → American Songbird (2026)",
  TO = "TRG → other owners / left CFP",
  MW = "Molpus → kept (mostly)",
  LY = "Lyme → kept",
  NK = "Other private → kept",
  NS = "Other private → sold or left CFP"
)
# One colour per owner group, then the lists of groups that count as corporate or
# other private.
GROUP_COLS <- setNames(
  c("#7570b3", "#1b9e77", "#d95f02", "#56b4e9", "#e7298a", "#f0c419", "#d2b48c", "#5c3a1e"),
  unlist(G)
)
CORP_GROUPS <- c(G$TV, G$KH, G$SB, G$TO, G$MW, G$LY)

# Figure 05 splits "TRG → other owners / left CFP" in two.
TO_KEPT <- "TRG → other owners, still enrolled"
TO_LEFT <- "TRG → left the program"
TO_SPLIT_COLS <- setNames(c("#56b4e9", "#08306b"), c(TO_KEPT, TO_LEFT))
OTHER_GROUPS <- c(G$NK, G$NS)

# Zone names used in the analysis CSVs -> display labels.
ZONE_LAB <- c(
  "TRG/Verdant retained" = G$TV,
  "Heartlands (TRG to TNC)" = G$KH,
  "Songbird (TRG to Songbird, 2026)" = G$SB,
  "TRG other sale / left CFP" = G$TO,
  "MWF" = G$MW,
  "Lyme" = G$LY,
  "Non-institutional kept" = G$NK,
  "Non-institutional sold / left" = G$NS
)
# Zone codes in output_spatial/cfp_sale_zones.tif
CORP_ZONE_CODES <- c(1, 2, 3, 4, 20, 21)
OTHER_ZONE_CODES <- c(5, 6)

# ============================================================================

# ---- Folders and file paths ----
# Find the project root so scripts work from either the root or this folder, then
# make sure the output folders for figures, tables and downloaded data exist.
root <- if (dir.exists("output_csvs")) "." else if (dir.exists("../output_csvs")) ".." else
  stop("Run from the kee_trees project root (or from maps_scripts_for_presentation/).")
HERE <- file.path(root, "maps_scripts_for_presentation")
FIG_DIR <- file.path(HERE, "figures")
TAB_DIR <- file.path(HERE, "tables")
DATA_DIR <- file.path(HERE, "data")
for (d in c(FIG_DIR, TAB_DIR, DATA_DIR)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# Short helpers that build paths to the analysis outputs (CSV tables and spatial
# files) and the raw inputs.
csv <- function(f) read.csv(file.path(root, "output_csvs", f), stringsAsFactors = FALSE, check.names = FALSE)
spatial <- function(f) file.path(root, "output_spatial", f)
input <- function(f) file.path(root, "inputs", f)

# The 2020 Tree Canopy Cover (TCC) raster sets the common map grid: its projection
# (an Albers equal-area projection in metres) is used for every layer, and its pixel
# size gives the acres each pixel represents (1 acre = 4046.86 square metres).
GRID <- rast(input("tcc/HK_TCC_2020.tif"))
GRID_CRS <- st_crs(crs(GRID))
ACRES_PER_PX <- prod(res(GRID)) / 4046.8564224

# Number formatting for labels: whole numbers with commas, or one decimal place.
fmt_n <- function(x) format(round(as.numeric(x)), big.mark = ",", trim = TRUE)
fmt_1 <- function(x) sprintf("%.1f", as.numeric(x))

# Corporate firm from a 2020 owner name (same patterns as scripts/10). Returns NA
# for owners that are not one of the corporate investment firms.
firm_from_name <- function(name) {
  n <- as.character(name)
  case_when(
    grepl("TRG\\s*THRESHOLD|VERDANT", n, ignore.case = TRUE) ~ "TRG",
    grepl("\\bLYME\\b", n, ignore.case = TRUE) ~ "Lyme",
    grepl("MOLPUS|\\bMWF\\b", n, ignore.case = TRUE) ~ "Molpus",
    grepl("MANULIFE|HANCOCK\\s*TIMBER|Hancock Natural", n, ignore.case = TRUE) ~ "Manulife",
    TRUE ~ NA_character_
  )
}

# ---- Data readers ----------------------------------------------------------

# Houghton and Keweenaw county outlines, in the map grid's projection.
read_counties <- function() {
  st_read(input("boundaries/houghton_keweenaw_counties.shp"), quiet = TRUE) |>
    st_transform(GRID_CRS) |>
    st_make_valid() |>
    select(COUNTY)
}

# Counties without Isle Royale and its nearby islands (no Commercial Forest land there).
# Splits the counties into separate islands, keeps pieces whose southern edge is
# below 47.6 degrees north latitude, and rejoins them by county.
read_mainland <- function() {
  parts <- suppressWarnings(st_cast(st_cast(read_counties(), "MULTIPOLYGON"), "POLYGON"))
  north <- vapply(st_geometry(st_transform(parts, 4326)), function(g) st_bbox(g)[["ymin"]], 0)
  parts[north < 47.6, ] |>
    group_by(COUNTY) |>
    summarise(.groups = "drop")
}

# 2020 CFP parcels with owner group and owner class. Parcels under 20 acres have no
# group in the analysis, so their class comes from the 2020 owner name.
# The owner names come from the 2020 state CFP list and are joined by parcel ID.
read_parcels <- function() {
  p <- st_read(spatial("cfp_sale_story.gpkg"), "cfp_parcels_2020", quiet = TRUE)
  names20 <- st_read(input("cfp/cfp_hk_2020.shp"), quiet = TRUE) |>
    st_drop_geometry() |>
    transmute(parid = as.character(parid), owner_2020 = trimws(as.character(search_nam))) |>
    distinct(parid, .keep_all = TRUE)
  p |>
    mutate(parid = as.character(parid)) |>
    left_join(names20, by = "parid") |>
    mutate(class = case_when(
      group %in% CORP_GROUPS ~ CORP,
      group %in% OTHER_GROUPS ~ OTHER,
      !is.na(firm_from_name(owner_2020)) ~ CORP,
      TRUE ~ OTHER
    ))
}

# Townships (US Census county subdivisions) in the two counties. Downloaded once (needs
# internet) and saved to data/townships.gpkg.
read_townships <- function() {
  f <- file.path(DATA_DIR, "townships.gpkg")
  if (!file.exists(f)) {
    options(tigris_use_cache = TRUE)
    tigris::county_subdivisions("MI", county = c("Houghton", "Keweenaw"), cb = TRUE, year = 2024,
                                progress_bar = FALSE) |>
      st_transform(GRID_CRS) |>
      group_by(township = NAME, county = sub(" County", "", NAMELSADCO)) |>
      summarise(.groups = "drop") |>
      st_write(f, quiet = TRUE)
  }
  st_read(f, quiet = TRUE)
}

# Outline of a conservation purchase ("Heartlands" or "Songbird") merged into one
# shape, with any height values dropped and geometry errors repaired.
read_footprint <- function(name) {
  f <- switch(name,
    Heartlands = "boundaries/keweenaw_heartlands.shp",
    Songbird = "boundaries/american_songbird.shp"
  )
  st_read(input(f), quiet = TRUE) |>
    st_zm() |>
    st_transform(GRID_CRS) |>
    st_make_valid() |>
    st_union() |>
    st_as_sf()
}

# Owner-class acres per analysis zone (pixel-based, from the by-year CSVs).
zone_acres <- function() {
  csv("cfp_sale_footprint_hansen_by_year.csv") |>
    distinct(zone, zone_acres) |>
    mutate(group = ZONE_LAB[zone], class = if_else(group %in% CORP_GROUPS, CORP, OTHER))
}

# Dates each owner took over its parcels, read from the 2026 state list
# (field OwnerParce, the date the 2026 owner acquired the parcel), as decimal years.
# For each owner the median acquisition date is used, ignoring missing and
# placeholder dates (before 1900). Decimal years let the dates sit on a year axis.
owner_start_dates <- function() {
  p26 <- st_read(input("cfp/cfp_hk_2026.shp"), quiet = TRUE) |> st_drop_geometry()
  d <- as.Date(p26$OwnerParce)
  dec <- function(pattern) {
    x <- d[grepl(pattern, p26$FullLegalN, ignore.case = TRUE) & !is.na(d) & d > as.Date("1900-01-01")]
    x <- sort(x)[ceiling(length(x) / 2)]
    as.numeric(format(x, "%Y")) + (as.numeric(format(x, "%j")) - 1) / 365
  }
  list(
    verdant = dec("Verdant"),
    tnc = dec("Nature Conservancy"),
    lyme = dec("\\bLyme\\b"),
    molpus = dec("Molpus|\\bMWF\\b")
  )
}

# Turns a decimal year (e.g. 2022.45) back into a short label such as "Jun 2022".
month_year <- function(x) format(as.Date(sprintf("%d-01-01", floor(x))) + round((x %% 1) * 365), "%b %Y")

# Readable range of the dates an owner took over most of its parcels (middle 80%),
# e.g. "Jun–Aug 2022" or "Nov 2022–Jan 2023".
owner_date_text <- function(pattern) {
  p26 <- st_read(input("cfp/cfp_hk_2026.shp"), quiet = TRUE) |> st_drop_geometry()
  d <- as.Date(p26$OwnerParce)
  d <- sort(d[grepl(pattern, p26$FullLegalN, ignore.case = TRUE) & !is.na(d) & d > as.Date("1900-01-01")])
  q <- d[c(max(1, ceiling(0.1 * length(d))), ceiling(0.9 * length(d)))]
  # Shorten the label when both ends fall in the same month or the same year.
  m <- format(q, "%b")
  y <- format(q, "%Y")
  if (identical(m[1], m[2]) && identical(y[1], y[2])) return(paste(m[1], y[1]))
  if (identical(y[1], y[2])) return(paste0(m[1], "–", m[2], " ", y[1]))
  paste0(m[1], " ", y[1], "–", m[2], " ", y[2])
}

# ---- Themes ----------------------------------------------------------------

# Common look for all charts: minimal grid, bold left-aligned titles, legend on top,
# small grey caption. Titles are dropped when SHOW_TITLES is FALSE.
theme_chart <- function(base_size = BASE_SIZE) {
  t <- theme_minimal(base_size = base_size, base_family = FONT_FAMILY) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(face = "bold", size = base_size + 6),
      plot.subtitle = element_text(color = "grey30", margin = margin(b = 10)),
      plot.caption = element_text(color = "grey40", hjust = 0, size = base_size - 5, lineheight = 1.1),
      legend.position = "top",
      legend.justification = "left",
      strip.text = element_text(face = "bold", hjust = 0),
      plot.margin = margin(15, 20, 10, 15)
    )
  if (!SHOW_TITLES) t <- t + theme(plot.title = element_blank(), plot.subtitle = element_blank())
  t
}

# Common look for map panels: no axes or grid, a small centred panel title, and no
# legend (legends are drawn separately in the slide's side column).
theme_map <- function(base_size = BASE_SIZE) {
  theme_void(base_size = base_size, base_family = FONT_FAMILY) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = base_size, margin = margin(b = 4)),
      legend.position = "none",
      plot.margin = margin(4, 4, 4, 4)
    )
}

# ---- Map furniture ---------------------------------------------------------

# Scale bar drawn with plain ggplot layers (map units are metres).
# xlim and ylim are the map's extent; the bar is placed relative to them so it sits
# in the same corner whatever the map size. Returns nothing if scale bars are off.
scale_bar <- function(xlim, ylim, length = NULL, units = SCALE_UNITS, pos = "bottomleft",
                      text_size = BASE_SIZE / 3.6) {
  if (!SHOW_SCALE_BAR) return(NULL)
  per <- if (units == "mi") 1609.344 else 1000
  w <- diff(xlim)
  h <- diff(ylim)
  # If no length is given, pick the largest round distance that fits in a quarter of
  # the map width.
  if (is.null(length)) {
    nice <- c(0.5, 1, 2, 5, 10, 20, 25, 50, 100, 200, 500)
    length <- max(nice[nice * per <= w / 4])
  }
  # Bar length in metres, bar height and start point, all as fractions of the map size.
  len <- length * per
  bar_h <- h * 0.013
  x0 <- if (pos == "bottomleft") xlim[1] + w * 0.04 else xlim[2] - w * 0.04 - len
  y0 <- ylim[1] + h * 0.035
  lab <- function(v) format(v, trim = TRUE, drop0trailing = TRUE)
  # A two-part bar (dark half, white half) with "0" and the distance written above.
  list(
    annotate("rect", xmin = x0, xmax = x0 + len / 2, ymin = y0, ymax = y0 + bar_h,
             fill = "grey15", colour = "grey15", linewidth = 0.3),
    annotate("rect", xmin = x0 + len / 2, xmax = x0 + len, ymin = y0, ymax = y0 + bar_h,
             fill = "white", colour = "grey15", linewidth = 0.3),
    annotate("text", x = c(x0, x0 + len), y = y0 + bar_h * 1.9,
             label = c("0", paste(lab(length), units)), hjust = c(0, 1),
             size = text_size, vjust = 0, family = FONT_FAMILY, colour = "grey15")
  )
}

# North arrow pointing to true north (the Albers grid is rotated a few degrees here).
north_arrow <- function(xlim, ylim, pos = "topleft", text_size = BASE_SIZE / 3.2) {
  w <- diff(xlim)
  h <- diff(ylim)
  x0 <- if (pos == "topleft") xlim[1] + w * 0.05 else xlim[2] - w * 0.05
  y0 <- ylim[2] - h * 0.14
  # Find true north: convert the arrow's base to latitude/longitude, step a little
  # north, convert back to map units, and measure the angle between the two points.
  pt <- st_sfc(st_point(c(x0, y0)), crs = GRID_CRS) |> st_transform(4326)
  ll <- st_coordinates(pt)
  up <- st_sfc(st_point(c(ll[1], ll[2] + 0.05)), crs = 4326) |> st_transform(GRID_CRS) |> st_coordinates()
  ang <- atan2(up[1] - x0, up[2] - y0)
  len <- h * 0.07
  x1 <- x0 + len * sin(ang)
  y1 <- y0 + len * cos(ang)
  # Draw the arrow, with an "N" just beyond its tip.
  list(
    annotate("segment", x = x0, y = y0, xend = x1, yend = y1, colour = "grey15", linewidth = 0.8,
             arrow = arrow(length = unit(0.12, "in"), type = "closed")),
    annotate("text", x = x1 + (x1 - x0) * 0.35, y = y1 + (y1 - y0) * 0.35, label = "N",
             fontface = "bold", size = text_size, family = FONT_FAMILY, colour = "grey15")
  )
}

# Widens a bounding box by a small fraction on every side and returns x and y limits,
# so map features don't touch the edge of the panel.
pad_bbox <- function(b, frac = 0.03) {
  dx <- (b[["xmax"]] - b[["xmin"]]) * frac
  dy <- (b[["ymax"]] - b[["ymin"]]) * frac
  list(x = c(b[["xmin"]] - dx, b[["xmax"]] + dx), y = c(b[["ymin"]] - dy, b[["ymax"]] + dy))
}

# ---- Locator maps ----------------------------------------------------------

# Background layers for the small "where is this?" maps are cached in this file.
LOCATOR_FILE <- file.path(DATA_DIR, "locator_layers.gpkg")

# One-time build of the locator layers: US states and Michigan counties (US Census,
# via tigris) plus Great Lakes and Canada (Natural Earth). Needs internet once.
build_locator_layers <- function() {
  message("Building locator layers (one-time download)...")
  old <- sf_use_s2(FALSE)
  on.exit(sf_use_s2(old))
  options(tigris_use_cache = TRUE)
  # Everything is cropped to a box around the Upper Great Lakes (longitude/latitude).
  box <- st_bbox(c(xmin = -93.6, ymin = 43.8, xmax = -82.4, ymax = 49.6), crs = 4326)
  crop_to <- function(x) suppressWarnings(st_crop(st_make_valid(st_transform(x, 4326)), box))
  states <- tigris::states(cb = TRUE, year = 2024, progress_bar = FALSE)
  region_states <- crop_to(states) |> select(STUSPS, NAME)
  mi_counties <- tigris::counties(state = "MI", cb = TRUE, year = 2024, progress_bar = FALSE) |>
    crop_to() |>
    select(NAME)
  # Lakes and the Canadian border come from Natural Earth, downloaded to a temporary folder.
  tmp <- tempfile()
  dir.create(tmp)
  lakes <- rnaturalearth::ne_download(10, "lakes", "physical", returnclass = "sf", destdir = tmp) |>
    crop_to() |>
    select(name)
  canada <- rnaturalearth::ne_download(10, "countries", "cultural", returnclass = "sf", destdir = tmp) |>
    filter(ADMIN == "Canada") |>
    crop_to() |>
    select(ADMIN)
  # Save each layer, in the map grid's projection, to the cache file.
  for (nm in c("region_states", "mi_counties", "lakes", "canada")) {
    st_write(st_transform(get(nm), GRID_CRS), LOCATOR_FILE, nm, quiet = TRUE, delete_layer = TRUE)
  }
}

# Reads the cached locator layers (building them first if needed) as a named list.
read_locator <- function() {
  if (!file.exists(LOCATOR_FILE)) build_locator_layers()
  lyr <- c("region_states", "mi_counties", "lakes", "canada")
  setNames(lapply(lyr, function(l) st_read(LOCATOR_FILE, l, quiet = TRUE)), lyr)
}

# Locator map colours: pale blue water, light grey land, red for the study area.
WATER <- "#d4e4f1"
LAND <- "grey95"
HIGHLIGHT <- "#c0392b"

# ---- Layout and saving -----------------------------------------------------

# Pulls just the legend out of a ggplot so it can be placed in the slide's side
# column. Returns an empty placeholder if the plot has no legend.
get_legend <- function(p) {
  g <- ggplotGrob(p)
  i <- grep("guide-box", vapply(g$grobs, function(x) x$name, ""))
  i <- i[!vapply(g$grobs[i], inherits, logical(1), "zeroGrob")]
  if (length(i)) g$grobs[[i[1]]] else nullGrob()
}

# A block of explanatory text for the side column, anchored to its top-left corner.
# Line breaks must be written into the text ("\n"); it is not wrapped automatically.
text_block <- function(txt, size = BASE_SIZE - 3) {
  textGrob(txt, x = unit(0.1, "npc"), y = unit(0.95, "npc"), just = c("left", "top"),
           gp = gpar(fontsize = size, col = "grey20", fontfamily = FONT_FAMILY, lineheight = 1.15))
}

# Running list of every figure file written in this R session. It is kept (not reset)
# if this file is sourced again.
FIGURES <- if (exists("FIGURES")) FIGURES else character()

# Compose one slide-sized PNG: title and subtitle, map panels side by side, an optional
# right-hand column (legend, notes) and a caption.
# title_pos = "top" puts the title across the top of the slide; "side" puts it,
# right-aligned, at the top of the right-hand column so the maps get the full height.
save_layout <- function(file, panels, side = list(), side_heights = NULL, title = NULL,
                        subtitle = NULL, caption = NULL, side_width = 0.27,
                        width = FIG_W, height = FIG_H, title_pos = "top") {
  # Open a high-resolution PNG file to draw into; it is closed when the function ends.
  path <- file.path(FIG_DIR, file)
  ragg::agg_png(path, width = width, height = height, units = "in", res = DPI, background = "white")
  on.exit(dev.off(), add = TRUE)
  grid.newpage()
  # Only draw a title or subtitle if one was given and titles are switched on.
  show_t <- SHOW_TITLES && !is.null(title)
  show_s <- SHOW_TITLES && !is.null(subtitle)
  if (title_pos == "side" && length(side) > 0 && (show_t || show_s)) {
    # Side title: wrap the title and subtitle to fit the side column's width.
    side_in <- width * side_width
    # Wrap into as few lines as fit the column (measured in the actual font), then even
    # out the line lengths so no word sits alone on the last line.
    avail_in <- side_in - 0.45
    # widest() measures the longest line in inches. balanced() tries every wrap width
    # from widest to narrowest, keeps those that fit, and among the ones with the
    # fewest lines picks the narrowest, which gives lines of similar length.
    widest <- function(lines, gp) {
      max(vapply(lines, function(l) convertWidth(grobWidth(textGrob(l, gp = gp)), "in", TRUE), 0))
    }
    balanced <- function(x, gp) {
      ws <- seq(nchar(x) + 1, 8)
      fit <- vapply(ws, function(w) widest(strwrap(x, w), gp) <= avail_in, TRUE)
      n_lines <- vapply(ws, function(w) length(strwrap(x, w)), 0L)
      n_min <- min(n_lines[fit], n_lines[length(ws)])
      w_best <- min(ws[fit & n_lines == n_min], nchar(x) + 1)
      paste(strwrap(x, w_best), collapse = "\n")
    }
    gp_t <- gpar(fontsize = BASE_SIZE + 8, fontface = "bold", fontfamily = FONT_FAMILY)
    gp_s <- gpar(fontsize = BASE_SIZE, fontfamily = FONT_FAMILY)
    t_txt <- if (show_t) balanced(title, gp_t) else ""
    s_txt <- if (show_s) balanced(subtitle, gp_s) else ""
    n_t <- if (show_t) length(strsplit(t_txt, "\n")[[1]]) else 0
    n_s <- if (show_s) length(strsplit(s_txt, "\n")[[1]]) else 0
    # Height of the header in inches, from the number of title and subtitle lines.
    head_h <- 0.25 + 0.44 * n_t + 0.3 * n_s
    # The header: title and subtitle right-aligned, subtitle placed below the title lines.
    header <- gTree(children = gList(
      if (show_t) textGrob(t_txt, x = unit(1, "npc") - unit(0.3, "in"), y = unit(1, "npc") - unit(0.25, "in"),
                           just = c("right", "top"),
                           gp = gpar(fontsize = BASE_SIZE + 8, fontface = "bold", fontfamily = FONT_FAMILY,
                                     lineheight = 1.05)),
      if (show_s) textGrob(s_txt, x = unit(1, "npc") - unit(0.3, "in"),
                           y = unit(1, "npc") - unit(0.3 + 0.44 * n_t, "in"), just = c("right", "top"),
                           gp = gpar(fontsize = BASE_SIZE, col = "grey30", fontfamily = FONT_FAMILY,
                                     lineheight = 1.1))
    ))
    # Put the header first in the side column with a fixed height; the other side
    # items share the remaining height. The top title row is then switched off.
    hts <- if (is.null(side_heights)) rep(1, length(side)) else side_heights
    side <- c(list(header), side)
    side_heights <- unit(c(head_h, hts), c("in", rep("null", length(hts))))
    show_t <- show_s <- FALSE
  }
  # Split the slide into three rows: a header strip (sized for any title and
  # subtitle), the main area, and a caption strip (sized by its number of lines).
  head_in <- 0.15 + if (show_t) 0.5 else 0
  head_in <- head_in + if (show_s) 0.36 else 0
  cap_n <- if (is.null(caption)) 0 else length(strsplit(caption, "\n")[[1]])
  cap_in <- 0.12 + 0.23 * cap_n
  pushViewport(viewport(layout = grid.layout(3, 1, heights = unit(c(head_in, 1, cap_in), c("in", "null", "in")))))
  # Top title and subtitle, left-aligned in the header strip.
  if (show_t) {
    grid.text(title, x = unit(0.3, "in"), y = unit(1, "npc") - unit(0.38, "in"), just = c("left", "center"),
              gp = gpar(fontsize = BASE_SIZE + 8, fontface = "bold", fontfamily = FONT_FAMILY),
              vp = viewport(layout.pos.row = 1))
  }
  if (show_s) {
    grid.text(subtitle, x = unit(0.3, "in"), y = unit(1, "npc") - unit(if (show_t) 0.78 else 0.3, "in"),
              just = c("left", "center"),
              gp = gpar(fontsize = BASE_SIZE, col = "grey30", fontfamily = FONT_FAMILY),
              vp = viewport(layout.pos.row = 1))
  }
  # Main area: the panels share the width equally, leaving side_width (a fraction of
  # the slide) for the side column if there is one. Each panel is drawn in its own cell.
  pushViewport(viewport(layout.pos.row = 2))
  n <- length(panels)
  has_side <- length(side) > 0
  widths <- if (has_side) c(rep((1 - side_width) / n, n), side_width) else rep(1 / n, n)
  pushViewport(viewport(layout = grid.layout(1, length(widths), widths = unit(widths, "null"))))
  for (i in seq_len(n)) {
    pushViewport(viewport(layout.pos.col = i))
    grid.draw(ggplotGrob(panels[[i]]))
    popViewport()
  }
  # Side column: stack the side items (header, legends, text, small plots) from top to
  # bottom, with heights set by side_heights (equal shares if not given).
  if (has_side) {
    pushViewport(viewport(layout.pos.col = n + 1))
    hts <- if (is.null(side_heights)) rep(1, length(side)) else side_heights
    if (!is.unit(hts)) hts <- unit(hts, "null")
    pushViewport(viewport(layout = grid.layout(length(side), 1, heights = hts)))
    for (j in seq_along(side)) {
      pushViewport(viewport(layout.pos.row = j))
      g <- side[[j]]
      if (inherits(g, "ggplot")) g <- ggplotGrob(g)
      grid.draw(g)
      popViewport()
    }
    popViewport(2)
  }
  popViewport(2)
  # Caption in the bottom strip, then record the file in the list of figures.
  if (cap_n > 0) {
    grid.text(caption, x = unit(0.3, "in"), y = unit(1, "npc") - unit(0.06, "in"), just = c("left", "top"),
              gp = gpar(fontsize = BASE_SIZE - 5, col = "grey40", fontfamily = FONT_FAMILY, lineheight = 1.1),
              vp = viewport(layout.pos.row = 3))
  }
  FIGURES <<- union(FIGURES, path)
  message("Wrote ", path)
  invisible(path)
}

# Saves a single ggplot chart as a slide-sized PNG (no grid layout needed) and records
# it in the list of figures.
save_chart <- function(p, file, width = FIG_W, height = FIG_H) {
  path <- file.path(FIG_DIR, file)
  ggsave(path, p, width = width, height = height, dpi = DPI, device = ragg::agg_png, bg = "white")
  FIGURES <<- union(FIGURES, path)
  message("Wrote ", path)
  invisible(path)
}

# The figure scripts check for this flag, so this file is only sourced once per session.
SETUP_LOADED <- TRUE
