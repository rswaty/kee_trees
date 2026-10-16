# Shared settings, data readers and map helpers for the presentation figures.
#
# Every figure script sources this file, so each one runs on its own from the
# kee_trees project root:
#   Rscript maps_scripts_for_presentation/05_map_corporate_by_destination.R
# or run everything and build one PowerPoint deck:
#   Rscript maps_scripts_for_presentation/make_all.R
#
# Figures are written to maps_scripts_for_presentation/figures/ as 300 dpi PNGs
# sized for a full 16:9 slide (13.33 x 7.5 in).

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
SCALE_UNITS <- "mi"      # scale bars in "mi" or "km"

# Owner classes. Change the wording here and every figure follows.
CORP <- "Corporate owners"
OTHER <- "Other private owners"
NONCFP <- "Not in the Commercial Forest Program"
CORP_NOTE <- "Corporate owners = TRG/Verdant, Molpus and Lyme, investment companies that own forest land for investors."
OTHER_NOTE <- paste0(
  "Other private owners = everyone else enrolled, including families, clubs and other timber companies ",
  "(Lake Superior Timberlands holds 69% of the acres other private owners kept)."
)

CLASS_COLS <- setNames(c("#3a4a9f", "#d09a2c", "grey55"), c(CORP, OTHER, NONCFP))
CLASS_PALE <- setNames(c("#cfd4ee", "#f3e3bd"), c(CORP, OTHER))

# Owner groups, 2020 -> 2026 (same wording as the slide deck).
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
GROUP_COLS <- setNames(
  c("#7570b3", "#1b9e77", "#d95f02", "#56b4e9", "#e7298a", "#f0c419", "#d2b48c", "#5c3a1e"),
  unlist(G)
)
CORP_GROUPS <- c(G$TV, G$KH, G$SB, G$TO, G$MW, G$LY)
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

root <- if (dir.exists("output_csvs")) "." else if (dir.exists("../output_csvs")) ".." else
  stop("Run from the kee_trees project root (or from maps_scripts_for_presentation/).")
HERE <- file.path(root, "maps_scripts_for_presentation")
FIG_DIR <- file.path(HERE, "figures")
TAB_DIR <- file.path(HERE, "tables")
DATA_DIR <- file.path(HERE, "data")
for (d in c(FIG_DIR, TAB_DIR, DATA_DIR)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

csv <- function(f) read.csv(file.path(root, "output_csvs", f), stringsAsFactors = FALSE, check.names = FALSE)
spatial <- function(f) file.path(root, "output_spatial", f)
input <- function(f) file.path(root, "inputs", f)

GRID <- rast(input("tcc/HK_TCC_2020.tif"))
GRID_CRS <- st_crs(crs(GRID))
ACRES_PER_PX <- prod(res(GRID)) / 4046.8564224

fmt_n <- function(x) format(round(as.numeric(x)), big.mark = ",", trim = TRUE)
fmt_1 <- function(x) sprintf("%.1f", as.numeric(x))

# Corporate firm from a 2020 owner name (same patterns as scripts/10).
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

read_counties <- function() {
  st_read(input("boundaries/houghton_keweenaw_counties.shp"), quiet = TRUE) |>
    st_transform(GRID_CRS) |>
    st_make_valid() |>
    select(COUNTY)
}

# Counties without Isle Royale and its nearby islands (no Commercial Forest land there).
read_mainland <- function() {
  parts <- suppressWarnings(st_cast(st_cast(read_counties(), "MULTIPOLYGON"), "POLYGON"))
  north <- vapply(st_geometry(st_transform(parts, 4326)), function(g) st_bbox(g)[["ymin"]], 0)
  parts[north < 47.6, ] |>
    group_by(COUNTY) |>
    summarise(.groups = "drop")
}

# 2020 CFP parcels with owner group and owner class. Parcels under 20 acres have no
# group in the analysis, so their class comes from the 2020 owner name.
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

month_year <- function(x) format(as.Date(sprintf("%d-01-01", floor(x))) + round((x %% 1) * 365), "%b %Y")

# Readable range of the dates an owner took over most of its parcels (middle 80%),
# e.g. "Jun–Aug 2022" or "Nov 2022–Jan 2023".
owner_date_text <- function(pattern) {
  p26 <- st_read(input("cfp/cfp_hk_2026.shp"), quiet = TRUE) |> st_drop_geometry()
  d <- as.Date(p26$OwnerParce)
  d <- sort(d[grepl(pattern, p26$FullLegalN, ignore.case = TRUE) & !is.na(d) & d > as.Date("1900-01-01")])
  q <- d[c(max(1, ceiling(0.1 * length(d))), ceiling(0.9 * length(d)))]
  m <- format(q, "%b")
  y <- format(q, "%Y")
  if (identical(m[1], m[2]) && identical(y[1], y[2])) return(paste(m[1], y[1]))
  if (identical(y[1], y[2])) return(paste0(m[1], "–", m[2], " ", y[1]))
  paste0(m[1], " ", y[1], "–", m[2], " ", y[2])
}

# ---- Themes ----------------------------------------------------------------

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
scale_bar <- function(xlim, ylim, length = NULL, units = SCALE_UNITS, pos = "bottomleft",
                      text_size = BASE_SIZE / 3.6) {
  per <- if (units == "mi") 1609.344 else 1000
  w <- diff(xlim)
  h <- diff(ylim)
  if (is.null(length)) {
    nice <- c(0.5, 1, 2, 5, 10, 20, 25, 50, 100, 200, 500)
    length <- max(nice[nice * per <= w / 4])
  }
  len <- length * per
  bar_h <- h * 0.013
  x0 <- if (pos == "bottomleft") xlim[1] + w * 0.04 else xlim[2] - w * 0.04 - len
  y0 <- ylim[1] + h * 0.035
  lab <- function(v) format(v, trim = TRUE, drop0trailing = TRUE)
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
  pt <- st_sfc(st_point(c(x0, y0)), crs = GRID_CRS) |> st_transform(4326)
  ll <- st_coordinates(pt)
  up <- st_sfc(st_point(c(ll[1], ll[2] + 0.05)), crs = 4326) |> st_transform(GRID_CRS) |> st_coordinates()
  ang <- atan2(up[1] - x0, up[2] - y0)
  len <- h * 0.07
  x1 <- x0 + len * sin(ang)
  y1 <- y0 + len * cos(ang)
  list(
    annotate("segment", x = x0, y = y0, xend = x1, yend = y1, colour = "grey15", linewidth = 0.8,
             arrow = arrow(length = unit(0.12, "in"), type = "closed")),
    annotate("text", x = x1 + (x1 - x0) * 0.35, y = y1 + (y1 - y0) * 0.35, label = "N",
             fontface = "bold", size = text_size, family = FONT_FAMILY, colour = "grey15")
  )
}

pad_bbox <- function(b, frac = 0.03) {
  dx <- (b[["xmax"]] - b[["xmin"]]) * frac
  dy <- (b[["ymax"]] - b[["ymin"]]) * frac
  list(x = c(b[["xmin"]] - dx, b[["xmax"]] + dx), y = c(b[["ymin"]] - dy, b[["ymax"]] + dy))
}

# ---- Locator maps ----------------------------------------------------------

LOCATOR_FILE <- file.path(DATA_DIR, "locator_layers.gpkg")

# One-time build of the locator layers: US states and Michigan counties (US Census,
# via tigris) plus Great Lakes and Canada (Natural Earth). Needs internet once.
build_locator_layers <- function() {
  message("Building locator layers (one-time download)...")
  old <- sf_use_s2(FALSE)
  on.exit(sf_use_s2(old))
  options(tigris_use_cache = TRUE)
  box <- st_bbox(c(xmin = -93.6, ymin = 43.8, xmax = -82.4, ymax = 49.6), crs = 4326)
  crop_to <- function(x) suppressWarnings(st_crop(st_make_valid(st_transform(x, 4326)), box))
  states <- tigris::states(cb = TRUE, year = 2024, progress_bar = FALSE)
  region_states <- crop_to(states) |> select(STUSPS, NAME)
  mi_counties <- tigris::counties(state = "MI", cb = TRUE, year = 2024, progress_bar = FALSE) |>
    crop_to() |>
    select(NAME)
  tmp <- tempfile()
  dir.create(tmp)
  lakes <- rnaturalearth::ne_download(10, "lakes", "physical", returnclass = "sf", destdir = tmp) |>
    crop_to() |>
    select(name)
  canada <- rnaturalearth::ne_download(10, "countries", "cultural", returnclass = "sf", destdir = tmp) |>
    filter(ADMIN == "Canada") |>
    crop_to() |>
    select(ADMIN)
  for (nm in c("region_states", "mi_counties", "lakes", "canada")) {
    st_write(st_transform(get(nm), GRID_CRS), LOCATOR_FILE, nm, quiet = TRUE, delete_layer = TRUE)
  }
}

read_locator <- function() {
  if (!file.exists(LOCATOR_FILE)) build_locator_layers()
  lyr <- c("region_states", "mi_counties", "lakes", "canada")
  setNames(lapply(lyr, function(l) st_read(LOCATOR_FILE, l, quiet = TRUE)), lyr)
}

WATER <- "#d4e4f1"
LAND <- "grey95"
HIGHLIGHT <- "#c0392b"

# ---- Layout and saving -----------------------------------------------------

get_legend <- function(p) {
  g <- ggplotGrob(p)
  i <- grep("guide-box", vapply(g$grobs, function(x) x$name, ""))
  i <- i[!vapply(g$grobs[i], inherits, logical(1), "zeroGrob")]
  if (length(i)) g$grobs[[i[1]]] else nullGrob()
}

text_block <- function(txt, size = BASE_SIZE - 3) {
  textGrob(txt, x = unit(0.1, "npc"), y = unit(0.95, "npc"), just = c("left", "top"),
           gp = gpar(fontsize = size, col = "grey20", fontfamily = FONT_FAMILY, lineheight = 1.15))
}

FIGURES <- if (exists("FIGURES")) FIGURES else character()

# Compose one slide-sized PNG: title and subtitle across the top, map panels side by
# side, an optional right-hand column (legend, notes) and a caption.
save_layout <- function(file, panels, side = list(), side_heights = NULL, title = NULL,
                        subtitle = NULL, caption = NULL, side_width = 0.27,
                        width = FIG_W, height = FIG_H) {
  path <- file.path(FIG_DIR, file)
  ragg::agg_png(path, width = width, height = height, units = "in", res = DPI, background = "white")
  on.exit(dev.off(), add = TRUE)
  grid.newpage()
  show_t <- SHOW_TITLES && !is.null(title)
  show_s <- SHOW_TITLES && !is.null(subtitle)
  head_in <- 0.15 + if (show_t) 0.5 else 0
  head_in <- head_in + if (show_s) 0.36 else 0
  cap_n <- if (is.null(caption)) 0 else length(strsplit(caption, "\n")[[1]])
  cap_in <- 0.12 + 0.23 * cap_n
  pushViewport(viewport(layout = grid.layout(3, 1, heights = unit(c(head_in, 1, cap_in), c("in", "null", "in")))))
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
  if (has_side) {
    pushViewport(viewport(layout.pos.col = n + 1))
    hts <- if (is.null(side_heights)) rep(1, length(side)) else side_heights
    pushViewport(viewport(layout = grid.layout(length(side), 1, heights = unit(hts, "null"))))
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
  if (cap_n > 0) {
    grid.text(caption, x = unit(0.3, "in"), y = unit(1, "npc") - unit(0.06, "in"), just = c("left", "top"),
              gp = gpar(fontsize = BASE_SIZE - 5, col = "grey40", fontfamily = FONT_FAMILY, lineheight = 1.1),
              vp = viewport(layout.pos.row = 3))
  }
  FIGURES <<- union(FIGURES, path)
  message("Wrote ", path)
  invisible(path)
}

save_chart <- function(p, file, width = FIG_W, height = FIG_H) {
  path <- file.path(FIG_DIR, file)
  ggsave(path, p, width = width, height = height, dpi = DPI, device = ragg::agg_png, bg = "white")
  FIGURES <<- union(FIGURES, path)
  message("Wrote ", path)
  invisible(path)
}

SETUP_LOADED <- TRUE
