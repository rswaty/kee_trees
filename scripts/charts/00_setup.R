# Shared settings, data and helpers for the PowerPoint chart scripts.
#
# Each chart script sources this file, so you can run any one of them on its own:
#   Rscript scripts/charts/03_canopy_by_year.R
# or run everything (and build one combined deck):
#   Rscript scripts/charts/make_all_charts.R
#
# Outputs go to output_visuals/powerpoint/:
#   <chart>.png   high-resolution picture, sized for a 16:9 slide
#   <chart>.pptx  one-slide deck; charts are native PowerPoint shapes, so text,
#                 colours and lines can be edited in PowerPoint (right-click >
#                 Group > Ungroup). Maps are inserted as pictures.

Sys.setenv(PROJ_NETWORK = "OFF")
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(sf)
  library(terra)
})
terraOptions(progress = 0)

# ============================== USER SETTINGS ===============================

BASE_SIZE <- 18          # base font size (points) for charts
FONT_FAMILY <- ""        # "" = default; e.g. "Arial", "Calibri" (must be installed)
SHOW_TITLES <- TRUE      # FALSE drops chart titles/subtitles (use PowerPoint slide titles instead)
DPI <- 300               # PNG resolution

# Chart sizes in inches. A 16:9 PowerPoint slide is 13.33 x 7.5.
W_FULL <- 12
H_FULL <- 5.6
W_HALF <- 6.2            # two maps side by side
H_HALF <- 5.8

# Your own PowerPoint template (e.g. an organisation template), or NULL for a
# plain 16:9 deck. Charts are placed on its "Blank" layout.
PPTX_TEMPLATE <- NULL

# Group labels: change the wording here and every chart follows.
G <- list(
  TV = "TRG → Verdant",
  KH = "TRG → The Nature Conservancy (Heartlands)",
  SB = "TRG → Verdant → American Songbird (2026)",
  TO = "TRG → other owners / left CFP",
  MW = "Molpus → Molpus (mostly kept)",
  LY = "Lyme → Lyme",
  NK = "Other private → same owner",
  NS = "Other private → new owner / left CFP"
)

# Group colours, in the same order as G.
GROUP_COLS <- setNames(
  c("#7570b3", "#1b9e77", "#d95f02", "#56b4e9", "#e7298a", "#f0c419", "#d2b48c", "#5c3a1e"),
  unlist(G)
)

# Groups shown in the by-year line charts.
FOCUS <- c(G$TV, G$KH, G$SB, G$NK)

# Disturbance type colours (all-land charts).
DIST_COLS <- c(
  "Fire" = "#e31a1c",
  "Wind, weather or stress" = "#41b6c4",
  "Insects or disease" = "#7570b3",
  "Harvest or mechanical" = "#d95f02",
  "Unknown cause" = "#08519c",
  "Other" = "grey65"
)

# Vegetation physiognomy colours (BpS / EVT charts).
PHYS_COLS <- c(
  "Hardwood" = "#66a61e",
  "Conifer" = "#1b7837",
  "Hardwood-Conifer" = "#a6d96a",
  "Conifer-Hardwood" = "#a6d96a",
  "Riparian" = "#41b6c4",
  "Agricultural" = "#e6ab02",
  "Developed-Roads" = "grey55",
  "Developed" = "grey55",
  "Exotic Herbaceous" = "#d95f02",
  "Shrubland" = "#a6761d",
  "Grassland" = "#fdd49e",
  "Sparsely Vegetated" = "#d9d9d9"
)

# ============================================================================

root <- if (dir.exists("output_csvs")) "." else if (dir.exists("../../output_csvs")) "../.." else
  stop("Run from the kee_trees project root (or open kee_trees.Rproj).")
PPT_DIR <- file.path(root, "output_visuals", "powerpoint")
dir.create(PPT_DIR, showWarnings = FALSE, recursive = TRUE)

csv <- function(f) read.csv(file.path(root, "output_csvs", f), stringsAsFactors = FALSE, check.names = FALSE)
spatial <- function(f) file.path(root, "output_spatial", f)
input <- function(f) file.path(root, "inputs", f)

fmt_n <- function(x) format(round(as.numeric(x)), big.mark = ",", trim = TRUE)
fmt_1 <- function(x) sprintf("%.1f", as.numeric(x))
pct_lab <- function(x) paste0(x, "%")

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

theme_ppt <- function(base_size = BASE_SIZE) {
  t <- theme_minimal(base_size = base_size, base_family = FONT_FAMILY) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "right",
      plot.title.position = "plot",
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey35")
    )
  if (!SHOW_TITLES) t <- t + theme(plot.title = element_blank(), plot.subtitle = element_blank())
  t
}

theme_map <- function(base_size = BASE_SIZE - 2) {
  t <- theme_void(base_size = base_size, base_family = FONT_FAMILY) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  if (!SHOW_TITLES) t <- t + theme(plot.title = element_blank(), plot.subtitle = element_blank())
  t
}

# Sale footprints and counties, in the analysis grid's CRS.
read_footprints <- function(crs_ref = crs(rast(input("tcc/HK_TCC_2020.tif")))) {
  list(
    Heartlands = st_transform(st_read(input("boundaries/keweenaw_heartlands.shp"), quiet = TRUE), crs_ref),
    Songbird = st_transform(st_read(input("boundaries/american_songbird.shp"), quiet = TRUE), crs_ref)
  )
}

# ---- PowerPoint output ----------------------------------------------------

HAVE_OFFICER <- requireNamespace("officer", quietly = TRUE) && requireNamespace("rvg", quietly = TRUE)
if (!HAVE_OFFICER) message("Packages officer/rvg not installed: writing PNGs only.")
if (!exists("CHARTS")) CHARTS <- list()

widescreen_template <- function() {
  src <- system.file("template", "template.pptx", package = "officer")
  tmp <- tempfile("pptx_")
  utils::unzip(src, exdir = tmp)
  f <- file.path(tmp, "ppt", "presentation.xml")
  x <- readLines(f, warn = FALSE, encoding = "UTF-8")
  writeLines(gsub("<p:sldSz[^>]*/>", '<p:sldSz cx="12192000" cy="6858000"/>', x), f, useBytes = TRUE)
  out <- tempfile(fileext = ".pptx")
  zip::zip(out, files = list.files(tmp, recursive = TRUE, all.files = TRUE), root = tmp)
  out
}

new_deck <- function() {
  officer::read_pptx(if (is.null(PPTX_TEMPLATE)) widescreen_template() else PPTX_TEMPLATE)
}

add_chart_slide <- function(doc, chart) {
  doc <- officer::add_slide(doc, layout = "Blank")
  ss <- officer::slide_size(doc)
  w <- min(chart$width, ss$width - 0.4)
  h <- min(chart$height, ss$height - 0.4)
  loc <- officer::ph_location(left = (ss$width - w) / 2, top = (ss$height - h) / 2, width = w, height = h)
  value <- if (chart$editable) rvg::dml(ggobj = chart$plot) else officer::external_img(chart$png, width = w, height = h)
  officer::ph_with(doc, value, location = loc)
}

add_table_slide <- function(doc, df, title = NULL) {
  doc <- officer::add_slide(doc, layout = "Title Only")
  if (!is.null(title)) doc <- officer::ph_with(doc, title, location = officer::ph_location_type("title"))
  ss <- officer::slide_size(doc)
  officer::ph_with(doc, df, location = officer::ph_location(left = 0.6, top = 1.6, width = ss$width - 1.2, height = 3))
}

# Save a chart as PNG (and a one-slide editable .pptx). Maps: editable = FALSE.
save_chart <- function(p, name, width = W_FULL, height = H_FULL, editable = TRUE) {
  png <- file.path(PPT_DIR, paste0(name, ".png"))
  ggsave(png, p, width = width, height = height, dpi = DPI, bg = "white")
  chart <- list(plot = p, png = png, width = width, height = height, editable = editable)
  CHARTS[[name]] <<- chart
  if (HAVE_OFFICER) print(add_chart_slide(new_deck(), chart), target = file.path(PPT_DIR, paste0(name, ".pptx")))
  message("Wrote ", png)
  invisible(p)
}

# Save a table as CSV (and a one-slide .pptx with a native PowerPoint table).
save_table <- function(df, name, title = NULL) {
  write.csv(df, file.path(PPT_DIR, paste0(name, ".csv")), row.names = FALSE)
  CHARTS[[name]] <<- list(table = df, title = title)
  if (HAVE_OFFICER) print(add_table_slide(new_deck(), df, title), target = file.path(PPT_DIR, paste0(name, ".pptx")))
  message("Wrote ", file.path(PPT_DIR, paste0(name, ".csv")))
  invisible(df)
}

CHART_SETUP_LOADED <- TRUE
