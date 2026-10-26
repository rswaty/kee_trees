# Pixel-level canopy change and Hansen loss timing by sale footprint / owner zone.
# Canopy = USFS tree canopy cover (TCC); loss timing = Hansen Global Forest Change
# loss year. Both are 30 m rasters on the same grid.
# Zones (2020 Commercial Forest Program (CFP) parcels >= MIN_ACRES, plus Songbird and
# Heartlands footprints painted on top of TRG land only): TRG/Verdant retained,
# TRG other sale / left CFP, Lyme, MWF, non-institutional kept, non-institutional
# sold / left, Heartlands (TRG to The Nature Conservancy, TNC), Songbird.
# 2026 owners and same-owner rules come from scripts/lib_owner_2026.R (renumbered
# parcels are followed by location; names are matched word by word).
# Writes the zone raster used by scripts/12_sale_footprint_landfire_tcc.R.
# Run from project root: Rscript scripts/11_sale_footprint_pixels.R
# Does not touch Shiny.

# Stop the PROJ map-projection library from trying to download grid files.
Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
  library(tidyr)
})

# The figure is optional: skip it if ggplot2 is not installed.
has_ggplot <- requireNamespace("ggplot2", quietly = TRUE)
if (has_ggplot) {
  suppressPackageStartupMessages(library(ggplot2))
}

# ---- Setup ----
# Paths are relative to the project folder, so stop early if run from elsewhere.
root <- if (dir.exists("output_csvs") && dir.exists("inputs/tcc")) {
  "."
} else {
  stop("Run from the kee_trees project root.")
}
source(file.path(root, "scripts/lib_owner_2026.R"))

cfp_dir <- file.path(root, "inputs/cfp")
tcc_dir <- file.path(root, "inputs/tcc")
csv_dir <- file.path(root, "output_csvs")
gis_dir <- file.path(root, "output_spatial")
fig_dir <- file.path(root, "output_visuals")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# Boundaries of the two named sales of former TRG/Verdant land.
songbird_path <- file.path(
  root,
  "inputs/boundaries/american_songbird.shp"
)
heartlands_path <- file.path(root, "inputs/boundaries/keweenaw_heartlands.shp")

# Same 20-acre parcel minimum as scripts 09 and 10.
MIN_ACRES <- 20
# A cell counts as heavily cut when its canopy fell by more than 20 percentage points.
DROP_PP <- -20
# Hansen loss years reported in the year-by-year profile.
HANSEN_YEARS <- 2015:2024

# Integer code stored in the zone raster for each zone. 0 (set below) means land
# outside any 2020 CFP parcel of at least MIN_ACRES. The footprint codes are set
# well apart (20, 21) from the ownership codes.
ZONES <- c(
  "TRG/Verdant retained" = 1L,
  "TRG other sale / left CFP" = 2L,
  "Lyme" = 3L,
  "MWF" = 4L,
  "Non-institutional kept" = 5L,
  "Non-institutional sold / left" = 6L,
  "Heartlands (TRG to TNC)" = 20L,
  "Songbird (TRG to Songbird, 2026)" = 21L
)

# Name cleanup helper (same as script 10): lower case, punctuation and legal
# suffixes such as LLC removed. Not used by the zone rules below.
norm_name <- function(s) {
  s <- tolower(as.character(s))
  s[is.na(s)] <- ""
  s <- gsub("[^a-z0-9 ]", " ", s)
  s <- gsub("\\b(llc|inc|ltd|co|company|corp|corporation|limited)\\b", "", s)
  gsub("\\s+", " ", trimws(s))
}

# Read one year of TCC (percent canopy per 30 m cell). Values 254 and 255 are
# no-data codes, so they are set to NA.
read_tcc <- function(y) {
  subst(rast(file.path(tcc_dir, sprintf("HK_TCC_%d.tif", y))), c(254, 255), NA)
}

# ---- Load rasters and parcels ----
message("Loading TCC, Hansen, CFP, footprints...")
tcc15 <- read_tcc(2015)
tcc20 <- read_tcc(2020)
tcc25 <- read_tcc(2025)
# Hansen loss year: 0 = no loss, 1-24 = loss in 2001-2024. The check stops the
# script if it is not on exactly the same grid as TCC.
hansen <- rast(file.path(gis_dir, "hansen_lossyear.tif"))
stopifnot(compareGeom(tcc20, hansen))
# Area of one cell in acres: cell width x height in square metres (30 m x 30 m),
# divided by 4046.856 square metres per acre (about 0.22 acres per cell).
px_acres <- prod(res(tcc20)) / 4046.8564224

# Parcels, repaired (st_make_valid) and moved to the raster's map projection.
p20 <- st_read(file.path(cfp_dir, "cfp_hk_2020.shp"), quiet = TRUE) |>
  st_make_valid() |>
  st_transform(crs(tcc20))
p26 <- st_read(file.path(cfp_dir, "cfp_hk_2026.shp"), quiet = TRUE) |>
  st_make_valid() |>
  st_transform(crs(tcc20))

# Keep 2020 parcels of at least MIN_ACRES (recorded acres).
p20 <- p20 |>
  mutate(
    parid = as.character(parid),
    acres = as.numeric(acres),
    name_2020 = trimws(as.character(search_nam))
  ) |>
  filter(!is.na(acres), acres >= MIN_ACRES)
# ---- Assign each parcel to a zone ----
# Add the 2026 owner (by parcel ID, or by at least 50% map overlap for renumbered
# parcels), then work out the 2020 firm and the zone.
p20 <- p20 |>
  left_join(owner_2026_lookup(p20, p26), by = "parid") |>
  mutate(
    # Institutional firm from the 2020 owner name; NA = non-institutional owner.
    firm = case_when(
      grepl("TRG\\s*THRESHOLD|VERDANT", name_2020, ignore.case = TRUE) ~ "TRG",
      grepl("\\bLYME\\b", name_2020, ignore.case = TRUE) ~ "Lyme",
      grepl("MOLPUS|\\bMWF\\b", name_2020, ignore.case = TRUE) ~ "MWF",
      TRUE ~ NA_character_
    ),
    # Same owner in 2026 by the shared word-by-word name rules.
    same_name = same_owner_2026(name_2020, name_2026, owner_type, lastname),
    # TRG land counts as retained when the 2026 owner is still TRG, Threshold or
    # Verdant (the rename from TRG to Verdant is not a sale).
    trg_retained = firm %in% "TRG" &
      grepl("verdant|trg|threshold", name_2026, ignore.case = TRUE),
    # First matching rule wins. Lyme and MWF are not split into kept and sold.
    zone = case_when(
      trg_retained ~ "TRG/Verdant retained",
      firm %in% "TRG" ~ "TRG other sale / left CFP",
      firm %in% "Lyme" ~ "Lyme",
      firm %in% "MWF" ~ "MWF",
      same_name ~ "Non-institutional kept",
      TRUE ~ "Non-institutional sold / left"
    ),
    zone_code = unname(ZONES[zone])
  )

# ---- Build the zone raster ----
# Burn parcel zone codes onto the TCC grid; cells outside the parcels get 0.
zr <- rasterize(vect(p20), tcc20, field = "zone_code", background = 0)
kh <- vect(st_transform(st_read(heartlands_path, quiet = TRUE), crs(tcc20)))
sb <- vect(st_transform(st_read(songbird_path, quiet = TRUE), crs(tcc20)))
# Sale footprints are painted only over 2020 TRG land; other owners' parcels and
# unenrolled land inside the boundaries keep their own zone.
# This keeps the footprint zones a clean "former TRG land" comparison.
trg_cells <- zr %in% ZONES[c("TRG/Verdant retained", "TRG other sale / left CFP")]
in_kh <- !is.na(rasterize(kh, tcc20)) & trg_cells
in_sb <- !is.na(rasterize(sb, tcc20)) & trg_cells
zr <- ifel(in_kh, ZONES[["Heartlands (TRG to TNC)"]], zr)
zr <- ifel(in_sb, ZONES[["Songbird (TRG to Songbird, 2026)"]], zr)
names(zr) <- "zone_code"
# Save the zone raster as small whole numbers (INT1U = 0-255), compressed, plus a
# lookup table of code to zone name. Script 12 reads both.
writeRaster(
  zr, file.path(gis_dir, "cfp_sale_zones.tif"),
  overwrite = TRUE, wopt = list(datatype = "INT1U", gdal = c("COMPRESS=DEFLATE"))
)
write.csv(
  data.frame(zone_code = unname(ZONES), zone = names(ZONES)),
  file.path(csv_dir, "cfp_sale_zone_labels.csv"),
  row.names = FALSE
)

# Pull every cell's values into plain vectors. All rasters share one grid, so
# position i is the same cell in each vector.
z <- values(zr, mat = FALSE)
v15 <- values(tcc15, mat = FALSE)
v20 <- values(tcc20, mat = FALSE)
v25 <- values(tcc25, mat = FALSE)
ly <- values(hansen, mat = FALSE)

# ---- Zone summaries ----
# For each zone: area, mean canopy in 2015, 2020 and 2025, mean change before and
# after 2020, percent of area cut by more than 20 points in each period, and
# percent with Hansen loss in 2015-2019 versus 2020-2024. Canopy statistics use
# only cells with valid TCC in all three years; Hansen shares use all zone cells.
message("Summarising zones...")
zone_rows <- lapply(names(ZONES), function(nm) {
  m <- z == ZONES[[nm]]
  m[is.na(m)] <- FALSE
  ok <- m & !is.na(v15) & !is.na(v20) & !is.na(v25)
  d_pre <- v20[ok] - v15[ok]
  d_post <- v25[ok] - v20[ok]
  l <- ly[m]
  data.frame(
    zone = nm,
    acres = sum(m) * px_acres,
    mean_tcc_2015 = mean(v15[ok]),
    mean_tcc_2020 = mean(v20[ok]),
    mean_tcc_2025 = mean(v25[ok]),
    delta_pp_2015_2020 = mean(d_pre),
    delta_pp_2020_2025 = mean(d_post),
    pct_area_drop20_2015_2020 = 100 * mean(d_pre < DROP_PP),
    pct_area_drop20_2020_2025 = 100 * mean(d_post < DROP_PP),
    pct_hansen_loss_2015_2019 = 100 * mean(l >= 15 & l <= 19, na.rm = TRUE),
    pct_hansen_loss_2020_2024 = 100 * mean(l >= 20 & l <= 24, na.rm = TRUE)
  )
})
zone_summary <- bind_rows(zone_rows)

# Hansen loss acres in each year, per zone, and as a percent of the zone's area.
# Hansen stores year 2017 as 17, hence y - 2000.
year_rows <- lapply(names(ZONES), function(nm) {
  m <- z == ZONES[[nm]]
  m[is.na(m)] <- FALSE
  l <- ly[m]
  data.frame(
    zone = nm,
    year = HANSEN_YEARS,
    loss_acres = vapply(HANSEN_YEARS, function(y) sum(l == y - 2000, na.rm = TRUE), numeric(1)) *
      px_acres,
    zone_acres = sum(m) * px_acres
  )
})
year_profile <- bind_rows(year_rows) |>
  mutate(pct_of_zone = 100 * loss_acres / zone_acres)

# ---- Write outputs ----
zone_path <- file.path(csv_dir, "cfp_sale_footprint_zone_summary.csv")
year_path <- file.path(csv_dir, "cfp_sale_footprint_hansen_by_year.csv")
write.csv(zone_summary, zone_path, row.names = FALSE)
write.csv(year_profile, year_path, row.names = FALSE)
message("Wrote ", zone_path)
message("Wrote ", year_path)

# ---- Figure ----
# Line chart of yearly Hansen loss for the two sale footprints and two
# comparison zones.
if (has_ggplot) {
  focus <- c(
    "Songbird (TRG to Songbird, 2026)",
    "Heartlands (TRG to TNC)",
    "TRG/Verdant retained",
    "Non-institutional kept"
  )
  g <- year_profile |>
    filter(zone %in% focus) |>
    mutate(zone = factor(zone, levels = focus)) |>
    ggplot(aes(year, pct_of_zone, color = zone)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.6) +
    scale_x_continuous(breaks = HANSEN_YEARS) +
    labs(
      title = "Hansen forest loss by year, share of each zone",
      subtitle = "Songbird land sold 2026; Heartlands land sold to The Nature Conservancy",
      x = NULL,
      y = "Percent of zone area lost that year",
      color = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank()) +
    guides(color = guide_legend(nrow = 2))
  fig_path <- file.path(fig_dir, "cfp_sale_footprint_hansen_by_year.png")
  ggsave(fig_path, g, width = 8, height = 4.8, dpi = 150)
  message("Wrote ", fig_path)
}

# ---- Console report ----
message("\n=== Zone summary ===")
print(zone_summary, row.names = FALSE, digits = 3)
# Wide table: one row per zone, one column per year.
message("\n=== Hansen loss by year, percent of zone ===")
print(
  year_profile |>
    select(zone, year, pct_of_zone) |>
    pivot_wider(names_from = year, values_from = pct_of_zone) |>
    as.data.frame(),
  row.names = FALSE,
  digits = 2
)
message("Done.")
