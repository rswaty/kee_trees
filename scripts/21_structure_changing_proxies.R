# Current structure-changing disturbance, all land in Houghton and Keweenaw counties
# (mainland, open water excluded; same land as scripts/18), from three measures:
#   LANDFIRE high severity: LANDFIRE Annual Disturbance cells rated High severity
#     (2010-2025). LANDFIRE severity ratings are conservative, so this is the low end.
#   Hansen: Global Forest Change stand-replacing forest loss (2010-2024).
#   Canopy fell 20+ points: USFS Tree Canopy Cover (TCC) at least 20 points below its
#     highest value in the previous 3 years, each cell counted once per 3 years (2013-2025).
#     Includes heavy partial cuts, so this is the high end.
# "Structure-changing" means a disturbance that would move a stand to a different
# succession class in the Biophysical Settings (BpS) models.
# Compared with the historical structure-changing rate from scripts/18.
# Requires scripts/18 (hk_disturbance_totals.csv) and output_spatial/hansen_lossyear.tif.
# Run from project root: Rscript scripts/21_structure_changing_proxies.R

# ---- Setup ----
# Stop the PROJ library from trying to download projection grids from the internet.
Sys.setenv(PROJ_NETWORK = "OFF")

# terra reads rasters, sf reads the county boundary, foreign reads the .dbf
# attribute tables that come with LANDFIRE rasters, dplyr builds tables.
suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(foreign)
  library(dplyr)
})

# Make sure the script is run from the project folder, then set paths.
root <- if (dir.exists("inputs/landfire/annual_disturbance") && dir.exists("inputs/tcc")) {
  "."
} else {
  stop("Run from the kee_trees project root.")
}
csv_dir <- file.path(root, "output_csvs")
dist_dir <- file.path(root, "inputs/landfire/annual_disturbance")

# Settings: a canopy drop is a fall of 20 or more percentage points from the
# highest value in the previous 3 years. 20 points is stricter than the
# 15 points used in the footprint maps (script 17), to focus on heavier
# canopy removal. The three names label the measures in the outputs.
DROP_PP <- 20
LOOKBACK <- 3
LF_HIGH <- "LANDFIRE high severity"
HANSEN <- "Hansen stand-replacing loss"
TCC20 <- "Canopy fell 20+ points"

# ---- Land mask (LANDFIRE grid), as in scripts/18 ----
# "Land" = cells inside the two counties that have a BpS value and are not
# open water. Cell area in acres uses 4046.856 square metres per acre
# (a 30 m cell is about 0.22 acres).
bps_dir <- file.path(root, "inputs/landfire/LF2020_BPS")
bps <- rast(list.files(bps_dir, pattern = "\\.tif$", full.names = TRUE)[1])
bps_vat <- read.dbf(list.files(bps_dir, pattern = "vat\\.dbf$", full.names = TRUE)[1], as.is = TRUE)
counties <- st_read(file.path(root, "inputs/boundaries/houghton_keweenaw_counties.shp"), quiet = TRUE) |>
  st_transform(crs(bps))
inside <- !is.na(values(rasterize(vect(counties), bps), mat = FALSE))
bps_v <- values(bps, mat = FALSE)
land_lf <- inside & !is.na(bps_v) & !(bps_v %in% bps_vat$Value[bps_vat$BPS_NAME == "Open Water"])
lf_px <- prod(res(bps)) / 4046.8564224
land_acres <- sum(land_lf) * lf_px

# ---- LANDFIRE high severity ----
# For each annual disturbance layer (folder names end in "_Dist" plus a
# two-digit year), use the attribute table to find the codes whose SEVERITY
# is "High", and total the land acres with those codes. LANDFIRE severity
# categories in these tables are Low, Medium or Moderate, High, and
# "No Severity" (mapped from records with no detected image change); only
# High counts here.
layers <- sort(list.dirs(dist_dir, recursive = FALSE, full.names = TRUE))
layers <- layers[grepl("_Dist[0-9]{2}$", basename(layers))]
lf_high <- bind_rows(lapply(layers, function(d) {
  r <- rast(list.files(d, pattern = "\\.tif$", full.names = TRUE)[1])
  vat <- read.dbf(list.files(d, pattern = "vat\\.dbf$", full.names = TRUE)[1], as.is = TRUE)
  v <- values(r, mat = FALSE)
  high <- vat$Value[!is.na(vat$SEVERITY) & vat$SEVERITY == "High"]
  data.frame(year = 2000L + as.integer(sub(".*_Dist", "", basename(d))),
             measure = LF_HIGH, annual_acres = sum(land_lf & v %in% high) * lf_px)
}))

# ---- Canopy cover and Hansen (canopy grid; land mask carried over from LANDFIRE) ----
# Helper: read one TCC year with the 254/255 no-data codes set to NA.
read_tcc <- function(y) subst(rast(file.path(root, sprintf("inputs/tcc/HK_TCC_%d.tif", y))), c(254, 255), NA)
tcc_ref <- read_tcc(2020)
# Copy the LANDFIRE land mask onto the TCC grid (nearest neighbour, so it
# stays a yes/no mask) so all three measures cover the same land.
land_r <- bps
values(land_r) <- ifelse(land_lf, 1, NA)
land_tcc <- !is.na(values(project(land_r, tcc_ref, method = "near"), mat = FALSE))
tcc_px <- prod(res(tcc_ref)) / 4046.8564224

# Build a table of canopy values: one row per land cell, one column per year.
years <- 2010:2025
m <- sapply(years, function(y) values(read_tcc(y), mat = FALSE)[land_tcc])
# Flag a cell in a year when canopy is at least 20 points below its highest
# value in the previous 3 years. The first possible year is 2013, because
# 2010-2012 are needed as the lookback.
flag <- matrix(FALSE, nrow(m), length(years))
for (i in (LOOKBACK + 1):length(years)) {
  peak <- suppressWarnings(apply(m[, (i - LOOKBACK):(i - 1), drop = FALSE], 1, max, na.rm = TRUE))
  flag[, i] <- !is.na(m[, i]) & is.finite(peak) & (peak - m[, i]) >= DROP_PP
}
# A single cut can stay below the old peak for several years, so it would be
# flagged repeatedly. Keep a flag only if the cell was not already flagged in
# any of the previous 3 years.
new_flag <- flag
for (i in (LOOKBACK + 1):length(years)) {
  new_flag[, i] <- flag[, i] & !apply(flag[, (i - LOOKBACK):(i - 1), drop = FALSE], 1, any)
}
# Acres per year of new canopy drops, starting in 2013.
tcc_drop <- data.frame(year = years, measure = TCC20, annual_acres = colSums(new_flag) * tcc_px) |>
  filter(year >= min(years) + LOOKBACK)

# Hansen loss year (already on the TCC grid) is stored as years since 2000
# with 0 = no loss. Count land cells lost per year from 2010 on and convert to acres.
hansen <- values(rast(file.path(root, "output_spatial/hansen_lossyear.tif")), mat = FALSE)[land_tcc]
han <- data.frame(year = 2000L + hansen[!is.na(hansen) & hansen > 0]) |>
  count(year, name = "n") |>
  filter(year >= 2010) |>
  transmute(year, measure = HANSEN, annual_acres = n * tcc_px)

# Combine the three measures into one year-by-year table.
by_year <- bind_rows(lf_high, han, tcc_drop) |> arrange(measure, year)
write.csv(by_year, file.path(csv_dir, "hk_structure_changing_by_year.csv"), row.names = FALSE)

# ---- Period averages vs. the historical structure-changing rate ----
# Historical structure-changing acres per year, from script 18's BpS model
# calculation (reference class share x annual transition probability).
hist_chg <- read.csv(file.path(csv_dir, "hk_disturbance_totals.csv"), check.names = FALSE) |>
  filter(period == "Historical: structure-changing only") |>
  pull(annual_acres)
# Average each measure over the 2010s and over 2020 onward, then express it
# as a percent of land per year and as a multiple of the historical rate
# (e.g. 2 = twice the historical rate).
summary_tab <- by_year |>
  mutate(period = if_else(year >= 2020, "2020 onward", "2010s")) |>
  group_by(measure, period) |>
  summarise(first_year = min(year), last_year = max(year), annual_acres = mean(annual_acres), .groups = "drop") |>
  mutate(pct_of_land = 100 * annual_acres / land_acres,
         times_historical = annual_acres / hist_chg)
write.csv(summary_tab, file.path(csv_dir, "hk_structure_changing_summary.csv"), row.names = FALSE)

message(sprintf("Land: %s acres; historical structure-changing: %s ac/yr",
                format(round(land_acres), big.mark = ","), format(round(hist_chg), big.mark = ",")))
print(summary_tab |> mutate(across(where(is.numeric), ~ round(.x, 2))))
