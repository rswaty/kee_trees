# Parcel-level tree canopy cover (TCC) change, 2020→2025, with Commercial Forest
# Program (CFP) owner type in 2020 and 2026.
# Unit: 2020 CFP parcels (attribute acres >= MIN_ACRES). Ranked by the size of the
# change (|Δ|) in percentage points (pp) of canopy cover.
# 2026 owner type: by parcel ID, or for renumbered parcels by overlap (scripts/lib_owner_2026.R);
# neither = left CFP. acres_2026 = 2020 acres x share of the parcel still enrolled in 2026.
# The output table feeds scripts/10_institutional_sellers_vs_keepers.R.
# Run from project root: Rscript scripts/09_cfp_parcel_tcc_change.R
# Does not touch Shiny.

# Stop the PROJ map-projection library from trying to download grid files.
Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
})

# ---- Setup ----
# Paths are relative to the project folder, so stop early if run from elsewhere.
root <- if (dir.exists("inputs/cfp") && dir.exists("inputs/tcc")) {
  "."
} else {
  stop("Run from the kee_trees project root.")
}
# Shared rules for finding each parcel's 2026 owner.
source(file.path(root, "scripts/lib_owner_2026.R"))

cfp_dir <- file.path(root, "inputs/cfp")
tcc_dir <- file.path(root, "inputs/tcc")
csv_dir <- file.path(root, "output_csvs")
gis_dir <- file.path(root, "output_spatial")
for (d in c(csv_dir, gis_dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# Parcels smaller than 20 acres hold only a handful of 30 m canopy cells, so
# their averages are noisy; they are left out.
MIN_ACRES <- 20
# Owner-type label for parcels that are no longer enrolled in 2026.
EXIT_LABEL <- "non-CFP in 2026"

# ---- Load parcels and canopy rasters ----
# st_make_valid() repairs self-crossing or broken polygon outlines so that
# overlay steps later do not fail.
message("Loading parcels and TCC...")
p20 <- st_read(file.path(cfp_dir, "cfp_hk_2020.shp"), quiet = TRUE) |>
  st_make_valid()
p26 <- st_read(file.path(cfp_dir, "cfp_hk_2026.shp"), quiet = TRUE) |>
  st_make_valid()

# USFS TCC rasters (percent canopy per 30 m cell). Values 254 and 255 are
# no-data codes, not canopy values, so they are set to NA.
tcc20 <- subst(rast(file.path(tcc_dir, "HK_TCC_2020.tif")), c(254, 255), NA)
tcc25 <- subst(rast(file.path(tcc_dir, "HK_TCC_2025.tif")), c(254, 255), NA)

# Put the parcels in the same map projection as the canopy rasters.
p20 <- st_transform(p20, crs(tcc20))
p26 <- st_transform(p26, crs(tcc20))

# Tidy the 2020 attributes: acres as a number, blank owner types labelled, and
# parcel IDs as text so they join cleanly with the 2026 list.
p20$parcel_acres <- as.numeric(p20$acres)
p20$owner_type_2020 <- trimws(as.character(p20$owner_type))
p20$owner_type_2020[is.na(p20$owner_type_2020) | !nzchar(p20$owner_type_2020)] <-
  "(missing owner type)"
p20$parid <- as.character(p20$parid)

p26$parid <- as.character(p26$parid)

# Keep 2020 parcels of at least MIN_ACRES (using the acres recorded on the list).
p20 <- p20[!is.na(p20$parcel_acres) & p20$parcel_acres >= MIN_ACRES, ]
message(sprintf("2020 parcels >= %d ac: %d", MIN_ACRES, nrow(p20)))

# ---- 2026 owner type and share still enrolled ----
# For each 2020 parcel: the 2026 owner type (by parcel ID, or by map overlap of
# at least 50% for renumbered parcels) and the share of its area still in the
# program. A parcel still enrolled but with a blank owner type is labelled
# "(missing owner type)" so it is not mistaken for one that left.
message("Following each 2020 parcel into the 2026 list...")
own26 <- owner_2026_lookup(p20, p26) |>
  left_join(cfp_2026_share(p20, p26), by = "parid") |>
  mutate(owner_type_2026 = if_else(!is.na(name_2026) & (is.na(owner_type_2026) | !nzchar(owner_type_2026)),
                                   "(missing owner type)", owner_type_2026)) |>
  select(parid, owner_type_2026, share_in_cfp_2026)

# ---- Average canopy per parcel ----
# Mean canopy of all 30 m cells inside each 2020 parcel, for 2020 and 2025,
# skipping no-data cells. n_pix counts the cells that had valid 2020 data.
message("Zonal mean TCC 2020 and 2025 per 2020 parcel...")
v20 <- vect(p20)
z20 <- terra::extract(tcc20, v20, fun = mean, na.rm = TRUE, ID = FALSE)
z25 <- terra::extract(tcc25, v20, fun = mean, na.rm = TRUE, ID = FALSE)
n_pix <- terra::extract(tcc20, v20, fun = function(x) sum(!is.na(x)), ID = FALSE)

# ---- Build the parcel table ----
# One row per parcel with 2020 attributes and canopy averages.
out <- p20 |>
  st_drop_geometry() |>
  transmute(
    parid,
    county = County_Nam,
    acres_2020 = parcel_acres,
    owner_type_2020,
    mean_tcc_2020 = as.numeric(z20[[1]]),
    mean_tcc_2025 = as.numeric(z25[[1]]),
    n_tcc_pixels = as.integer(n_pix[[1]])
  ) |>
  left_join(own26, by = "parid") |>
  # No 2026 owner found means the parcel left the program. delta_pp is the
  # canopy change in percentage points (negative = canopy loss).
  mutate(
    owner_type_2026 = if_else(is.na(owner_type_2026), EXIT_LABEL, owner_type_2026),
    still_in_cfp_2026 = owner_type_2026 != EXIT_LABEL,
    acres_2026 = round(acres_2020 * share_in_cfp_2026, 1),
    owner_type_changed = still_in_cfp_2026 & (owner_type_2020 != owner_type_2026),
    delta_pp = mean_tcc_2025 - mean_tcc_2020,
    abs_delta_pp = abs(delta_pp)
  ) |>
  # Drop parcels with no usable canopy cells, then rank by the size of the change
  # (gain or loss), larger parcels first on ties.
  filter(!is.na(delta_pp), n_tcc_pixels > 0) |>
  arrange(desc(abs_delta_pp), desc(acres_2020)) |>
  mutate(rank_abs_delta = row_number()) |>
  select(
    rank_abs_delta,
    parid,
    county,
    acres_2020,
    acres_2026,
    n_tcc_pixels,
    mean_tcc_2020,
    mean_tcc_2025,
    delta_pp,
    abs_delta_pp,
    owner_type_2020,
    owner_type_2026,
    owner_type_changed,
    still_in_cfp_2026
  )

# ---- Write and report ----
out_path <- file.path(csv_dir, "cfp_parcel_tcc_change_by_owner_type.csv")
write.csv(out, out_path, row.names = FALSE)
message("Wrote ", out_path, " (", nrow(out), " parcels)")

# Print the 25 parcels with the largest canopy change to the console.
message("\n=== Top 25 by |Δ TCC| (pp), 2020→2025 ===")
print(
  out |>
    head(25) |>
    select(
      rank_abs_delta, parid, acres_2020, mean_tcc_2020, mean_tcc_2025,
      delta_pp, owner_type_2020, owner_type_2026
    ),
  row.names = FALSE,
  digits = 2
)

# Headline counts: owner-type changes, exits from the program, and the spread
# of canopy change.
message("\n=== Summary ===")
message(sprintf("Parcels ranked: %d", nrow(out)))
message(sprintf(
  "Owner type changed (still in CFP): %d (%.1f%%)",
  sum(out$owner_type_changed),
  100 * mean(out$owner_type_changed)
))
message(sprintf("Left CFP by 2026: %d", sum(!out$still_in_cfp_2026)))
message(sprintf(
  "Median |Δ|: %.2f pp; 95th pct |Δ|: %.2f pp; max |Δ|: %.2f pp",
  median(out$abs_delta_pp),
  quantile(out$abs_delta_pp, 0.95),
  max(out$abs_delta_pp)
))
message("Done.")
