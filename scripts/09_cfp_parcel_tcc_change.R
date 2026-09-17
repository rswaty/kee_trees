# Parcel-level TCC change (2020→2025) with CFP owner type 2020 and 2026.
# Unit: 2020 CFP parcels (attribute acres >= MIN_ACRES). Ranked by |Δ| pp.
# Ownership 2026 joined on parid; parcels missing in 2026 = left CFP.
# Run from project root: Rscript scripts/09_cfp_parcel_tcc_change.R
# Does not touch Shiny.

Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
})

root <- if (dir.exists("data/cfp_data") && dir.exists("data/TCC_Houghton_Keweenaw")) {
  "."
} else {
  stop("Run from the kee_trees project root.")
}

cfp_dir <- file.path(root, "data/cfp_data")
tcc_dir <- file.path(root, "data/TCC_Houghton_Keweenaw")
out_dir <- file.path(root, "data/processed")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

MIN_ACRES <- 20
EXIT_LABEL <- "non-CFP in 2026"

message("Loading parcels and TCC...")
p20 <- st_read(file.path(cfp_dir, "cfp_hk_2020.shp"), quiet = TRUE) |>
  st_make_valid()
p26 <- st_read(file.path(cfp_dir, "cfp_hk_2026.shp"), quiet = TRUE) |>
  st_make_valid()

tcc20 <- subst(rast(file.path(tcc_dir, "HK_TCC_2020.tif")), c(254, 255), NA)
tcc25 <- subst(rast(file.path(tcc_dir, "HK_TCC_2025.tif")), c(254, 255), NA)

p20 <- st_transform(p20, crs(tcc20))
p26 <- st_transform(p26, crs(tcc20))

p20$parcel_acres <- as.numeric(p20$acres)
p20$owner_type_2020 <- trimws(as.character(p20$owner_type))
p20$owner_type_2020[is.na(p20$owner_type_2020) | !nzchar(p20$owner_type_2020)] <-
  "(missing owner type)"
p20$parid <- as.character(p20$parid)

p26$owner_type_2026 <- trimws(as.character(p26$OwnerTypeT))
p26$owner_type_2026[is.na(p26$owner_type_2026) | !nzchar(p26$owner_type_2026)] <-
  "(missing owner type)"
p26$parid <- as.character(p26$parid)

# One row per parid in 2026 (duplicate 37663 is identical)
own26 <- p26 |>
  st_drop_geometry() |>
  distinct(parid, .keep_all = TRUE) |>
  select(parid, owner_type_2026, acres_2026 = Acres)

p20 <- p20[!is.na(p20$parcel_acres) & p20$parcel_acres >= MIN_ACRES, ]
message(sprintf("2020 parcels >= %d ac: %d", MIN_ACRES, nrow(p20)))

message("Zonal mean TCC 2020 and 2025 per 2020 parcel...")
v20 <- vect(p20)
z20 <- terra::extract(tcc20, v20, fun = mean, na.rm = TRUE, ID = FALSE)
z25 <- terra::extract(tcc25, v20, fun = mean, na.rm = TRUE, ID = FALSE)
n_pix <- terra::extract(tcc20, v20, fun = function(x) sum(!is.na(x)), ID = FALSE)

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
  mutate(
    owner_type_2026 = if_else(is.na(owner_type_2026), EXIT_LABEL, owner_type_2026),
    still_in_cfp_2026 = owner_type_2026 != EXIT_LABEL,
    owner_type_changed = still_in_cfp_2026 & (owner_type_2020 != owner_type_2026),
    delta_pp = mean_tcc_2025 - mean_tcc_2020,
    abs_delta_pp = abs(delta_pp)
  ) |>
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

out_path <- file.path(out_dir, "cfp_parcel_tcc_change_by_owner_type.csv")
write.csv(out, out_path, row.names = FALSE)
message("Wrote ", out_path, " (", nrow(out), " parcels)")

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
