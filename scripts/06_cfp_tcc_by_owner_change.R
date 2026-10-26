# Mean tree canopy cover (TCC) in 2020 vs 2025 on Commercial Forest Program (CFP)
# cells, grouped by what happened to the owner type between 2020 and 2026.
# Run from project root: Rscript scripts/06_cfp_tcc_by_owner_change.R
# Does not touch Shiny.
# Needs the owner rasters and label table written by scripts/04_cfp_ownership_change.R.

# Stop the PROJ library from trying to download projection files from the internet.
Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
})

# ---- Folders ----
# All paths are relative to the project folder, so stop early if run from elsewhere.
root <- if (dir.exists("output_csvs") && dir.exists("inputs/tcc")) {
  "."
} else {
  stop("Run from the kee_trees project root.")
}

csv_dir <- file.path(root, "output_csvs")

gis_dir <- file.path(root, "output_spatial")
tcc_dir <- file.path(root, "inputs/tcc")

# ---- Load CFP owner-type rasters and TCC ----
# Owner rasters hold a number per cell for owner type (parcels of 20 acres or
# more only); the label table turns those numbers back into names.
r20 <- rast(file.path(gis_dir, "cfp_owner_2020.tif"))
r26 <- rast(file.path(gis_dir, "cfp_owner_2026.tif"))
lut <- read.csv(file.path(csv_dir, "cfp_owner_labels.csv"), stringsAsFactors = FALSE)
code_to_label <- setNames(lut$label, lut$code)

# TCC values 254 and 255 are no-data codes, so set them to missing. Then trim or
# pad the TCC rasters to exactly the CFP raster's extent so cells line up one-to-one.
# 2025 is used as the "after" year because there is no 2026 TCC.
tcc20 <- subst(rast(file.path(tcc_dir, "HK_TCC_2020.tif")), c(254, 255), NA)
tcc25 <- subst(rast(file.path(tcc_dir, "HK_TCC_2025.tif")), c(254, 255), NA)
tcc20 <- extend(crop(tcc20, r20), r20)
tcc25 <- extend(crop(tcc25, r20), r20)

# Acres per cell: 30 m x 30 m = 900 square metres, divided by 4046.856 square
# metres per acre (about 0.22 acres).
px_acres <- prod(res(r20)) / 4046.8564224

# Pull every cell's value into simple lists, all in the same cell order.
v20 <- values(r20, mat = FALSE)
v26 <- values(r26, mat = FALSE)
c20 <- values(tcc20, mat = FALSE)
c25 <- values(tcc25, mat = FALSE)

# ---- Define ownership cohorts ----
# Each cohort is a TRUE/FALSE flag per cell:
#   both_cfp = in CFP in both 2020 and 2026
#   changed  = in CFP both years but with a different owner type
#   stable   = in CFP both years with the same owner type
#   exited   = in CFP in 2020 but not 2026
#   entered  = in CFP in 2026 but not 2020
both_cfp <- !is.na(v20) & !is.na(v26)
changed <- both_cfp & (v20 != v26)
stable <- both_cfp & (v20 == v26)
exited <- !is.na(v20) & is.na(v26)
entered <- is.na(v20) & !is.na(v26)

# Use only cells with a valid TCC value in both 2020 and 2025.
ok <- function(idx) idx & !is.na(c20) & !is.na(c25)

# ---- Summarize TCC for each cohort ----
# For one cohort: number of cells, acres, and mean and median TCC in each year.
summarize_cohort <- function(idx, name) {
  i <- ok(idx)
  n <- sum(i)
  data.frame(
    cohort = name,
    n_pixels = n,
    acres = n * px_acres,
    mean_tcc_2020 = if (n) mean(c20[i]) else NA_real_,
    mean_tcc_2025 = if (n) mean(c25[i]) else NA_real_,
    median_tcc_2020 = if (n) median(c20[i]) else NA_real_,
    median_tcc_2025 = if (n) median(c25[i]) else NA_real_,
    stringsAsFactors = FALSE
  )
}

overall <- bind_rows(
  summarize_cohort(changed, "Owner type changed"),
  summarize_cohort(stable, "Stable owner type"),
  summarize_cohort(both_cfp, "All CFP both years"),
  summarize_cohort(exited, "Left CFP by 2026"),
  summarize_cohort(entered, "Entered CFP by 2026")
)
# Change in mean canopy, in percentage points; negative means canopy was lost.
overall$delta_pp <- overall$mean_tcc_2025 - overall$mean_tcc_2020

# ---- Break the "owner type changed" cohort down by specific transition ----
# One row per changed cell: its 2020 and 2026 owner type and its TCC each year.
trans_df <- data.frame(
  from_code = v20[ok(changed)],
  to_code = v26[ok(changed)],
  tcc_2020 = c20[ok(changed)],
  tcc_2025 = c25[ok(changed)]
)
trans_df$from <- unname(code_to_label[as.character(trans_df$from_code)])
trans_df$to <- unname(code_to_label[as.character(trans_df$to_code)])

# Average TCC for each "from -> to" owner-type pair, largest acreage first.
by_flow <- trans_df |>
  group_by(from, to) |>
  summarise(
    n_pixels = n(),
    acres = n_pixels * px_acres,
    mean_tcc_2020 = mean(tcc_2020),
    mean_tcc_2025 = mean(tcc_2025),
    .groups = "drop"
  ) |>
  mutate(delta_pp = mean_tcc_2025 - mean_tcc_2020) |>
  arrange(desc(acres))

# ---- Save and print results ----
write.csv(overall, file.path(csv_dir, "cfp_tcc_by_owner_change_cohort.csv"), row.names = FALSE)
write.csv(by_flow, file.path(csv_dir, "cfp_tcc_by_owner_type_flow.csv"), row.names = FALSE)

# Round selected columns to 2 decimals for easier reading on screen.
round_df <- function(d, cols) {
  for (col in cols) d[[col]] <- round(d[[col]], 2)
  as.data.frame(d)
}

cat("=== Mean TCC 2020 vs 2025 by ownership cohort ===\n")
cat("CFP owner rasters: >=20 ac parcel filter. TCC 2025 = latest (no 2026 TCC).\n\n")
print(round_df(
  overall,
  c("acres", "mean_tcc_2020", "mean_tcc_2025", "median_tcc_2020", "median_tcc_2025", "delta_pp")
), row.names = FALSE)

cat("\n=== Top owner-type transitions among type-changed pixels ===\n")
print(utils::head(round_df(
  by_flow,
  c("acres", "mean_tcc_2020", "mean_tcc_2025", "delta_pp")
), 15), row.names = FALSE)

cat("\nWrote output_csvs/cfp_tcc_by_owner_change_cohort.csv\n")
cat("Wrote output_csvs/cfp_tcc_by_owner_type_flow.csv\n")
