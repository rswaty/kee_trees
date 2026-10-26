# Institutional sellers vs keepers: major canopy disturbance rates.
# Universe: 2020 Commercial Forest Program (CFP) parcels >= MIN_ACRES owned by the
# institutional (corporate timberland) roster: TRG/Verdant, Lyme, MWF (Molpus family),
# plus Manulife/Hancock if any appear.
# Primary: true transfer (exclude the TRG↔Verdant rebrand) vs keepers.
# Sensitivity: any loose legal-name change (or left CFP) = sold.
# Songbird (2026 sale, after the CFP 2026 snapshot) and Keweenaw Heartlands
# (TRG -> The Nature Conservancy, TNC) are assigned from their footprints, not from
# CFP legal names.
# "Major disturbance" = parcel mean tree canopy cover (TCC) fell more than 20
# percentage points (pp) from 2020 to 2025.
# Needs the output of scripts/09_cfp_parcel_tcc_change.R.
# Run from project root: Rscript scripts/10_institutional_sellers_vs_keepers.R
# Does not touch Shiny.

# Stop the PROJ map-projection library from trying to download grid files.
Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})

# ---- Setup ----
# Paths are relative to the project folder, so stop early if run from elsewhere.
root <- if (dir.exists("inputs/cfp") && dir.exists("output_csvs")) {
  "."
} else {
  stop("Run from the kee_trees project root.")
}
# 2026 owner (renumbered parcels followed by location) and same-owner rules.
source(file.path(root, "scripts/lib_owner_2026.R"))

cfp_dir <- file.path(root, "inputs/cfp")
csv_dir <- file.path(root, "output_csvs")
gis_dir <- file.path(root, "output_spatial")
# Boundaries of the two named sales of former TRG/Verdant land.
songbird_path <- file.path(
  root,
  "inputs/boundaries/american_songbird.shp"
)
heartlands_path <- file.path(
  root,
  "inputs/boundaries/keweenaw_heartlands.shp"
)
# Parcel canopy-change table written by script 09.
rank_path <- file.path(csv_dir, "cfp_parcel_tcc_change_by_owner_type.csv")

# Same 20-acre minimum as script 09; small parcels have noisy canopy averages.
MIN_ACRES <- 20
# A parcel is "heavily cut" (major disturbance) when its mean canopy dropped by
# more than 20 percentage points, 2020 to 2025.
MAJOR_LOSS_PP <- -20
# Label used for parcels with no 2026 owner (left the program).
EXIT_LABEL <- "non-CFP in 2026"
# Parcel counts as inside a footprint when this share of its GIS area overlaps
FOOTPRINT_SHARE <- 0.5

# ---- Helper functions ----
# Light name cleanup: lower case, punctuation removed, and legal suffixes such as
# LLC or Inc dropped, so "Lyme Timber Co LLC" and "LYME TIMBER" compare equal.
norm_name <- function(s) {
  s <- tolower(as.character(s))
  s[is.na(s)] <- ""
  s <- gsub("[^a-z0-9 ]", " ", s)
  s <- gsub("\\b(llc|inc|ltd|co|company|corp|corporation|limited)\\b", "", s)
  gsub("\\s+", " ", trimws(s))
}

# Roster patterns (2020 search_nam). Molpus/Manulife strings rarely appear in HK
# (Houghton and Keweenaw counties); MWF is included as the local Molpus-family
# stand-in (see methods note). Any other owner gets NA (non-institutional).
firm_from_name <- function(name) {
  n <- as.character(name)
  case_when(
    grepl("TRG\\s*THRESHOLD|VERDANT", n, ignore.case = TRUE) ~ "TRG",
    grepl("\\bLYME\\b", n, ignore.case = TRUE) ~ "Lyme",
    grepl("MOLPUS|\\bMWF\\b", n, ignore.case = TRUE) ~ "MWF",
    grepl("MANULIFE|HANCOCK\\s*TIMBER|Hancock Natural", n, ignore.case = TRUE) ~
      "Manulife",
    TRUE ~ NA_character_
  )
}

# TRUE when the 2026 name still belongs to the same company family as the 2020
# owner, even if the legal name changed. Used to spot rebrands that are not
# real sales. Parcels that left the program are never "same family".
same_family <- function(firm, name_2020, name_2026) {
  n0 <- norm_name(name_2020)
  n1 <- norm_name(name_2026)
  if (!nzchar(n1) || identical(name_2026, EXIT_LABEL)) {
    return(FALSE)
  }
  # Identical names after cleanup are clearly the same owner.
  if (identical(n0, n1) && nzchar(n0)) {
    return(TRUE)
  }
  # TRG Threshold ↔ Verdant treated as rebrand / same beneficial owner
  if (identical(firm, "TRG")) {
    trgish <- function(x) {
      grepl("trg|threshold|verdant", x, ignore.case = TRUE)
    }
    return(trgish(name_2020) && trgish(name_2026))
  }
  # Lyme / MWF / Manulife: loose same-name or same firm token in 2026
  if (identical(firm, "Lyme")) {
    return(grepl("\\bLYME\\b", name_2026, ignore.case = TRUE))
  }
  if (identical(firm, "MWF")) {
    return(grepl("\\bMWF\\b|MOLPUS", name_2026, ignore.case = TRUE))
  }
  if (identical(firm, "Manulife")) {
    return(grepl("MANULIFE|HANCOCK", name_2026, ignore.case = TRUE))
  }
  FALSE
}

# Sort each parcel into one "exit bucket" describing what happened to it.
# Rules are checked in order and the first that applies wins:
#   1. songbird: inside the American Songbird footprint, or Songbird named in 2026.
#   2. tnc_heartlands: inside the Keweenaw Heartlands footprint, or TNC named in 2026.
#   3. left_cfp: no 2026 owner found (left the program).
#   4. kept_same_name: 2026 name matches the 2020 owner.
#   5. rebrand_same_family: different legal name but same company family
#      (for example TRG renamed to Verdant).
#   6. other_sale: anything else, i.e. a different owner in 2026.
# Footprints come first so that land in those two sales is labelled by the sale,
# whatever owner (or no owner) the 2026 list shows. Songbird bought after the
# 2026 list was made, so its name is not on that list.
exit_bucket <- function(
    left_cfp,
    name_same_loose,
    rebrand,
    name_2026,
    songbird_overlap,
    songbird_name,
    heartlands_overlap
) {
  case_when(
    songbird_overlap | songbird_name ~ "songbird",
    heartlands_overlap |
      grepl("Nature Conservancy", name_2026, ignore.case = TRUE) ~ "tnc_heartlands",
    left_cfp ~ "left_cfp",
    name_same_loose ~ "kept_same_name",
    rebrand ~ "rebrand_same_family",
    TRUE ~ "other_sale"
  )
}

# Share (0-1) of each parcel's area that falls inside a sale footprint. The
# footprint is merged into one shape so overlapping pieces are not counted twice.
# Parcels that do not touch the footprint get 0.
footprint_share <- function(parcels, path) {
  fp <- st_read(path, quiet = TRUE) |>
    st_make_valid() |>
    st_transform(st_crs(parcels)) |>
    st_union()
  parcel_area <- as.numeric(st_area(parcels))
  ix <- suppressWarnings(st_intersection(parcels["parid"], fp))
  ov <- tapply(as.numeric(st_area(ix)), ix$parid, sum)
  share <- ov[parcels$parid] / parcel_area
  share[is.na(share)] <- 0
  setNames(as.numeric(share), parcels$parid)
}

# ---- Load inputs ----
message("Loading ranked TCC parcels and CFP names...")
if (!file.exists(rank_path)) {
  stop("Missing ", rank_path, " — run scripts/09_cfp_parcel_tcc_change.R first.")
}
rank <- read.csv(rank_path, stringsAsFactors = FALSE) |>
  mutate(parid = as.character(parid))

# st_make_valid() repairs broken polygon outlines so overlays do not fail.
p20 <- st_read(file.path(cfp_dir, "cfp_hk_2020.shp"), quiet = TRUE) |>
  st_make_valid()
p26 <- st_read(file.path(cfp_dir, "cfp_hk_2026.shp"), quiet = TRUE) |>
  st_make_valid()

# 2020 owner name, recorded acres, last name and owner type, one row per parcel.
# Last name and owner type feed the family rule for private individuals.
names20 <- p20 |>
  st_drop_geometry() |>
  transmute(
    parid = as.character(parid),
    name_2020 = trimws(as.character(search_nam)),
    acres_attr = as.numeric(acres),
    lastname_2020 = lastname,
    owner_type_raw_2020 = owner_type
  ) |>
  distinct(parid, .keep_all = TRUE)

# 2026 owner name for each 2020 parcel: by parcel ID, or by map overlap (at least
# 50% covered) for parcels the state renumbered. matched_by_2026 records which.
names26 <- owner_2026_lookup(
  mutate(p20, parid = as.character(parid)),
  st_transform(p26, st_crs(p20))
) |>
  select(parid, name_2026, matched_by_2026 = matched_by)

# ---- Sale footprints ----
# Areas are measured in EPSG:5070 (an equal-area projection for the lower 48
# states) so that overlap shares are true area shares.
message("Overlaying Songbird and Keweenaw Heartlands footprints...")
p20_eq <- p20 |>
  mutate(parid = as.character(parid)) |>
  select(parid) |>
  st_transform(5070)
p20_eq <- p20_eq[!duplicated(p20_eq$parid), ]
sb_share <- footprint_share(p20_eq, songbird_path)
kh_share <- footprint_share(p20_eq, heartlands_path)
# Parcels at least half inside each footprint count as part of that sale.
songbird_parids <- names(sb_share)[sb_share >= FOOTPRINT_SHARE]
heartlands_parids <- names(kh_share)[kh_share >= FOOTPRINT_SHARE]
message(sprintf(
  "Parcels >= %.0f%% inside: Songbird %d, Heartlands %d",
  100 * FOOTPRINT_SHARE,
  length(songbird_parids),
  length(heartlands_parids)
))

# ---- Classify every parcel ----
# Join canopy change, 2020 names and 2026 names, then flag ownership outcomes.
# Columns are built in order, so name_same_loose is computed while a missing
# 2026 name is still NA (which counts as "not the same owner").
inst <- rank |>
  filter(acres_2020 >= MIN_ACRES) |>
  left_join(names20, by = "parid") |>
  left_join(names26, by = "parid") |>
  mutate(
    # Same owner by the shared word-by-word name rules (scripts/lib_owner_2026.R).
    name_same_loose = same_owner_2026(name_2020, name_2026, owner_type_raw_2020, lastname_2020),
    name_2026 = if_else(is.na(name_2026), EXIT_LABEL, name_2026),
    institutional_firm = firm_from_name(name_2020),
    is_institutional = !is.na(institutional_firm),
    left_cfp = name_2026 == EXIT_LABEL,
    songbird_name = grepl("Songbird", name_2026, ignore.case = TRUE),
    songbird_share = unname(sb_share[parid]),
    heartlands_share = unname(kh_share[parid]),
    songbird_overlap = parid %in% songbird_parids,
    heartlands_overlap = parid %in% heartlands_parids,
    # Name changed but still the same company family (e.g. TRG to Verdant).
    rebrand_same_family = !name_same_loose &
      !left_cfp &
      mapply(same_family, institutional_firm, name_2020, name_2026),
    # Primary sale: true transfer (not same-name, not rebrand), or inside a
    # Songbird / Heartlands footprint
    sold_true_transfer = is_institutional &
      (left_cfp | (!name_same_loose & !rebrand_same_family) |
        songbird_overlap | heartlands_overlap),
    # Sensitivity: any loose name change or left CFP
    sold_any_name_change = is_institutional &
      (left_cfp | !name_same_loose | songbird_overlap | heartlands_overlap),
    major_disturbance = delta_pp < MAJOR_LOSS_PP,
    exit_bucket = exit_bucket(
      left_cfp,
      name_same_loose,
      rebrand_same_family,
      name_2026,
      songbird_overlap,
      songbird_name,
      heartlands_overlap
    ),
    # For institutional rows only, primary cohort label
    cohort_primary = case_when(
      !is_institutional ~ NA_character_,
      sold_true_transfer ~ "seller",
      TRUE ~ "keeper"
    ),
    # Same, under the looser sensitivity definition of a sale.
    cohort_sensitivity = case_when(
      !is_institutional ~ NA_character_,
      sold_any_name_change ~ "seller",
      TRUE ~ "keeper"
    )
  )

# Institutional parcels only, grouped by firm, largest first.
inst_only <- inst |>
  filter(is_institutional) |>
  arrange(institutional_firm, desc(acres_2020))

# Columns kept for the parcel-level output table.
parcel_out <- inst_only |>
  select(
    parid,
    county,
    acres_2020,
    acres_2026,
    institutional_firm,
    name_2020,
    name_2026,
    owner_type_2020,
    owner_type_2026,
    mean_tcc_2020,
    mean_tcc_2025,
    delta_pp,
    major_disturbance,
    name_same_loose,
    rebrand_same_family,
    left_cfp,
    songbird_name,
    songbird_share,
    songbird_overlap,
    heartlands_share,
    heartlands_overlap,
    exit_bucket,
    sold_true_transfer,
    sold_any_name_change,
    cohort_primary,
    cohort_sensitivity
  )

# ---- Disturbance rates by cohort ----
# Counts, acres and percent heavily cut for sellers and keepers, either for all
# institutional owners together or firm by firm. Acres are 2020 acres.
summarise_rates <- function(df, cohort_vec, group_firm = FALSE) {
  df <- df |> mutate(cohort = cohort_vec)
  g <- if (group_firm) {
    df |> group_by(institutional_firm, cohort)
  } else {
    df |>
      mutate(institutional_firm = "All institutional") |>
      group_by(institutional_firm, cohort)
  }
  g |>
    summarise(
      n_parcels = n(),
      acres = sum(acres_2020, na.rm = TRUE),
      n_major_disturbance = sum(major_disturbance, na.rm = TRUE),
      acres_major_disturbance = sum(acres_2020[major_disturbance], na.rm = TRUE),
      pct_parcels_major = 100 * mean(major_disturbance, na.rm = TRUE),
      pct_acres_major = 100 * acres_major_disturbance / acres,
      mean_delta_pp = mean(delta_pp, na.rm = TRUE),
      median_delta_pp = median(delta_pp, na.rm = TRUE),
      .groups = "drop"
    )
}

# Four versions: primary and sensitivity sale definitions, each pooled and by firm.
summary_primary_pooled <- summarise_rates(
  inst_only, inst_only$cohort_primary, FALSE
) |>
  mutate(sale_definition = "true_transfer", scope = "pooled")
summary_primary_by_firm <- summarise_rates(
  inst_only, inst_only$cohort_primary, TRUE
) |>
  mutate(sale_definition = "true_transfer", scope = "by_firm")
summary_sens_pooled <- summarise_rates(
  inst_only, inst_only$cohort_sensitivity, FALSE
) |>
  mutate(sale_definition = "any_name_change", scope = "pooled")
summary_sens_by_firm <- summarise_rates(
  inst_only, inst_only$cohort_sensitivity, TRUE
) |>
  mutate(sale_definition = "any_name_change", scope = "by_firm")

# Stack the four into one table.
summary_rates <- bind_rows(
  summary_primary_pooled,
  summary_primary_by_firm,
  summary_sens_pooled,
  summary_sens_by_firm
) |>
  select(
    sale_definition,
    scope,
    institutional_firm,
    cohort,
    everything()
  )

# ---- Exit buckets and other contrasts ----
# Disturbance rates for each firm's exit bucket (songbird, TNC, left, kept, etc.).
bucket_summary <- inst_only |>
  group_by(institutional_firm, exit_bucket) |>
  summarise(
    n_parcels = n(),
    acres = sum(acres_2020, na.rm = TRUE),
    n_major_disturbance = sum(major_disturbance, na.rm = TRUE),
    acres_major_disturbance = sum(acres_2020[major_disturbance], na.rm = TRUE),
    pct_parcels_major = 100 * mean(major_disturbance, na.rm = TRUE),
    pct_acres_major = 100 * acres_major_disturbance / acres,
    mean_delta_pp = mean(delta_pp, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(institutional_firm, desc(acres))

# Secondary contrast from the drawing: sold institutional vs sold non-institutional
# Institutional owners use the primary sale definition. Non-institutional owners
# have no rebrand rule, so for them any name change, exit or footprint overlap
# counts as sold.
owner_class_summary <- inst |>
  mutate(
    owner_class = if_else(is_institutional, "Institutional", "Non-institutional"),
    sold = if_else(
      is_institutional,
      sold_true_transfer,
      left_cfp | !name_same_loose | songbird_overlap | heartlands_overlap
    ),
    cohort = if_else(sold, "seller", "keeper")
  ) |>
  group_by(owner_class, cohort) |>
  summarise(
    n_parcels = n(),
    acres = sum(acres_2020, na.rm = TRUE),
    n_major_disturbance = sum(major_disturbance, na.rm = TRUE),
    acres_major_disturbance = sum(acres_2020[major_disturbance], na.rm = TRUE),
    pct_parcels_major = 100 * mean(major_disturbance, na.rm = TRUE),
    pct_acres_major = 100 * acres_major_disturbance / acres,
    mean_delta_pp = mean(delta_pp, na.rm = TRUE),
    .groups = "drop"
  )

# Named exit spotlight: TNC / Songbird acres from institutional (esp. TRG)
spotlight <- inst_only |>
  filter(exit_bucket %in% c("tnc_heartlands", "songbird", "left_cfp")) |>
  group_by(exit_bucket, institutional_firm) |>
  summarise(
    n_parcels = n(),
    acres = sum(acres_2020, na.rm = TRUE),
    n_major_disturbance = sum(major_disturbance, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Write outputs ----
parcel_path <- file.path(csv_dir, "cfp_institutional_sellers_keepers_parcels.csv")
rates_path <- file.path(csv_dir, "cfp_institutional_sellers_keepers_rates.csv")
bucket_path <- file.path(csv_dir, "cfp_institutional_exit_buckets.csv")
spot_path <- file.path(csv_dir, "cfp_institutional_named_exits.csv")
class_path <- file.path(csv_dir, "cfp_sellers_by_owner_class.csv")

write.csv(parcel_out, parcel_path, row.names = FALSE)
write.csv(summary_rates, rates_path, row.names = FALSE)
write.csv(bucket_summary, bucket_path, row.names = FALSE)
write.csv(spotlight, spot_path, row.names = FALSE)
write.csv(owner_class_summary, class_path, row.names = FALSE)

message("Wrote ", parcel_path, " (", nrow(parcel_out), " institutional parcels)")
message("Wrote ", rates_path)
message("Wrote ", bucket_path)
message("Wrote ", spot_path)

# ---- Console report ----
# Parcel count and 2020 acres per institutional firm.
message("\n=== Universe ===")
message(sprintf(
  "Institutional parcels (>= %d ac): %d (%.0f ac)",
  MIN_ACRES,
  nrow(inst_only),
  sum(inst_only$acres_2020)
))
print(
  inst_only |>
    count(institutional_firm, name = "n") |>
    left_join(
      inst_only |>
        group_by(institutional_firm) |>
        summarise(acres = sum(acres_2020), .groups = "drop"),
      by = "institutional_firm"
    ),
  row.names = FALSE
)

message("\n=== Primary (true transfer): sellers vs keepers ===")
print(summary_primary_pooled, row.names = FALSE, digits = 2)
message("\n=== Primary by firm ===")
print(summary_primary_by_firm, row.names = FALSE, digits = 2)

message("\n=== Sensitivity (any name change) pooled ===")
print(summary_sens_pooled, row.names = FALSE, digits = 2)

message("\n=== Exit buckets ===")
print(as.data.frame(bucket_summary), row.names = FALSE, digits = 3)

message("\n=== Sellers vs keepers by owner class ===")
print(as.data.frame(owner_class_summary), row.names = FALSE, digits = 3)

message("Done.")
