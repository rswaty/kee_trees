# Hansen tree-cover loss and LANDFIRE disturbance rates on Commercial Forest
# Program (CFP) land, grouped by what happened to the owner type 2020 to 2026.
# Run from project root: Rscript scripts/07_cfp_disturbance_by_owner_change.R
# Does not touch Shiny. Mirrors cohorts in scripts/06_cfp_tcc_by_owner_change.R.
# Needs outputs from scripts 01 (Hansen), 03 (LANDFIRE) and 04 (CFP owners).

# Stop the PROJ library from trying to download projection files from the internet.
Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
})

# ---- Folders ----
# All paths are relative to the project folder, so stop early if run from elsewhere.
root <- if (dir.exists("output_csvs") && dir.exists("inputs/cfp")) {
  "."
} else {
  stop("Run from the kee_trees project root.")
}

csv_dir <- file.path(root, "output_csvs")

gis_dir <- file.path(root, "output_spatial")

# ---- Load CFP owner rasters and disturbance layers ----
# Owner rasters hold a number per cell for owner type (parcels of 20 acres or
# more only); the label table turns those numbers back into names.
r20 <- rast(file.path(gis_dir, "cfp_owner_2020.tif"))
r26 <- rast(file.path(gis_dir, "cfp_owner_2026.tif"))
lut <- read.csv(file.path(csv_dir, "cfp_owner_labels.csv"), stringsAsFactors = FALSE)
code_to_label <- setNames(lut$label, lut$code)

hansen <- rast(file.path(gis_dir, "hansen_lossyear.tif"))
lf <- rast(file.path(gis_dir, "landfire_fdist_agent.tif"))
# Align to CFP grid (should already match TCC Albers).
# Trim or pad to exactly the CFP raster's extent so cells line up one-to-one.
hansen <- extend(crop(hansen, r20), r20)
lf <- extend(crop(lf, r20), r20)

# Acres per cell: 30 m x 30 m = 900 square metres, divided by 4046.856 square
# metres per acre (about 0.22 acres).
px_acres <- prod(res(r20)) / 4046.8564224

# Pull every cell's value into simple lists, all in the same cell order.
v20 <- values(r20, mat = FALSE)
v26 <- values(r26, mat = FALSE)
h <- values(hansen, mat = FALSE)
# GFC lossyear: 0 = no loss; 1 = 2001 … 24 = 2024
# (GFC = Hansen Global Forest Change.) Convert to calendar years; no loss = missing.
h_year <- ifelse(!is.na(h) & h > 0, 2000L + as.integer(h), NA_integer_)
lfv <- values(lf, mat = FALSE)
# class_id = agent_id*10 + years_since_bin; agent 1=harvest, 2=mech, 3=insects, 4=fire
# Dividing by 10 and dropping the remainder keeps just the agent number.
lf_agent <- ifelse(is.na(lfv), NA_integer_, as.integer(lfv) %/% 10L)

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

# Names for the LANDFIRE agent numbers, kept for reference.
agent_name <- c(
  "1" = "harvest_remove",
  "2" = "mech_unknown",
  "3" = "insects",
  "4" = "fire"
)

# ---- Summarize disturbance for each cohort ----
# For one cohort, report the share (%) of its cells with Hansen loss overall and
# in three time windows, and with each LANDFIRE disturbance agent.
summarize_cohort <- function(idx, name) {
  i <- which(idx)
  n <- length(i)
  acres <- n * px_acres

  # Hansen loss in this cohort, split into 2010-2014, 2015-2020 and 2021-2024.
  # 2015-2020 is the period before the 2020 ownership snapshot; 2021-2024 is after.
  hy <- h_year[i]
  any_hansen <- !is.na(hy)
  pre <- any_hansen & hy >= 2015L & hy <= 2020L
  mid <- any_hansen & hy >= 2021L & hy <= 2024L
  early <- any_hansen & hy >= 2010L & hy <= 2014L

  # LANDFIRE disturbance in this cohort, by agent.
  la <- lf_agent[i]
  any_lf <- !is.na(la)
  harvest <- any_lf & la == 1L
  mech <- any_lf & la == 2L
  insects <- any_lf & la == 3L
  fire <- any_lf & la == 4L

  # The mean of TRUE/FALSE flags is the fraction TRUE; times 100 gives a percent.
  data.frame(
    cohort = name,
    n_pixels = n,
    acres = acres,
    # Hansen
    hansen_any_acres = sum(any_hansen) * px_acres,
    hansen_any_pct = if (n) 100 * mean(any_hansen) else NA_real_,
    hansen_2010_2014_pct = if (n) 100 * mean(early) else NA_real_,
    hansen_2015_2020_pct = if (n) 100 * mean(pre) else NA_real_,
    hansen_2021_2024_pct = if (n) 100 * mean(mid) else NA_real_,
    # LANDFIRE (2014–2024 product window in this project)
    lf_any_acres = sum(any_lf) * px_acres,
    lf_any_pct = if (n) 100 * mean(any_lf) else NA_real_,
    lf_harvest_pct = if (n) 100 * mean(harvest) else NA_real_,
    lf_mech_pct = if (n) 100 * mean(mech) else NA_real_,
    lf_insects_pct = if (n) 100 * mean(insects) else NA_real_,
    lf_fire_pct = if (n) 100 * mean(fire) else NA_real_,
    stringsAsFactors = FALSE
  )
}

cohorts <- bind_rows(
  summarize_cohort(changed, "Owner type changed"),
  summarize_cohort(stable, "Stable owner type"),
  summarize_cohort(both_cfp, "All CFP both years"),
  summarize_cohort(exited, "Left CFP by 2026"),
  summarize_cohort(entered, "Entered CFP by 2026")
)

# ---- Break the "owner type changed" cohort down by specific transition ----
# Top type transitions: Hansen + LF harvest rates
# (LF = LANDFIRE.) One row per changed cell, then summarized per "from -> to" pair.
i_ch <- which(changed)
flow <- data.frame(
  from = unname(code_to_label[as.character(v20[i_ch])]),
  to = unname(code_to_label[as.character(v26[i_ch])]),
  h_year = h_year[i_ch],
  lf_agent = lf_agent[i_ch]
)
by_flow <- flow |>
  group_by(from, to) |>
  summarise(
    n_pixels = n(),
    acres = n_pixels * px_acres,
    hansen_any_pct = 100 * mean(!is.na(h_year)),
    hansen_2015_2020_pct = 100 * mean(!is.na(h_year) & h_year >= 2015 & h_year <= 2020),
    hansen_2021_2024_pct = 100 * mean(!is.na(h_year) & h_year >= 2021 & h_year <= 2024),
    lf_any_pct = 100 * mean(!is.na(lf_agent)),
    lf_harvest_pct = 100 * mean(!is.na(lf_agent) & lf_agent == 1L),
    .groups = "drop"
  ) |>
  arrange(desc(acres))

# ---- Save and print results ----
write.csv(cohorts, file.path(csv_dir, "cfp_disturbance_by_owner_change_cohort.csv"), row.names = FALSE)
write.csv(by_flow, file.path(csv_dir, "cfp_disturbance_by_owner_type_flow.csv"), row.names = FALSE)

# Round selected columns for easier reading on screen.
rnd <- function(d, cols, digits = 2) {
  for (col in cols) if (col %in% names(d)) d[[col]] <- round(d[[col]], digits)
  as.data.frame(d)
}

cat("=== Hansen + LANDFIRE by ownership cohort ===\n")
cat("CFP >=20 ac filter. Hansen lossyear 1-24 => 2001-2024.\n")
cat("LANDFIRE class_id agent: 1=harvest, 2=other mechanical, 3=insects, 4=fire.\n\n")

show_cols <- c(
  "cohort", "acres",
  "hansen_any_pct", "hansen_2015_2020_pct", "hansen_2021_2024_pct",
  "lf_any_pct", "lf_harvest_pct", "lf_mech_pct", "lf_insects_pct", "lf_fire_pct"
)
print(rnd(cohorts[, show_cols], setdiff(show_cols, "cohort")), row.names = FALSE)

cat("\nAcres with any Hansen / any LANDFIRE:\n")
print(rnd(cohorts[, c("cohort", "acres", "hansen_any_acres", "lf_any_acres")],
          c("acres", "hansen_any_acres", "lf_any_acres")), row.names = FALSE)

cat("\n=== Top owner-type transitions (disturbance rates) ===\n")
print(utils::head(rnd(by_flow, c(
  "acres", "hansen_any_pct", "hansen_2015_2020_pct", "hansen_2021_2024_pct",
  "lf_any_pct", "lf_harvest_pct"
)), 12), row.names = FALSE)

cat("\nWrote output_csvs/cfp_disturbance_by_owner_change_cohort.csv\n")
cat("Wrote output_csvs/cfp_disturbance_by_owner_type_flow.csv\n")
