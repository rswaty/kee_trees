# Mean TCC 2020 vs 2025 on CFP pixels by owner-type change cohort.
# Run from project root: Rscript scripts/06_cfp_tcc_by_owner_change.R
# Does not touch Shiny.

Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
})

root <- if (dir.exists("data/processed") && dir.exists("data/TCC_Houghton_Keweenaw")) {
  "."
} else {
  stop("Run from the kee_trees project root.")
}

out_dir <- file.path(root, "data/processed")
tcc_dir <- file.path(root, "data/TCC_Houghton_Keweenaw")

r20 <- rast(file.path(out_dir, "cfp_owner_2020.tif"))
r26 <- rast(file.path(out_dir, "cfp_owner_2026.tif"))
lut <- read.csv(file.path(out_dir, "cfp_owner_labels.csv"), stringsAsFactors = FALSE)
code_to_label <- setNames(lut$label, lut$code)

tcc20 <- subst(rast(file.path(tcc_dir, "HK_TCC_2020.tif")), c(254, 255), NA)
tcc25 <- subst(rast(file.path(tcc_dir, "HK_TCC_2025.tif")), c(254, 255), NA)
tcc20 <- extend(crop(tcc20, r20), r20)
tcc25 <- extend(crop(tcc25, r20), r20)

px_acres <- prod(res(r20)) / 4046.8564224

v20 <- values(r20, mat = FALSE)
v26 <- values(r26, mat = FALSE)
c20 <- values(tcc20, mat = FALSE)
c25 <- values(tcc25, mat = FALSE)

both_cfp <- !is.na(v20) & !is.na(v26)
changed <- both_cfp & (v20 != v26)
stable <- both_cfp & (v20 == v26)
exited <- !is.na(v20) & is.na(v26)
entered <- is.na(v20) & !is.na(v26)

ok <- function(idx) idx & !is.na(c20) & !is.na(c25)

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
overall$delta_pp <- overall$mean_tcc_2025 - overall$mean_tcc_2020

trans_df <- data.frame(
  from_code = v20[ok(changed)],
  to_code = v26[ok(changed)],
  tcc_2020 = c20[ok(changed)],
  tcc_2025 = c25[ok(changed)]
)
trans_df$from <- unname(code_to_label[as.character(trans_df$from_code)])
trans_df$to <- unname(code_to_label[as.character(trans_df$to_code)])

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

write.csv(overall, file.path(out_dir, "cfp_tcc_by_owner_change_cohort.csv"), row.names = FALSE)
write.csv(by_flow, file.path(out_dir, "cfp_tcc_by_owner_type_flow.csv"), row.names = FALSE)

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

cat("\nWrote data/processed/cfp_tcc_by_owner_change_cohort.csv\n")
cat("Wrote data/processed/cfp_tcc_by_owner_type_flow.csv\n")
