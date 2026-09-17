# Year profiles: Hansen loss by year + mean TCC 2010-2025 by CFP ownership cohort.
# Run from project root: Rscript scripts/08_cfp_year_profiles.R
# Does not touch Shiny.

Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(tidyr)
})

has_ggplot <- requireNamespace("ggplot2", quietly = TRUE)
if (has_ggplot) {
  suppressPackageStartupMessages(library(ggplot2))
}

root <- if (dir.exists("data/processed") && dir.exists("data/TCC_Houghton_Keweenaw")) {
  "."
} else {
  stop("Run from the kee_trees project root.")
}

out_dir <- file.path(root, "data/processed")
fig_dir <- file.path(out_dir, "figures")
tcc_dir <- file.path(root, "data/TCC_Houghton_Keweenaw")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

r20 <- rast(file.path(out_dir, "cfp_owner_2020.tif"))
r26 <- rast(file.path(out_dir, "cfp_owner_2026.tif"))
lut <- read.csv(file.path(out_dir, "cfp_owner_labels.csv"), stringsAsFactors = FALSE)
code_to_label <- setNames(lut$label, lut$code)
fi_code <- lut$code[lut$label == "Forest Industry"]
ob_code <- lut$code[lut$label == "Other Business"]
stopifnot(length(fi_code) == 1L, length(ob_code) == 1L)

hansen <- extend(crop(rast(file.path(out_dir, "hansen_lossyear.tif")), r20), r20)
px_acres <- prod(res(r20)) / 4046.8564224

v20 <- values(r20, mat = FALSE)
v26 <- values(r26, mat = FALSE)
h <- values(hansen, mat = FALSE)
h_year <- ifelse(!is.na(h) & h > 0, 2000L + as.integer(h), NA_integer_)

both_cfp <- !is.na(v20) & !is.na(v26)
changed <- both_cfp & (v20 != v26)
stable <- both_cfp & (v20 == v26)
fi_to_ob <- both_cfp & (v20 == fi_code) & (v26 == ob_code)
stable_fi <- both_cfp & (v20 == fi_code) & (v26 == fi_code)

cohort_defs <- list(
  "Owner type changed" = changed,
  "Stable owner type" = stable,
  "FI → Other Business" = fi_to_ob,
  "Stable Forest Industry" = stable_fi
)

cohort_acres <- vapply(cohort_defs, function(idx) sum(idx) * px_acres, numeric(1))

# --- Hansen acres + % of cohort by loss year ---
hansen_rows <- list()
for (nm in names(cohort_defs)) {
  idx <- cohort_defs[[nm]]
  hy <- h_year[idx]
  hy <- hy[!is.na(hy)]
  if (!length(hy)) {
    hansen_rows[[nm]] <- data.frame(
      cohort = nm, year = integer(), n_pixels = integer(),
      acres = numeric(), pct_of_cohort = numeric(),
      stringsAsFactors = FALSE
    )
    next
  }
  tab <- as.data.frame(table(hy), stringsAsFactors = FALSE)
  names(tab) <- c("year", "n_pixels")
  tab$year <- as.integer(as.character(tab$year))
  tab$n_pixels <- as.integer(tab$n_pixels)
  tab$acres <- tab$n_pixels * px_acres
  tab$pct_of_cohort <- 100 * tab$acres / cohort_acres[[nm]]
  tab$cohort <- nm
  hansen_rows[[nm]] <- tab[, c("cohort", "year", "n_pixels", "acres", "pct_of_cohort")]
}
hansen_by_year <- bind_rows(hansen_rows) |>
  arrange(cohort, year)

# Fill missing years 2001-2024 with 0 for cleaner plots
years_all <- 2001:2024
hansen_by_year_full <- tidyr::expand_grid(
  cohort = names(cohort_defs),
  year = years_all
) |>
  left_join(hansen_by_year, by = c("cohort", "year")) |>
  mutate(
    n_pixels = tidyr::replace_na(n_pixels, 0L),
    acres = tidyr::replace_na(acres, 0),
    pct_of_cohort = tidyr::replace_na(pct_of_cohort, 0)
  )

# --- Mean annual TCC 2010-2025 by cohort ---
tcc_files <- sort(list.files(
  tcc_dir, pattern = "^HK_TCC_[0-9]{4}\\.tif$", full.names = TRUE
))
tcc_years <- as.integer(sub(".*HK_TCC_([0-9]{4})\\.tif$", "\\1", basename(tcc_files)))
stopifnot(length(tcc_files) > 0)

tcc_rows <- list()
for (i in seq_along(tcc_files)) {
  yr <- tcc_years[[i]]
  message("TCC ", yr, "...")
  r <- subst(rast(tcc_files[[i]]), c(254, 255), NA)
  r <- extend(crop(r, r20), r20)
  vals <- values(r, mat = FALSE)
  for (nm in names(cohort_defs)) {
    idx <- cohort_defs[[nm]] & !is.na(vals)
    n <- sum(idx)
    tcc_rows[[length(tcc_rows) + 1L]] <- data.frame(
      cohort = nm,
      year = yr,
      n_pixels = n,
      acres = n * px_acres,
      mean_tcc = if (n) mean(vals[idx]) else NA_real_,
      stringsAsFactors = FALSE
    )
  }
}
tcc_by_year <- bind_rows(tcc_rows) |>
  arrange(cohort, year)

# Cohort size summary
cohort_summary <- data.frame(
  cohort = names(cohort_defs),
  acres = as.numeric(cohort_acres),
  n_pixels = vapply(cohort_defs, sum, integer(1)),
  stringsAsFactors = FALSE
)

write.csv(cohort_summary, file.path(out_dir, "cfp_year_profile_cohort_acres.csv"), row.names = FALSE)
write.csv(hansen_by_year_full, file.path(out_dir, "cfp_hansen_loss_by_year_cohort.csv"), row.names = FALSE)
write.csv(tcc_by_year, file.path(out_dir, "cfp_mean_tcc_by_year_cohort.csv"), row.names = FALSE)

cat("=== Cohort sizes ===\n")
print(cohort_summary |> mutate(acres = round(acres, 1)), row.names = FALSE)

cat("\n=== Hansen: peak years by % of cohort acres ===\n")
peaks <- hansen_by_year_full |>
  group_by(cohort) |>
  slice_max(order_by = pct_of_cohort, n = 5, with_ties = FALSE) |>
  ungroup()
print(as.data.frame(peaks |> mutate(
  acres = round(acres, 1), pct_of_cohort = round(pct_of_cohort, 3)
)), row.names = FALSE)

cat("\n=== Hansen window totals (% of cohort) ===\n")
windows <- hansen_by_year_full |>
  group_by(cohort) |>
  summarise(
    pct_2010_2014 = sum(pct_of_cohort[year >= 2010 & year <= 2014]),
    pct_2015_2020 = sum(pct_of_cohort[year >= 2015 & year <= 2020]),
    pct_2021_2024 = sum(pct_of_cohort[year >= 2021 & year <= 2024]),
    pct_any_2001_2024 = sum(pct_of_cohort),
    .groups = "drop"
  )
print(as.data.frame(windows |> mutate(across(where(is.numeric), ~ round(.x, 2)))), row.names = FALSE)

cat("\n=== Mean TCC selected years ===\n")
tcc_wide <- tcc_by_year |>
  filter(year %in% c(2010, 2015, 2020, 2025)) |>
  select(cohort, year, mean_tcc) |>
  tidyr::pivot_wider(names_from = year, values_from = mean_tcc, names_prefix = "tcc_")
tcc_wide$delta_2015_2020 <- tcc_wide$tcc_2020 - tcc_wide$tcc_2015
tcc_wide$delta_2020_2025 <- tcc_wide$tcc_2025 - tcc_wide$tcc_2020
print(as.data.frame(tcc_wide |> mutate(across(where(is.numeric), ~ round(.x, 2)))), row.names = FALSE)

if (has_ggplot) {
  # Pair A: all changed vs stable
  # Pair B: FI→OB vs stable FI
  plot_hansen <- function(cohorts, title, outfile) {
    d <- hansen_by_year_full |> filter(cohort %in% cohorts, year >= 2010)
    d$cohort <- factor(d$cohort, levels = cohorts)
    g <- ggplot(d, aes(x = year, y = pct_of_cohort, color = cohort)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 1.6) +
      scale_x_continuous(breaks = seq(2010, 2024, by = 2)) +
      labs(
        title = title,
        subtitle = "Hansen stand-replacing loss; % of each cohort's acres",
        x = NULL, y = "% of cohort acres", color = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
    ggsave(outfile, g, width = 9, height = 5, dpi = 120)
    message("Wrote ", outfile)
  }

  plot_tcc <- function(cohorts, title, outfile) {
    d <- tcc_by_year |> filter(cohort %in% cohorts)
    d$cohort <- factor(d$cohort, levels = cohorts)
    g <- ggplot(d, aes(x = year, y = mean_tcc, color = cohort)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 1.6) +
      scale_x_continuous(breaks = seq(2010, 2025, by = 5)) +
      labs(
        title = title,
        subtitle = "Mean NLCD/USFS TCC on cohort pixels",
        x = NULL, y = "Mean TCC (%)", color = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")
    ggsave(outfile, g, width = 9, height = 5, dpi = 120)
    message("Wrote ", outfile)
  }

  plot_hansen(
    c("Owner type changed", "Stable owner type"),
    "Hansen loss by year: type-changed vs stable",
    file.path(fig_dir, "cfp_hansen_by_year_changed_vs_stable.png")
  )
  plot_hansen(
    c("FI → Other Business", "Stable Forest Industry"),
    "Hansen loss by year: FI→Other Business vs stable FI",
    file.path(fig_dir, "cfp_hansen_by_year_fi_to_ob_vs_stable_fi.png")
  )
  plot_tcc(
    c("Owner type changed", "Stable owner type"),
    "Mean TCC by year: type-changed vs stable",
    file.path(fig_dir, "cfp_tcc_by_year_changed_vs_stable.png")
  )
  plot_tcc(
    c("FI → Other Business", "Stable Forest Industry"),
    "Mean TCC by year: FI→Other Business vs stable FI",
    file.path(fig_dir, "cfp_tcc_by_year_fi_to_ob_vs_stable_fi.png")
  )
} else {
  message("ggplot2 not installed; CSVs written, PNGs skipped.")
}

cat("\nWrote CSVs under data/processed/cfp_*_by_year_cohort.csv\n")
if (has_ggplot) cat("Wrote PNGs under data/processed/figures/\n")
