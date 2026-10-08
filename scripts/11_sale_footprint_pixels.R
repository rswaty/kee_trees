# Pixel-level canopy change and Hansen loss timing by sale footprint / owner zone.
# Zones (2020 CFP parcels >= MIN_ACRES, plus Songbird and Heartlands footprints
# painted on top): TRG/Verdant retained, TRG other sale / left CFP, Lyme, MWF,
# non-institutional kept, non-institutional sold / left, Heartlands, Songbird.
# Run from project root: Rscript scripts/11_sale_footprint_pixels.R
# Does not touch Shiny.

Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(terra)
  library(sf)
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

cfp_dir <- file.path(root, "data/cfp_data")
tcc_dir <- file.path(root, "data/TCC_Houghton_Keweenaw")
out_dir <- file.path(root, "data/processed")
fig_dir <- file.path(out_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

songbird_path <- file.path(
  root,
  "data/American Songbird (17,315 acres)/American Songbird (17,315 acres).shp"
)
heartlands_path <- file.path(root, "data/Keweenaw Heartlands/Keweenaw Heartlands.shp")

MIN_ACRES <- 20
DROP_PP <- -20
HANSEN_YEARS <- 2015:2024

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

norm_name <- function(s) {
  s <- tolower(as.character(s))
  s[is.na(s)] <- ""
  s <- gsub("[^a-z0-9 ]", " ", s)
  s <- gsub("\\b(llc|inc|ltd|co|company|corp|corporation|limited)\\b", "", s)
  gsub("\\s+", " ", trimws(s))
}

read_tcc <- function(y) {
  subst(rast(file.path(tcc_dir, sprintf("HK_TCC_%d.tif", y))), c(254, 255), NA)
}

message("Loading TCC, Hansen, CFP, footprints...")
tcc15 <- read_tcc(2015)
tcc20 <- read_tcc(2020)
tcc25 <- read_tcc(2025)
hansen <- rast(file.path(out_dir, "hansen_lossyear.tif"))
stopifnot(compareGeom(tcc20, hansen))
px_acres <- prod(res(tcc20)) / 4046.8564224

p20 <- st_read(file.path(cfp_dir, "cfp_hk_2020.shp"), quiet = TRUE) |>
  st_make_valid() |>
  st_transform(crs(tcc20))
p26 <- st_read(file.path(cfp_dir, "cfp_hk_2026.shp"), quiet = TRUE) |>
  st_drop_geometry() |>
  transmute(parid = as.character(parid), name_2026 = trimws(as.character(FullLegalN))) |>
  distinct(parid, .keep_all = TRUE)

p20 <- p20 |>
  mutate(
    parid = as.character(parid),
    acres = as.numeric(acres),
    name_2020 = trimws(as.character(search_nam))
  ) |>
  filter(!is.na(acres), acres >= MIN_ACRES) |>
  left_join(p26, by = "parid") |>
  mutate(
    firm = case_when(
      grepl("TRG\\s*THRESHOLD|VERDANT", name_2020, ignore.case = TRUE) ~ "TRG",
      grepl("\\bLYME\\b", name_2020, ignore.case = TRUE) ~ "Lyme",
      grepl("MOLPUS|\\bMWF\\b", name_2020, ignore.case = TRUE) ~ "MWF",
      TRUE ~ NA_character_
    ),
    same_name = norm_name(name_2020) == norm_name(name_2026) & nzchar(norm_name(name_2020)),
    trg_retained = firm %in% "TRG" &
      grepl("verdant|trg|threshold", name_2026, ignore.case = TRUE),
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

zr <- rasterize(vect(p20), tcc20, field = "zone_code", background = 0)
kh <- vect(st_transform(st_read(heartlands_path, quiet = TRUE), crs(tcc20)))
sb <- vect(st_transform(st_read(songbird_path, quiet = TRUE), crs(tcc20)))
zr <- mask(zr, rasterize(kh, tcc20), inverse = TRUE, updatevalue = ZONES[["Heartlands (TRG to TNC)"]])
zr <- mask(zr, rasterize(sb, tcc20), inverse = TRUE, updatevalue = ZONES[["Songbird (TRG to Songbird, 2026)"]])
names(zr) <- "zone_code"
writeRaster(
  zr, file.path(out_dir, "cfp_sale_zones.tif"),
  overwrite = TRUE, wopt = list(datatype = "INT1U", gdal = c("COMPRESS=DEFLATE"))
)
write.csv(
  data.frame(zone_code = unname(ZONES), zone = names(ZONES)),
  file.path(out_dir, "cfp_sale_zone_labels.csv"),
  row.names = FALSE
)

z <- values(zr, mat = FALSE)
v15 <- values(tcc15, mat = FALSE)
v20 <- values(tcc20, mat = FALSE)
v25 <- values(tcc25, mat = FALSE)
ly <- values(hansen, mat = FALSE)

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

zone_path <- file.path(out_dir, "cfp_sale_footprint_zone_summary.csv")
year_path <- file.path(out_dir, "cfp_sale_footprint_hansen_by_year.csv")
write.csv(zone_summary, zone_path, row.names = FALSE)
write.csv(year_profile, year_path, row.names = FALSE)
message("Wrote ", zone_path)
message("Wrote ", year_path)

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

message("\n=== Zone summary ===")
print(zone_summary, row.names = FALSE, digits = 3)
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
