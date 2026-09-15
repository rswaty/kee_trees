# Commercial Forest Program (CFP) 2020→2026 ownership / name change on the TCC grid.
# Run from project root: Rscript scripts/04_cfp_ownership_change.R
#
# Writes Sankey CSVs (GIS acres from 30 m pixels) for several minimum parcel-acre
# thresholds (attribute acres). Default analysis excludes parcels < 20 acres.
# Enter/exit pixels: "non-CFP in 2020" / "non-CFP in 2026".

Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(terra)
  library(sf)
  library(dplyr)
})

terraOptions(progress = 1)

root <- if (dir.exists("data/cfp_data") && dir.exists("data/TCC_Houghton_Keweenaw")) {
  "."
} else {
  stop("Run this script from the kee_trees project root.")
}

cfp_dir <- file.path(root, "data/cfp_data")
tcc_dir <- file.path(root, "data/TCC_Houghton_Keweenaw")
out_dir <- file.path(root, "data/processed")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

gdal_opts <- c("COMPRESS=DEFLATE", "ZLEVEL=9", "TILED=YES")
EXIT_LABEL <- "non-CFP in 2026"
ENTER_LABEL <- "non-CFP in 2020"
# Slider choices in Shiny; default / published rasters use 20.
MIN_PARCEL_ACRES <- c(20, 40, 60, 80, 100, 160, 320)
DEFAULT_MIN_ACRES <- 20

message("Loading TCC template...")
tcc_files <- sort(list.files(tcc_dir, pattern = "^HK_TCC_[0-9]{4}\\.tif$", full.names = TRUE))
stopifnot(length(tcc_files) > 0)
template <- rast(tcc_files[[1]])
px_acres <- prod(res(template)) / 4046.8564224

message("Reading CFP shapefiles...")
p20_all <- st_read(file.path(cfp_dir, "cfp_hk_2020.shp"), quiet = TRUE) |>
  st_make_valid()
p26_all <- st_read(file.path(cfp_dir, "cfp_hk_2026.shp"), quiet = TRUE) |>
  st_make_valid()

p20_all <- st_transform(p20_all, crs(template))
p26_all <- st_transform(p26_all, crs(template))

p20_all$parcel_acres <- as.numeric(p20_all$acres)
p26_all$parcel_acres <- as.numeric(p26_all$Acres)
p20_all$owner_label <- trimws(as.character(p20_all$owner_type))
p26_all$owner_label <- trimws(as.character(p26_all$OwnerTypeT))
p20_all$name_label <- trimws(as.character(p20_all$search_nam))
p26_all$name_label <- trimws(as.character(p26_all$FullLegalN))

fix_missing <- function(x, fallback) {
  x[is.na(x) | !nzchar(x)] <- fallback
  x
}
p20_all$owner_label <- fix_missing(p20_all$owner_label, "(missing owner type)")
p26_all$owner_label <- fix_missing(p26_all$owner_label, "(missing owner type)")
p20_all$name_label <- fix_missing(p20_all$name_label, "(missing name)")
p26_all$name_label <- fix_missing(p26_all$name_label, "(missing name)")

own_labels <- sort(unique(c(p20_all$owner_label, p26_all$owner_label)))
name_labels <- sort(unique(c(p20_all$name_label, p26_all$name_label)))
p20_all$own_code <- match(p20_all$owner_label, own_labels)
p26_all$own_code <- match(p26_all$owner_label, own_labels)
p20_all$name_code <- match(p20_all$name_label, name_labels)
p26_all$name_code <- match(p26_all$name_label, name_labels)

build_flows <- function(r20, r26, labels, kind) {
  code_enter_left <- length(labels) + 1L
  code_exit_right <- length(labels) + 2L
  label_lookup <- c(
    setNames(labels, seq_along(labels)),
    setNames(ENTER_LABEL, code_enter_left),
    setNames(EXIT_LABEL, code_exit_right)
  )

  v20 <- values(r20, mat = FALSE)
  v26 <- values(r26, mat = FALSE)
  only26 <- is.na(v20) & !is.na(v26)
  only20 <- !is.na(v20) & is.na(v26)
  both_na <- is.na(v20) & is.na(v26)
  v20[only26] <- code_enter_left
  v26[only20] <- code_exit_right

  tab <- table(v20[!both_na], v26[!both_na], useNA = "no")
  df <- as.data.frame(tab, stringsAsFactors = FALSE)
  names(df) <- c("from_code", "to_code", "n_pixels")
  df$from_code <- as.integer(as.character(df$from_code))
  df$to_code <- as.integer(as.character(df$to_code))
  df <- df[df$n_pixels > 0, , drop = FALSE]
  df$from <- unname(label_lookup[as.character(df$from_code)])
  df$to <- unname(label_lookup[as.character(df$to_code)])
  df$acres <- df$n_pixels * px_acres
  df$kind <- kind
  df$changed <- df$from != df$to
  df <- df[order(-df$acres), , drop = FALSE]
  rownames(df) <- NULL
  df[, c("kind", "from", "to", "n_pixels", "acres", "changed")]
}

rasterize_codes <- function(sf_obj, codes, template) {
  if (nrow(sf_obj) == 0) {
    return(rast(template) * NA_real_)
  }
  v <- vect(sf_obj)
  v$code <- as.integer(codes)
  rasterize(v, template, field = "code", touches = FALSE)
}

summarize_owner_flows <- function(flows) {
  data.frame(
    metric = c(
      "cfp_acres_2020",
      "cfp_acres_2026",
      "stable_owner_acres",
      "changed_owner_acres",
      "exited_acres",
      "entered_acres"
    ),
    acres = c(
      sum(flows$acres[flows$from != ENTER_LABEL]),
      sum(flows$acres[flows$to != EXIT_LABEL]),
      sum(flows$acres[!flows$changed]),
      sum(flows$acres[flows$changed &
        flows$from != ENTER_LABEL &
        flows$to != EXIT_LABEL]),
      sum(flows$acres[flows$to == EXIT_LABEL]),
      sum(flows$acres[flows$from == ENTER_LABEL])
    ),
    stringsAsFactors = FALSE
  )
}

summarize_name_flows <- function(flows) {
  data.frame(
    metric = c(
      "stable_name_acres",
      "changed_name_acres",
      "exited_acres",
      "entered_acres"
    ),
    acres = c(
      sum(flows$acres[!flows$changed]),
      sum(flows$acres[flows$changed &
        flows$from != ENTER_LABEL &
        flows$to != EXIT_LABEL]),
      sum(flows$acres[flows$to == EXIT_LABEL]),
      sum(flows$acres[flows$from == ENTER_LABEL])
    ),
    stringsAsFactors = FALSE
  )
}

owner_flows_all <- list()
name_flows_all <- list()
owner_summary_all <- list()
name_summary_all <- list()
r_own20_default <- NULL
r_own26_default <- NULL
r_nm20_default <- NULL
r_nm26_default <- NULL

for (min_ac in MIN_PARCEL_ACRES) {
  message(sprintf("=== min parcel acres >= %.0f ===", min_ac))
  p20 <- p20_all[!is.na(p20_all$parcel_acres) & p20_all$parcel_acres >= min_ac, ]
  p26 <- p26_all[!is.na(p26_all$parcel_acres) & p26_all$parcel_acres >= min_ac, ]
  message(sprintf(
    "  parcels kept: 2020=%d / %d, 2026=%d / %d",
    nrow(p20), nrow(p20_all), nrow(p26), nrow(p26_all)
  ))

  r_own20 <- rasterize_codes(p20, p20$own_code, template)
  r_own26 <- rasterize_codes(p26, p26$own_code, template)
  r_nm20 <- rasterize_codes(p20, p20$name_code, template)
  r_nm26 <- rasterize_codes(p26, p26$name_code, template)

  own_flows <- build_flows(r_own20, r_own26, own_labels, "owner_type")
  nm_flows <- build_flows(r_nm20, r_nm26, name_labels, "legal_name")
  own_flows$min_parcel_acres <- min_ac
  nm_flows$min_parcel_acres <- min_ac

  own_sum <- summarize_owner_flows(own_flows)
  own_sum$min_parcel_acres <- min_ac
  nm_sum <- summarize_name_flows(nm_flows)
  nm_sum$min_parcel_acres <- min_ac

  owner_flows_all[[as.character(min_ac)]] <- own_flows
  name_flows_all[[as.character(min_ac)]] <- nm_flows
  owner_summary_all[[as.character(min_ac)]] <- own_sum
  name_summary_all[[as.character(min_ac)]] <- nm_sum

  if (identical(as.numeric(min_ac), as.numeric(DEFAULT_MIN_ACRES))) {
    r_own20_default <- r_own20
    r_own26_default <- r_own26
    r_nm20_default <- r_nm20
    r_nm26_default <- r_nm26
    names(r_own20_default) <- "cfp_owner_2020"
    names(r_own26_default) <- "cfp_owner_2026"
    names(r_nm20_default) <- "cfp_name_2020"
    names(r_nm26_default) <- "cfp_name_2026"
  }
}

owner_flows <- dplyr::bind_rows(owner_flows_all)
name_flows <- dplyr::bind_rows(name_flows_all)
owner_summary <- dplyr::bind_rows(owner_summary_all)
name_summary <- dplyr::bind_rows(name_summary_all)

owner_lut <- data.frame(code = seq_along(own_labels), label = own_labels, stringsAsFactors = FALSE)
name_lut <- data.frame(code = seq_along(name_labels), label = name_labels, stringsAsFactors = FALSE)

message("Writing CSVs...")
write.csv(owner_flows, file.path(out_dir, "cfp_owner_sankey.csv"), row.names = FALSE)
write.csv(name_flows, file.path(out_dir, "cfp_name_sankey.csv"), row.names = FALSE)
write.csv(owner_summary, file.path(out_dir, "cfp_owner_sankey_summary.csv"), row.names = FALSE)
write.csv(name_summary, file.path(out_dir, "cfp_name_sankey_summary.csv"), row.names = FALSE)
write.csv(owner_lut, file.path(out_dir, "cfp_owner_labels.csv"), row.names = FALSE)
write.csv(name_lut, file.path(out_dir, "cfp_name_labels.csv"), row.names = FALSE)
write.csv(
  data.frame(
    min_parcel_acres = MIN_PARCEL_ACRES,
    default = MIN_PARCEL_ACRES == DEFAULT_MIN_ACRES
  ),
  file.path(out_dir, "cfp_min_parcel_acres.csv"),
  row.names = FALSE
)

message("Writing class rasters at default min parcel acres = ", DEFAULT_MIN_ACRES, "...")
writeRaster(
  r_own20_default, file.path(out_dir, "cfp_owner_2020.tif"),
  overwrite = TRUE, wopt = list(datatype = "INT2S", gdal = gdal_opts)
)
writeRaster(
  r_own26_default, file.path(out_dir, "cfp_owner_2026.tif"),
  overwrite = TRUE, wopt = list(datatype = "INT2S", gdal = gdal_opts)
)
writeRaster(
  r_nm20_default, file.path(out_dir, "cfp_name_2020.tif"),
  overwrite = TRUE, wopt = list(datatype = "INT2S", gdal = gdal_opts)
)
writeRaster(
  r_nm26_default, file.path(out_dir, "cfp_name_2026.tif"),
  overwrite = TRUE, wopt = list(datatype = "INT2S", gdal = gdal_opts)
)

message("Done.")
def_own <- owner_summary[owner_summary$min_parcel_acres == DEFAULT_MIN_ACRES, ]
message(sprintf(
  "Default (>=%d ac) owner: exited %.0f, entered %.0f, changed %.0f",
  DEFAULT_MIN_ACRES,
  def_own$acres[def_own$metric == "exited_acres"],
  def_own$acres[def_own$metric == "entered_acres"],
  def_own$acres[def_own$metric == "changed_owner_acres"]
))
print(utils::head(name_flows[name_flows$min_parcel_acres == DEFAULT_MIN_ACRES, ], 12))
