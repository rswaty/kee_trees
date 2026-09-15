# Deploy the tile explorer to shinyapps.io (same app / same URL).
# Run from project root: source("deploy.R")
#
# IMPORTANT: use this script (appFiles list below). Deploying the whole
# project folder pulls in scripts that depend on freestiler/pmtiles from
# walkerke.r-universe.dev — shinyapps.io cannot parse that repository and
# fails with: "Unknown repository for package source".

if (!dir.exists("data/processed")) {
  stop("Run from the kee_trees project root.")
}

tile_files <- c(
  "www/tiles/hansen_loss.pmtiles",
  "www/tiles/tcc_decline.pmtiles",
  "www/tiles/landfire_fdist.pmtiles",
  "www/tiles/cfp_owner_2020.pmtiles",
  "www/tiles/cfp_owner_2026.pmtiles",
  "www/tiles/cfp_owner_change.pmtiles"
)
if (!all(file.exists(tile_files))) {
  stop(
    "Missing www/tiles PMTiles. Need hansen_loss, tcc_decline, landfire_fdist, ",
    "and cfp_owner_{2020,2026,change}.\n",
    "Rebuild with scripts/02_*, 03_*, 04_*, and 05_cfp_owner_tiles.R"
  )
}

app_files <- c(
  "app.R",
  "tiles_app/app.R",
  "data/processed/loss_by_county_year.csv",
  "data/processed/tcc_by_county_year.csv",
  "data/processed/landfire_fdist_by_agent.csv",
  "data/processed/cfp_owner_sankey.csv",
  "data/processed/cfp_name_sankey.csv",
  "data/processed/cfp_owner_sankey_summary.csv",
  "data/processed/cfp_name_sankey_summary.csv",
  "data/processed/cfp_min_parcel_acres.csv",
  tile_files
)

missing <- app_files[!file.exists(app_files)]
if (length(missing) > 0) {
  stop("Missing deploy files:\n- ", paste(missing, collapse = "\n- "))
}

# Fail fast if dependency capture would include r-universe-only packages.
deps <- rsconnect::appDependencies(appDir = ".", appFiles = app_files)
repo_col <- if ("Repository" %in% names(deps)) deps$Repository else rep(NA_character_, nrow(deps))
src_col <- if ("Source" %in% names(deps)) deps$Source else rep(NA_character_, nrow(deps))
blob <- tolower(paste(src_col, repo_col))
bad <- deps[grepl("r-universe|walkerke", blob), , drop = FALSE]
if (nrow(bad) > 0) {
  stop(
    "Deploy blocked: these packages are not from CRAN/GitHub and shinyapps.io ",
    "will reject the manifest:\n",
    paste0("  - ", bad$Package, " (", paste(bad$Source, bad$Repository), ")"),
    "\nRemove them from the app bundle or reinstall from CRAN/GitHub.\n",
    "(Do not deploy scripts/ that library(freestiler) / library(pmtiles).)"
  )
}

cat("Bundling", length(app_files), "files (PMTiles same-origin):\n")
cat(paste0("  ", app_files, collapse = "\n"), "\n\n")
cat("Runtime packages:", nrow(deps), "(CRAN/GitHub only)\n\n")

rsconnect::deployApp(
  appDir = ".",
  appFiles = app_files,
  appName = "kee_tree_cover"
)
