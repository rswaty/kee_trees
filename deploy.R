# Deploy the tile explorer to shinyapps.io (same app / same URL).
# Run from project root: source("deploy.R")
#
# PMTiles are bundled under www/tiles/ and served same-origin so GIS-machine
# browsers that block Cloudflare R2 still get polygons.

if (!dir.exists("data/processed")) {
  stop("Run from the kee_trees project root.")
}

tile_files <- c(
  "www/tiles/hansen_loss.pmtiles",
  "www/tiles/tcc_decline.pmtiles"
)
if (!all(file.exists(tile_files))) {
  stop(
    "Missing www/tiles PMTiles. Copy from data/tiles/ or run:\n",
    "  mkdir -p www/tiles && cp data/tiles/hansen_loss.pmtiles data/tiles/tcc_decline.pmtiles www/tiles/"
  )
}

app_files <- c(
  "app.R",
  "tiles_app/app.R",
  "data/processed/loss_by_county_year.csv",
  "data/processed/tcc_by_county_year.csv",
  tile_files
)

missing <- app_files[!file.exists(app_files)]
if (length(missing) > 0) {
  stop("Missing deploy files:\n- ", paste(missing, collapse = "\n- "))
}

cat("Bundling", length(app_files), "files (PMTiles same-origin):\n")
cat(paste0("  ", app_files, collapse = "\n"), "\n\n")

rsconnect::deployApp(
  appDir = ".",
  appFiles = app_files,
  appName = "kee_tree_cover"
)
