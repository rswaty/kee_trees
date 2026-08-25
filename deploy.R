# Deploy the tile explorer to shinyapps.io (same app / same URL).
# Run from project root: source("deploy.R")
#
# Map polygons are NOT bundled — they load from Cloudflare R2 PMTiles.

if (!dir.exists("data/processed")) {
  stop("Run from the kee_trees project root.")
}

app_files <- c(
  "app.R",
  "tiles_app/app.R",
  "data/processed/loss_by_county_year.csv",
  "data/processed/tcc_by_county_year.csv"
)

missing <- app_files[!file.exists(app_files)]
if (length(missing) > 0) {
  stop("Missing deploy files:\n- ", paste(missing, collapse = "\n- "))
}

cat("Bundling", length(app_files), "files (tiles stay on R2):\n")
cat(paste0("  ", app_files, collapse = "\n"), "\n\n")

rsconnect::deployApp(
  appDir = ".",
  appFiles = app_files,
  appName = "kee_tree_cover"
)
