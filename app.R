## Shiny app entrypoint (root) — MapLibre + PMTiles tile explorer.
## Geometry loads from Cloudflare R2; only lightweight CSVs ship with the app.
## To refresh processed stats: Rscript scripts/01_harmonize.R
## Old Leaflet in-memory app remains under dashboard/ for local comparison.

if (!file.exists("tiles_app/app.R")) {
  stop("Cannot find tiles_app/app.R from app root.")
}

message("Starting Keweenaw canopy tile explorer...")
Sys.setenv(KEE_DEPLOY_FROM_ROOT = "true")
source("tiles_app/app.R", local = FALSE)
shinyApp(ui, server)
