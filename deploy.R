# Deploy to shinyapps.io with only the files the dashboard needs.
# Run from project root: source("deploy.R")

if (!dir.exists("data/processed")) {
  stop("Run from the kee_trees project root.")
}

app_files <- c(
  "app.R",
  "dashboard/app.R",
  "data/processed/loss_by_county_year.csv",
  "data/processed/tcc_by_county_year.csv",
  "data/processed/loss_by_year.gpkg",
  "data/processed/counties.gpkg"
)

missing <- app_files[!file.exists(app_files)]
if (length(missing) > 0) {
  stop("Missing deploy files:\n- ", paste(missing, collapse = "\n- "))
}

cat("Bundling", length(app_files), "files:\n")
cat(paste0("  ", app_files, collapse = "\n"), "\n\n")

rsconnect::deployApp(
  appDir = ".",
  appFiles = app_files
)
