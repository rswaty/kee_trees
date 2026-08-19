## Deployment entrypoint wrapper.
## In shinyapps.io, working directory is the app bundle root.
## Source the dashboard app from there.

if (file.exists("dashboard/app.R")) {
  source("dashboard/app.R")
} else {
  stop("Cannot find dashboard/app.R from app root.")
}

