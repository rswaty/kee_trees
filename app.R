## Deployment entrypoint for shinyapps.io (deploy from project root).
## Load ui/server from dashboard/app.R, then start the app here.
## shinyapps.io expects shinyApp() in this file, not only in a sourced script.

if (!file.exists("dashboard/app.R")) {
  stop("Cannot find dashboard/app.R from app root.")
}

Sys.setenv(KEE_DEPLOY_FROM_ROOT = "true")
source("dashboard/app.R", local = FALSE)
shinyApp(ui, server)

