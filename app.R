## Deployment entrypoint wrapper.
## ShinyApps often bundles relative to the app root; this wrapper ensures the
## dashboard code runs with the project root as working directory.

setwd(normalizePath(dirname(sys.frame(1)$ofile)))
source("dashboard/app.R")

