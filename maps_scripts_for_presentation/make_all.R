# make_all.R — build every figure.
#
# Run from the kee_trees folder or from this folder:
#   Rscript maps_scripts_for_presentation/make_all.R
#
# Writes figures/*.png (16:9, ready to paste onto a slide).
# The first run downloads the locator-map layers once (needs internet); later runs use data/.

source(if (file.exists("00_setup.R")) "00_setup.R" else "maps_scripts_for_presentation/00_setup.R")

scripts <- sort(list.files(HERE, pattern = "^[0-9]{2}_.*\\.R$", full.names = TRUE))
scripts <- scripts[basename(scripts) != "00_setup.R"]

for (s in scripts) {
  message("\n==== ", basename(s), " ====")
  source(s, local = new.env(parent = globalenv()))
}

message("\nWrote ", length(list.files(FIG_DIR, pattern = "\\.png$")), " figures to ", FIG_DIR)
