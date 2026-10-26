# make_all.R — build every figure.
#
# Run from the kee_trees folder or from this folder:
#   Rscript maps_scripts_for_presentation/make_all.R
#
# Writes figures/*.png (16:9, ready to paste onto a slide).
# The first run downloads the locator-map layers once (needs internet); later runs use data/.

# Load the shared settings and helpers once for all scripts.
source(if (file.exists("00_setup.R")) "00_setup.R" else "maps_scripts_for_presentation/00_setup.R")

# Find the numbered figure scripts (01_, 02_, ...) in this folder, in number order,
# leaving out the setup file. Scripts in alternatives/ are not included.
scripts <- sort(list.files(HERE, pattern = "^[0-9]{2}_.*\\.R$", full.names = TRUE))
scripts <- scripts[basename(scripts) != "00_setup.R"]

# Run each script in its own fresh workspace, so variables from one script can't
# affect the next. The shared settings from 00_setup.R are still visible to all.
for (s in scripts) {
  message("\n==== ", basename(s), " ====")
  source(s, local = new.env(parent = globalenv()))
}

# Report how many PNG figures are now in the figures folder.
message("\nWrote ", length(list.files(FIG_DIR, pattern = "\\.png$")), " figures to ", FIG_DIR)
