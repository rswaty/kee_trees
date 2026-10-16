# make_all.R — build every figure and a slide deck with one figure per slide.
#
# Run from the kee_trees folder or from this folder:
#   Rscript maps_scripts_for_presentation/make_all.R
#
# Writes figures/*.png and presentation_figures.pptx (16:9, one figure per slide).
# The first run downloads the locator-map layers once (needs internet); later runs use data/.

source(if (file.exists("00_setup.R")) "00_setup.R" else "maps_scripts_for_presentation/00_setup.R")

scripts <- sort(list.files(HERE, pattern = "^[0-9]{2}_.*\\.R$", full.names = TRUE))
scripts <- scripts[basename(scripts) != "00_setup.R"]

for (s in scripts) {
  message("\n==== ", basename(s), " ====")
  source(s, local = new.env(parent = globalenv()))
}

# ---- slide deck ---------------------------------------------------------------

widescreen_template <- function() {
  src <- system.file("template", "template.pptx", package = "officer")
  tmp <- tempfile("pptx_")
  utils::unzip(src, exdir = tmp)
  f <- file.path(tmp, "ppt", "presentation.xml")
  x <- readLines(f, warn = FALSE, encoding = "UTF-8")
  writeLines(gsub("<p:sldSz[^>]*/>", '<p:sldSz cx="12192000" cy="6858000"/>', x), f, useBytes = TRUE)
  out <- tempfile(fileext = ".pptx")
  zip::zip(out, files = list.files(tmp, recursive = TRUE, all.files = TRUE), root = tmp)
  out
}

pngs <- sort(list.files(FIG_DIR, pattern = "\\.png$", full.names = TRUE))
doc <- officer::read_pptx(widescreen_template())
ss <- officer::slide_size(doc)
for (f in pngs) {
  doc <- officer::add_slide(doc, layout = "Blank")
  doc <- officer::ph_with(doc, officer::external_img(f, width = ss$width, height = ss$height),
                          location = officer::ph_location(left = 0, top = 0,
                                                          width = ss$width, height = ss$height))
}
deck <- file.path(HERE, "presentation_figures.pptx")
print(doc, target = deck)
message("\nWrote ", length(pngs), " slides to ", deck)
