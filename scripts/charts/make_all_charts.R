# Run every chart script and also build one combined deck with all charts and tables:
#   output_visuals/powerpoint/all_charts.pptx
# Run from project root: Rscript scripts/charts/make_all_charts.R
source(file.path(if (dir.exists("scripts")) "." else "../..", "scripts/charts/00_setup.R"))
CHARTS <- list()

chart_dir <- file.path(root, "scripts", "charts")
for (f in sort(list.files(chart_dir, pattern = "^[0-9]{2}_.*\\.R$", full.names = TRUE))) {
  if (basename(f) == "00_setup.R") next
  message("== ", basename(f))
  source(f, local = new.env())
}

if (HAVE_OFFICER) {
  doc <- new_deck()
  for (nm in names(CHARTS)) {
    x <- CHARTS[[nm]]
    doc <- if (is.null(x$table)) add_chart_slide(doc, x) else add_table_slide(doc, x$table, x$title)
  }
  out <- file.path(PPT_DIR, "all_charts.pptx")
  print(doc, target = out)
  message("Wrote ", out, " (", length(CHARTS), " slides)")
}
