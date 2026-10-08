# Tables: where TRG's land went, and big companies vs other private owners.
# Written as CSV (paste into PowerPoint) and as one-slide decks with native PowerPoint tables.
# Data: output_csvs/cfp_institutional_exit_buckets.csv, cfp_sellers_by_owner_class.csv (scripts/10),
#       cfp_sale_tcc_mean_by_year.csv (scripts/12).
if (!exists("CHART_SETUP_LOADED")) source(file.path(if (dir.exists("scripts")) "." else "../..", "scripts/charts/00_setup.R"))

buckets <- csv("cfp_institutional_exit_buckets.csv")
by_class <- csv("cfp_sellers_by_owner_class.csv")
tcc_year <- csv("cfp_sale_tcc_mean_by_year.csv")

bk <- function(bucket, col) sum(buckets[[col]][buckets$institutional_firm == "TRG" & buckets$exit_bucket %in% bucket])
tcc_at <- function(z, y) tcc_year$mean_tcc[tcc_year$zone == z & tcc_year$year == y]

trg <- list(
  list(zone = "TRG/Verdant retained", bucket = "rebrand_same_family"),
  list(zone = "Heartlands (TRG to TNC)", bucket = "tnc_heartlands"),
  list(zone = "Songbird (TRG to Songbird, 2026)", bucket = "songbird"),
  list(zone = "TRG other sale / left CFP", bucket = c("other_sale", "left_cfp"))
)
trg_table <- bind_rows(lapply(trg, function(x) {
  acres <- bk(x$bucket, "acres")
  major <- bk(x$bucket, "acres_major_disturbance")
  data.frame(
    `2020 → 2026` = ZONE_LAB[[x$zone]],
    Parcels = fmt_n(bk(x$bucket, "n_parcels")),
    Acres = fmt_n(acres),
    `Canopy 2020` = paste0(fmt_1(tcc_at(x$zone, 2020)), "%"),
    `Canopy 2025` = paste0(fmt_1(tcc_at(x$zone, 2025)), "%"),
    `Acres heavily cut` = paste0(fmt_n(major), " (", fmt_1(100 * major / acres), "%)"),
    check.names = FALSE
  )
}))
save_table(trg_table, "13a_table_trg_destinations", "Where TRG's land went")

cls <- function(oc, col) sum(by_class[[col]][by_class$owner_class == oc])
class_table <- data.frame(
  ` ` = c("Institutional owners", "Other private owners"),
  Acres = fmt_n(c(cls("Institutional", "acres"), cls("Non-institutional", "acres"))),
  `Acres heavily cut` = fmt_n(c(cls("Institutional", "acres_major_disturbance"), cls("Non-institutional", "acres_major_disturbance"))),
  Share = paste0(fmt_1(100 * c(cls("Institutional", "acres_major_disturbance") / cls("Institutional", "acres"),
                               cls("Non-institutional", "acres_major_disturbance") / cls("Non-institutional", "acres"))), "%"),
  check.names = FALSE
)
save_table(class_table, "13b_table_institutional_vs_other", "Big companies vs. other private owners")
