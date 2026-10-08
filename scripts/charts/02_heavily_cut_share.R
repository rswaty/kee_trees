# Bar chart: share of acres heavily cut (parcel canopy fell > 20 points, 2020-2025), by group.
# Data: output_csvs/cfp_institutional_exit_buckets.csv, cfp_sellers_by_owner_class.csv (scripts/10).
if (!exists("CHART_SETUP_LOADED")) source(file.path(if (dir.exists("scripts")) "." else "../..", "scripts/charts/00_setup.R"))

buckets <- csv("cfp_institutional_exit_buckets.csv")
by_class <- csv("cfp_sellers_by_owner_class.csv")

firm_row <- function(group, firm, bucket) {
  b <- buckets[buckets$institutional_firm == firm & buckets$exit_bucket %in% bucket, ]
  data.frame(group = group, acres = sum(b$acres), major = sum(b$acres_major_disturbance))
}
class_row <- function(group, cohort) {
  b <- by_class[by_class$owner_class == "Non-institutional" & by_class$cohort == cohort, ]
  data.frame(group = group, acres = sum(b$acres), major = sum(b$acres_major_disturbance))
}
MW_KEPT <- "Molpus → Molpus (kept)"

bars <- bind_rows(
  firm_row(G$SB, "TRG", "songbird"),
  firm_row(MW_KEPT, "MWF", "kept_same_name"),
  firm_row(G$TV, "TRG", "rebrand_same_family"),
  firm_row(G$TO, "TRG", c("other_sale", "left_cfp")),
  firm_row(G$LY, "Lyme", "kept_same_name"),
  firm_row(G$KH, "TRG", "tnc_heartlands"),
  class_row(G$NS, "seller"),
  class_row(G$NK, "keeper")
) |>
  mutate(
    pct = 100 * major / acres,
    fill = c(GROUP_COLS, setNames(GROUP_COLS[[G$MW]], MW_KEPT))[group],
    group = reorder(factor(group), pct)
  )

p <- ggplot(bars, aes(pct, group, fill = fill)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(fmt_n(major), " of ", fmt_n(acres), " acres")),
            hjust = -0.05, size = BASE_SIZE / 3.6, family = FONT_FAMILY) +
  scale_fill_identity() +
  scale_x_continuous(labels = pct_lab, expand = expansion(mult = c(0, 0.45))) +
  labs(
    title = "Share of acres heavily cut, 2020–2025",
    subtitle = "Acres in parcels whose average canopy fell more than 20 points",
    x = NULL, y = NULL
  ) +
  theme_ppt() +
  theme(panel.grid.major.y = element_blank())

save_chart(p, "02_heavily_cut_share")
