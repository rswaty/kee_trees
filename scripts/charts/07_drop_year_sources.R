# Bar chart: acres where canopy dropped, by year and source (USFS canopy vs Hansen), per footprint.
# Data: output_csvs/footprint_drop_year_by_source.csv (scripts/17).
if (!exists("CHART_SETUP_LOADED")) source(file.path(if (dir.exists("scripts")) "." else "../..", "scripts/charts/00_setup.R"))

SOURCE_COLS <- c("USFS canopy fell 15+ points" = "#2b8cbe", "Hansen forest cleared" = "#e6550d")

d <- csv("footprint_drop_year_by_source.csv") |>
  mutate(
    footprint = factor(paste0(footprint, " (", ifelse(footprint == "Heartlands", "2012–2022", "2015–2025"), ")"),
                       levels = c("Heartlands (2012–2022)", "Songbird (2015–2025)")),
    source = recode(source, "TCC drop >= 15 points" = names(SOURCE_COLS)[1], "Hansen loss" = names(SOURCE_COLS)[2])
  )

p <- ggplot(d, aes(year, acres, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  facet_wrap(~footprint, scales = "free_x") +
  scale_fill_manual(values = SOURCE_COLS) +
  scale_x_continuous(breaks = function(l) seq(ceiling(l[1]), floor(l[2]), 2)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Acres where canopy dropped, by year and source", x = NULL, y = "Acres", fill = NULL) +
  theme_ppt() +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"), panel.grid.major.x = element_blank())

save_chart(p, "07_drop_year_sources")
