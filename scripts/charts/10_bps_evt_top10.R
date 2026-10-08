# Bar charts: top 10 Biophysical Settings (LANDFIRE 2020) and Existing Vegetation Types
# (LANDFIRE 2024), all land in Houghton and Keweenaw counties (mainland, no open water).
# Data: output_csvs/hk_bps_acres.csv, hk_evt_acres.csv (scripts/18).
if (!exists("CHART_SETUP_LOADED")) source(file.path(if (dir.exists("scripts")) "." else "../..", "scripts/charts/00_setup.R"))

TOP_N <- 10
WRAP <- 45   # characters per line in the vegetation names

top_bar <- function(d, name_col, fill_col, title, subtitle) {
  d <- head(d, TOP_N) |> mutate(name = reorder(.data[[name_col]], acres))
  ggplot(d, aes(acres, name, fill = .data[[fill_col]])) +
    geom_col(width = 0.7) +
    geom_text(aes(label = paste0(round(pct, 1), "%")), hjust = -0.1, size = BASE_SIZE / 3.6, family = FONT_FAMILY) +
    scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
    scale_y_discrete(labels = function(x) stringr::str_wrap(x, WRAP)) +
    scale_fill_manual(values = PHYS_COLS) +
    labs(title = title, subtitle = subtitle, x = "Acres", y = NULL, fill = NULL) +
    theme_ppt(BASE_SIZE - 3) +
    theme(legend.position = "bottom", panel.grid.major.y = element_blank())
}

save_chart(
  top_bar(csv("hk_bps_acres.csv"), "BPS_NAME", "GROUPVEG",
          "What the land would naturally support",
          "Top 10 LANDFIRE Biophysical Settings (2020), percent of land area"),
  "10a_bps_top10", height = 6.6
)
save_chart(
  top_bar(csv("hk_evt_acres.csv"), "EVT_NAME", "EVT_PHYS",
          "What's there now",
          "Top 10 LANDFIRE Existing Vegetation Types (2024), percent of land area"),
  "10b_evt_top10", height = 6.6
)
