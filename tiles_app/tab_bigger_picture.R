# "The bigger picture" tab: all land in Houghton and Keweenaw counties (mainland, open
# water excluded). What the land would naturally support (LANDFIRE 2020 BpS), what grows
# there now (LANDFIRE 2024 EVT), and historical vs. current disturbance per year.
# Data: output_csvs/hk_*.csv from scripts/18_bps_evt_disturbance_regimes.R and
# scripts/21_structure_changing_proxies.R.
# Sourced by tiles_app/app.R; defines bigger_picture_ui and bigger_picture_server().

bp_files <- c("hk_bps_acres.csv", "hk_evt_acres.csv", "hk_disturbance_historical_vs_current.csv",
              "hk_current_disturbance_by_year.csv", "hk_disturbance_totals.csv",
              "hk_historical_disturbance_by_bps.csv", "hk_structure_changing_by_year.csv",
              "hk_structure_changing_summary.csv")
has_bigger_picture <- all(file.exists(file.path(processed, bp_files)))

BP_HIST_ALL <- "Historical: all disturbances"
BP_HIST_CHG <- "Historical: structure-changing only"
BP_CUR_10s <- "Current (LANDFIRE average, 2010–2019)"
BP_CUR_20s <- "Current (LANDFIRE average, 2020–2025)"
BP_PERIODS <- c(BP_HIST_ALL, BP_HIST_CHG, BP_CUR_10s, BP_CUR_20s)
BP_PERIOD_LAB <- c("Historical,<br>all disturbances", "Historical,<br>structure-changing",
                   "Current,<br>2010–2019", "Current,<br>2020–2025")

BP_TYPE_COLS <- c(
  "Fire" = "#e31a1c",
  "Wind, weather or stress" = "#41b6c4",
  "Insects or disease" = "#7570b3",
  "Harvest or mechanical" = "#d95f02",
  "Unknown cause" = "#08519c",
  "Other" = "grey"
)
# Structure-changing measures: low end (LANDFIRE severity is conservative) to high end.
BP_SC_COLS <- c(
  "LANDFIRE high severity" = "#111827",
  "Hansen stand-replacing loss" = "#b45309",
  "Canopy fell 20+ points" = "#059669"
)
BP_SC_LAB <- "Current,<br>structure-changing,<br>2020 onward"
BP_PHYS_COLS <- c(
  "Hardwood" = "#66a61e", "Conifer" = "#1b7837", "Hardwood-Conifer" = "#a6d96a",
  "Conifer-Hardwood" = "#a6d96a", "Riparian" = "#41b6c4", "Agricultural" = "#e6ab02",
  "Developed-Roads" = "#8c8c8c", "Developed" = "#8c8c8c", "Exotic Herbaceous" = "#d95f02",
  "Shrubland" = "#a6761d", "Grassland" = "#fdd49e", "Sparsely Vegetated" = "#d9d9d9"
)

if (has_bigger_picture) {
  bp_read <- function(f) read.csv(file.path(processed, f), stringsAsFactors = FALSE, check.names = FALSE)
  bp_bps <- bp_read("hk_bps_acres.csv")
  bp_evt <- bp_read("hk_evt_acres.csv")
  bp_by_year <- bp_read("hk_current_disturbance_by_year.csv")
  bp_totals <- bp_read("hk_disturbance_totals.csv")
  bp_land <- with(bp_totals[1, ], annual_acres / pct_of_land * 100)
  bp_tot <- function(p, col = "annual_acres") bp_totals[[col]][bp_totals$period == p]

  bp_periods <- bind_rows(
    bp_read("hk_disturbance_historical_vs_current.csv") |>
      filter(period %in% c(BP_HIST_ALL, BP_HIST_CHG)) |>
      select(period, type_group, annual_acres),
    bp_by_year |>
      tidyr::complete(year, type_group, fill = list(annual_acres = 0)) |>
      mutate(period = if_else(year < 2020, BP_CUR_10s, BP_CUR_20s)) |>
      group_by(period, type_group) |>
      summarise(annual_acres = mean(annual_acres), .groups = "drop")
  ) |>
    mutate(pct_of_land = 100 * annual_acres / bp_land)

  bp_sc_year <- bp_read("hk_structure_changing_by_year.csv")
  bp_sc <- bp_read("hk_structure_changing_summary.csv") |> filter(period == "2020 onward")
  bp_sc_lo <- min(bp_sc$annual_acres)
  bp_sc_hi <- max(bp_sc$annual_acres)
  bp_sc_x <- function(a) sprintf("%.1f", a / bp_tot(BP_HIST_CHG))

  bp_bps_table <- bp_read("hk_historical_disturbance_by_bps.csv") |>
    group_by(BPS_NAME) |>
    summarise(
      acres = sum(acres[!duplicated(BPS_MODEL)]),
      chg = sum(annual_acres[class_change]),
      all = sum(annual_acres),
      .groups = "drop"
    ) |>
    transmute(
      `Biophysical Setting` = BPS_NAME,
      Acres = round(acres),
      `% of land` = round(100 * acres / bp_land, 1),
      `Historical structure-changing, %/yr` = round(100 * chg / acres, 2),
      `Historical all disturbances, %/yr` = round(100 * all / acres, 2),
      `Structure-changing return interval, yrs` = ifelse(chg > 0, round(acres / chg), NA)
    ) |>
    arrange(desc(Acres))
}

bp_stat <- function(title, value, sub) {
  tags$div(
    class = "card mb-0 h-100",
    tags$div(
      class = "card-body py-2 px-3",
      tags$div(class = "small text-muted", title),
      tags$div(class = "fs-4 fw-semibold", style = "white-space:nowrap;", value),
      tags$div(class = "small text-muted", sub)
    )
  )
}

bp_fmt <- function(x) format(round(x), big.mark = ",", scientific = FALSE)

bigger_picture_ui <- if (has_bigger_picture) {
  page_fillable(
    fillable = FALSE,
    tags$p(
      class = "text-muted mb-2",
      "All ", bp_fmt(bp_land), " acres of land in Houghton and Keweenaw counties (mainland; open water and ",
      "Isle Royale excluded), not just Commercial Forest land."
    ),
    layout_columns(
      col_widths = c(2, 2, 2, 3, 3),
      gap = "0.5rem",
      bp_stat("Historical, structure-changing", paste(bp_fmt(bp_tot(BP_HIST_CHG)), "ac/yr"),
              paste0(round(bp_tot(BP_HIST_CHG, "pct_of_land"), 2), "% of the land a year")),
      bp_stat("Historical, all disturbances", paste(bp_fmt(bp_tot(BP_HIST_ALL)), "ac/yr"),
              paste0(round(bp_tot(BP_HIST_ALL, "pct_of_land"), 2), "%, including light disturbances")),
      bp_stat("Current, 2010–2019", paste(bp_fmt(bp_tot(BP_CUR_10s)), "ac/yr"),
              paste0(round(bp_tot(BP_CUR_10s, "pct_of_land"), 2), "% of the land a year")),
      bp_stat("Current, 2020–2025", paste(bp_fmt(bp_tot(BP_CUR_20s)), "ac/yr"),
              paste0(round(bp_tot(BP_CUR_20s, "pct_of_land"), 2), "% of the land a year, all disturbances")),
      bp_stat("Current structure-changing, 2020 onward",
              paste0(bp_fmt(bp_sc_lo), "–", bp_fmt(bp_sc_hi), " ac/yr"),
              paste0("about ", bp_sc_x(bp_sc_lo), "–", bp_sc_x(bp_sc_hi), "× the historical structure-changing rate"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        full_screen = TRUE,
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Acres disturbed each year vs. the historical expectation"
        ),
        plotlyOutput("bp_year_chart", height = "380px"),
        tags$p(class = "small text-muted mb-0 px-3 pb-2",
               "Bars: LANDFIRE Annual Disturbance, all severities. Coloured lines: three measures of ",
               "structure-changing disturbance, from LANDFIRE high severity (conservative) to canopy cover ",
               "falling 20+ points (includes heavy partial cuts). Dashed line: historical structure-changing ",
               "rate; dotted line: historical rate including light disturbances (BpS models).")
      ),
      card(
        full_screen = TRUE,
        card_header("Disturbance per year: historical vs. now"),
        plotlyOutput("bp_period_chart", height = "380px"),
        tags$p(class = "small text-muted mb-0 px-3 pb-2",
               "Structure-changing = moves a stand to a different succession class. The last bar spans ",
               "the three current measures (dark = lowest, light = up to the highest). ",
               "Harvest has no historical counterpart; \"unknown cause\" is likely mostly harvest.")
      )
    ),
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Vegetation: what the land would naturally support vs. what grows there now",
        tags$div(style = "width: 220px; margin-bottom: -1rem;",
                 sliderInput("bp_top_n", NULL, min = 5, max = 20, value = 10, step = 1,
                             post = " types", width = "100%"))
      ),
      layout_columns(
        col_widths = c(6, 6),
        tags$div(tags$div(class = "fw-semibold small", "Biophysical Settings (LANDFIRE 2020)"),
                 plotlyOutput("bp_bps_chart", height = "420px")),
        tags$div(tags$div(class = "fw-semibold small", "Existing Vegetation Types (LANDFIRE 2024)"),
                 plotlyOutput("bp_evt_chart", height = "420px"))
      )
    ),
    card(
      card_header("Historical disturbance by Biophysical Setting (BpS models)"),
      DT::DTOutput("bp_bps_table")
    )
  )
} else {
  page_fillable(card(
    card_header("The bigger picture"),
    card_body(tags$p("Data not found. From the project root run:"),
              tags$pre("Rscript scripts/18_bps_evt_disturbance_regimes.R"))
  ))
}

bigger_picture_server <- function(input, output, session) {
  if (!has_bigger_picture) return(invisible())

  type_bars <- function(p, d, x, y, orientation = "v") {
    for (t in names(BP_TYPE_COLS)) {
      dt <- d[d$type_group == t, ]
      if (!nrow(dt)) next
      p <- add_trace(p, data = dt, x = dt[[x]], y = dt[[y]], type = "bar", name = t,
                     marker = list(color = BP_TYPE_COLS[[t]]),
                     hovertemplate = paste0("%{x}<br>", t, ": %{y:,.0f} acres<extra></extra>"))
    }
    p
  }

  output$bp_year_chart <- renderPlotly({
    hist_chg <- bp_tot(BP_HIST_CHG)
    hist_all <- bp_tot(BP_HIST_ALL)
    ref <- function(y, dash) {
      list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = y, y1 = y,
           line = list(color = "#111827", dash = dash, width = 1.2))
    }
    lab <- function(y, text) {
      list(x = 0.01, xref = "paper", y = y, text = text, showarrow = FALSE, xanchor = "left",
           yanchor = "bottom", font = list(size = 11), bgcolor = "rgba(255,255,255,0.8)")
    }
    p <- type_bars(plot_ly(), bp_by_year, "year", "annual_acres")
    for (m in names(BP_SC_COLS)) {
      dm <- bp_sc_year[bp_sc_year$measure == m, ]
      p <- add_trace(p, x = dm$year, y = dm$annual_acres, type = "scatter", mode = "lines+markers", name = m,
                     line = list(color = BP_SC_COLS[[m]], width = 2), marker = list(color = BP_SC_COLS[[m]], size = 5),
                     hovertemplate = paste0("%{x}<br>", m, ": %{y:,.0f} acres<extra></extra>"))
    }
    p |>
      layout(barmode = "stack", xaxis = list(title = "", dtick = 1, tickangle = -45),
             yaxis = list(title = "Acres disturbed", tickformat = ",d"),
             shapes = list(ref(hist_chg, "dash"), ref(hist_all, "dot")),
             annotations = list(lab(hist_chg, paste0("Historical, structure-changing: ", bp_fmt(hist_chg), " ac/yr")),
                                lab(hist_all, paste0("Historical, all: ", bp_fmt(hist_all), " ac/yr"))),
             legend = list(orientation = "h", x = 0, y = -0.25, font = list(size = 10)),
             margin = list(t = 10, b = 40, l = 60, r = 10)) |>
      config(displayModeBar = FALSE)
  })

  output$bp_period_chart <- renderPlotly({
    levs <- c(BP_PERIOD_LAB, BP_SC_LAB)
    d <- bp_periods |>
      mutate(period = factor(BP_PERIOD_LAB[match(period, BP_PERIODS)], levels = levs))
    totals <- d |> group_by(period) |> summarise(a = sum(annual_acres), p = sum(pct_of_land), .groups = "drop")
    sc_x <- factor(BP_SC_LAB, levels = levs)
    bar_labels <- lapply(seq_len(nrow(totals)), function(i) {
      list(x = totals$period[i], y = totals$a[i], yanchor = "bottom", showarrow = FALSE,
           text = paste0(bp_fmt(totals$a[i]), " ac/yr<br>(", round(totals$p[i], 2), "%)"),
           font = list(size = 11))
    })
    sc_label <- list(x = sc_x, y = bp_sc_hi, yanchor = "bottom", showarrow = FALSE, font = list(size = 11),
                     text = paste0(bp_fmt(bp_sc_lo), "–", bp_fmt(bp_sc_hi), " ac/yr<br>(",
                                   bp_sc_x(bp_sc_lo), "–", bp_sc_x(bp_sc_hi), "× historical)"))
    sc_hover <- paste(paste0(bp_sc$measure, ": ", bp_fmt(bp_sc$annual_acres), " ac/yr"), collapse = "<br>")
    type_bars(plot_ly(), d, "period", "annual_acres") |>
      add_trace(x = sc_x, y = bp_sc_lo, type = "bar", name = "Structure-changing, lowest measure",
                marker = list(color = "#4b5563"), hovertext = sc_hover, hoverinfo = "text") |>
      add_trace(x = sc_x, y = bp_sc_hi - bp_sc_lo, type = "bar", name = "Structure-changing, range",
                marker = list(color = "#d1d5db"), hovertext = sc_hover, hoverinfo = "text") |>
      layout(barmode = "stack", xaxis = list(title = ""),
             yaxis = list(title = "Acres disturbed per year", tickformat = ",d",
                          range = c(0, max(totals$a) * 1.2)),
             annotations = c(bar_labels, list(sc_label)),
             legend = list(orientation = "h", x = 0, y = -0.2, font = list(size = 10)),
             margin = list(t = 10, b = 40, l = 60, r = 10)) |>
      config(displayModeBar = FALSE)
  })

  veg_chart <- function(d, name_col, fill_col) {
    d <- head(d, input$bp_top_n)
    d$name <- factor(d[[name_col]], levels = rev(d[[name_col]]))
    cols <- unname(BP_PHYS_COLS[d[[fill_col]]])
    cols[is.na(cols)] <- "#bdbdbd"
    plot_ly(d, x = ~acres, y = ~name, type = "bar", orientation = "h", marker = list(color = cols),
            text = paste0(round(d$pct, 1), "%"), textposition = "outside", cliponaxis = FALSE,
            customdata = d[[fill_col]],
            hovertemplate = "%{y}<br>%{customdata}<br>%{x:,.0f} acres<extra></extra>") |>
      layout(xaxis = list(title = "Acres", tickformat = ",d", range = c(0, max(d$acres) * 1.15)),
             yaxis = list(title = "", tickfont = list(size = 10)),
             margin = list(t = 5, b = 40, l = 10, r = 10)) |>
      config(displayModeBar = FALSE)
  }
  output$bp_bps_chart <- renderPlotly(veg_chart(bp_bps, "BPS_NAME", "GROUPVEG"))
  output$bp_evt_chart <- renderPlotly(veg_chart(bp_evt, "EVT_NAME", "EVT_PHYS"))

  output$bp_bps_table <- DT::renderDT({
    DT::datatable(bp_bps_table, rownames = FALSE, options = list(pageLength = 10, dom = "ftip"),
                  class = "compact stripe") |>
      DT::formatRound("Acres", digits = 0)
  })
}
