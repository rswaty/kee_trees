# "Heartlands and Songbird" tab: where and when the canopy dropped inside the two sale
# footprints, from the USFS canopy record and Hansen, with the ownership timeline and
# year-by-year comparisons against other owner groups.
# Data: output_spatial/app/*.geojson and output_csvs/app_sale_dates.csv from
# scripts/20_app_data.R, plus the by-year CSVs from scripts 11, 12, 16 and 17.
# Sourced by tiles_app/app.R; defines footprints_ui and footprints_server().

fp_app_dir <- file.path(proj_root, "output_spatial", "app")
has_footprints <- all(file.exists(
  file.path(fp_app_dir, c("drop_year.geojson", "footprints.geojson")),
  file.path(processed, c("app_sale_dates.csv", "footprint_drop_year_by_source.csv",
                         "footprint_drop_year_agreement.csv", "cfp_sale_tcc_mean_by_year.csv",
                         "cfp_sale_footprint_hansen_by_year.csv", "cfp_sale_landfire_annual_by_zone.csv"))
))

FP_YEARS <- list(Heartlands = 2012:2022, Songbird = 2015:2025)
FP_SOURCES <- c(
  "USFS canopy fell 15+ points" = "tcc",
  "Hansen forest cleared" = "hansen",
  "Both sources, within a year" = "both"
)
FP_SOURCE_LABEL <- setNames(names(FP_SOURCES), FP_SOURCES)
FP_ZONE <- c(Heartlands = "Heartlands (TRG to TNC)", Songbird = "Songbird (TRG to Songbird, 2026)")
FP_LINE_GROUPS <- c(
  "TRG/Verdant retained" = "TRG → Verdant",
  "Non-institutional kept" = "Other private → kept"
)
FP_LINE_COLS <- c(
  "TRG → Verdant → The Nature Conservancy (Heartlands)" = "#1b9e77",
  "TRG → Verdant → American Songbird (2026)" = "#d95f02",
  "TRG → Verdant" = "#7570b3",
  "Other private → kept" = "#b8a07e"
)
FP_GROUP_LAB <- c(
  Heartlands = "TRG → Verdant → The Nature Conservancy (Heartlands)",
  Songbird = "TRG → Verdant → American Songbird (2026)"
)

if (has_footprints) {
  fp_drop <- sf::st_read(file.path(fp_app_dir, "drop_year.geojson"), quiet = TRUE)
  fp_drop$source_label <- unname(FP_SOURCE_LABEL[fp_drop$source])
  fp_bounds <- sf::st_read(file.path(fp_app_dir, "footprints.geojson"), quiet = TRUE)
  fp_dates <- read.csv(file.path(processed, "app_sale_dates.csv"), stringsAsFactors = FALSE)
  fp_by_year <- read.csv(file.path(processed, "footprint_drop_year_by_source.csv"), stringsAsFactors = FALSE)
  fp_agree <- read.csv(file.path(processed, "footprint_drop_year_agreement.csv"), stringsAsFactors = FALSE)
  fp_metrics <- list(
    canopy = read.csv(file.path(processed, "cfp_sale_tcc_mean_by_year.csv"), stringsAsFactors = FALSE) |>
      transmute(zone, year, value = mean_tcc),
    hansen = read.csv(file.path(processed, "cfp_sale_footprint_hansen_by_year.csv"), stringsAsFactors = FALSE) |>
      transmute(zone, year, value = pct_of_zone),
    landfire = read.csv(file.path(processed, "cfp_sale_landfire_annual_by_zone.csv"), stringsAsFactors = FALSE) |>
      transmute(zone, year, value = pct_of_zone)
  )
}

FP_METRIC_LABEL <- c(
  canopy = "Average canopy cover (%)",
  hansen = "Share of land cleared (Hansen, %)",
  landfire = "Share of land disturbed (LANDFIRE, %)"
)

fp_date_text <- function(owner) {
  d <- fp_dates[fp_dates$owner == owner, ]
  s <- as.Date(d$date_start)
  e <- as.Date(d$date_end)
  if (format(s, "%Y-%m") == format(e, "%Y-%m")) return(format(s, "%b %Y"))
  if (format(s, "%Y") == format(e, "%Y")) return(paste0(format(s, "%b"), "–", format(e, "%b %Y")))
  paste0(format(s, "%b %Y"), "–", format(e, "%b %Y"))
}

fp_filter <- function(footprint, source, years) {
  list(
    "all",
    list("==", list("get", "footprint"), footprint),
    list("==", list("get", "source"), source),
    list(">=", list("to-number", list("get", "year")), as.integer(years[1])),
    list("<=", list("to-number", list("get", "year")), as.integer(years[2]))
  )
}

footprints_ui <- if (has_footprints) {
  page_sidebar(
    title = NULL,
    fillable = TRUE,
    sidebar = sidebar(
      width = 360,
      radioButtons("fp_name", "Sale footprint",
                   choices = c("Keweenaw Heartlands (2012–2022)" = "Heartlands",
                               "American Songbird (2015–2025)" = "Songbird")),
      radioButtons("fp_source", "Year the canopy dropped, from", choices = FP_SOURCES),
      sliderInput("fp_years", "Years shown", min = 2012, max = 2022, value = c(2012, 2022),
                  step = 1, sep = "", width = "100%"),
      uiOutput("fp_legend"),
      selectInput("fp_basemap", "Basemap", choices = basemap_choices, selected = "positron",
                  width = "100%"),
      tags$hr(),
      tags$div(class = "fw-semibold mb-1", "Ownership"),
      uiOutput("fp_timeline"),
      tags$hr(),
      tags$div(
        class = "small text-muted",
        tags$p(class = "mb-1", tags$strong("USFS canopy: "),
               "first year a 30 m cell's canopy fell 15+ points below its highest value in the previous 3 years."),
        tags$p(class = "mb-1", tags$strong("Hansen: "),
               "year Landsat detected stand-replacing loss (trees across the 30 m cell cleared). ",
               "Yes/no; record ends 2024."),
        tags$p(class = "mb-0", tags$strong("Both: "),
               "cells both sources flagged within a year of each other, coloured by the canopy year.")
      )
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        full_screen = TRUE,
        card_header(textOutput("fp_map_title", inline = TRUE)),
        maplibreOutput("fp_map", height = "100%")
      ),
      layout_columns(
        col_widths = 12,
        row_heights = c("auto", 1, 1),
        uiOutput("fp_stats"),
        card(
          full_screen = TRUE,
          card_header("Acres where the canopy dropped, by year"),
          plotlyOutput("fp_acres_chart", height = "100%")
        ),
        card(
          full_screen = TRUE,
          card_header("Compared with other owners"),
          radioButtons("fp_metric", NULL, inline = TRUE,
                       choices = c("Canopy cover" = "canopy", "Hansen cleared" = "hansen",
                                   "LANDFIRE disturbed" = "landfire")),
          plotlyOutput("fp_compare_chart", height = "100%")
        )
      )
    )
  )
} else {
  page_fillable(card(
    card_header("Heartlands and Songbird"),
    card_body(tags$p("App data not found. From the project root run:"),
              tags$pre("Rscript scripts/20_app_data.R"))
  ))
}

footprints_server <- function(input, output, session) {
  if (!has_footprints) return(invisible())

  yrs <- reactive(FP_YEARS[[input$fp_name]])

  observeEvent(input$fp_name, {
    y <- yrs()
    updateSliderInput(session, "fp_years", min = min(y), max = max(y), value = range(y))
  })

  # The slider lags one flush behind a footprint switch; clamp to the footprint's window.
  years_sel <- reactive({
    y <- yrs()
    v <- input$fp_years
    v <- c(max(min(y), v[1]), min(max(y), v[2]))
    if (v[1] > v[2]) range(y) else v
  })

  output$fp_map_title <- renderText({
    paste0(input$fp_name, " Acres: Canopy Cover Loss, ", min(yrs()), "–", max(yrs()),
           " (", FP_SOURCE_LABEL[[input$fp_source]], ")")
  })

  output$fp_legend <- renderUI({
    y <- yrs()
    cols <- viridisLite::turbo(length(y), begin = 0.05, end = 0.95)
    sel <- years_sel()
    tags$div(
      class = "small mb-3",
      tags$div(class = "fw-semibold mb-1", "Year canopy dropped"),
      tags$div(
        style = paste0("display:grid;grid-auto-flow:column;grid-template-rows:repeat(",
                       ceiling(length(y) / 2), ", auto);column-gap:1rem;"),
        lapply(seq_along(y), function(i) {
          tags$div(
            style = paste0("opacity:", if (y[i] >= sel[1] && y[i] <= sel[2]) 1 else 0.3, ";"),
            legend_row(cols[i], y[i])
          )
        })
      )
    )
  })

  output$fp_timeline <- renderUI({
    verdant <- fp_date_text("Verdant")
    rows <- if (input$fp_name == "Heartlands") {
      list(c("TRG", paste("until", verdant)),
           c("Verdant", paste0(verdant, " to ", fp_date_text("The Nature Conservancy"))),
           c("The Nature Conservancy", paste("since", fp_date_text("The Nature Conservancy"))))
    } else {
      list(c("TRG", paste("until", verdant)),
           c("Verdant", paste(verdant, "to 2026")),
           c("American Songbird", "2026 (after the 2026 state list)"))
    }
    tags$table(
      class = "table table-sm small mb-0",
      tags$tbody(lapply(rows, function(r) tags$tr(tags$td(tags$strong(r[1])), tags$td(r[2]))))
    )
  })

  output$fp_stats <- renderUI({
    a <- fp_agree[fp_agree$footprint == input$fp_name, ]
    box <- function(title, value, sub) {
      tags$div(
        class = "card mb-0",
        tags$div(
          class = "card-body py-2 px-3",
          tags$div(class = "small text-muted", title),
          tags$div(class = "fs-4 fw-semibold", style = "white-space:nowrap;", value),
          tags$div(class = "small text-muted", sub)
        )
      )
    }
    layout_columns(
      col_widths = c(6, 6),
      gap = "0.5rem",
      box("USFS canopy flagged", paste(format(round(a$acres_tcc), big.mark = ","), "ac"),
          paste0(round(100 * a$acres_tcc / a$footprint_acres, 1), "% of ",
                 format(round(a$footprint_acres), big.mark = ","), " ac")),
      box("Hansen flagged", paste(format(round(a$acres_hansen), big.mark = ","), "ac"),
          paste0(round(100 * a$acres_hansen / a$footprint_acres, 1), "% of the footprint"))
    )
  })

  output$fp_map <- renderMaplibre({
    maplibre(style = basemap_style_url("positron"), bounds = fp_bounds[fp_bounds$footprint == "Heartlands", ],
             scrollZoom = TRUE) |>
      add_source(id = "fp-bounds", data = fp_bounds) |>
      add_source(id = "fp-drop", data = fp_drop) |>
      add_fill_layer(id = "fp_area", source = "fp-bounds", fill_color = "#9ca3af", fill_opacity = 0.18) |>
      add_fill_layer(
        id = "fp_drop", source = "fp-drop",
        fill_color = get_column("color"), fill_opacity = 0.9,
        popup = concat(
          "<strong>", get_column("year"), "</strong><br>", get_column("source_label"), "<br>",
          number_format("acres", maximum_fraction_digits = 0, use_grouping = TRUE),
          " acres in this footprint that year"
        )
      ) |>
      add_line_layer(id = "fp_outline", source = "fp-bounds", line_color = "#374151", line_width = 1.2) |>
      add_navigation_control() |>
      add_scale_control(unit = "imperial")
  })

  apply_fp_view <- function() {
    proxy <- maplibre_proxy("fp_map")
    set_filter(proxy, layer_id = "fp_drop", filter = fp_filter(input$fp_name, input$fp_source, years_sel()))
    set_filter(proxy, layer_id = "fp_area", filter = list("==", list("get", "footprint"), input$fp_name))
    set_filter(proxy, layer_id = "fp_outline", filter = list("==", list("get", "footprint"), input$fp_name))
  }

  observeEvent(list(input$fp_name, input$fp_source, years_sel()), apply_fp_view())
  observeEvent(input$fp_name, {
    fit_bounds(maplibre_proxy("fp_map"), fp_bounds[fp_bounds$footprint == input$fp_name, ],
               animate = TRUE, padding = 40)
  }, ignoreInit = TRUE)
  observeEvent(input$fp_basemap, {
    set_style(maplibre_proxy("fp_map"), style = basemap_style_url(input$fp_basemap), preserve_layers = TRUE)
    apply_fp_view()
  }, ignoreInit = TRUE)

  output$fp_acres_chart <- renderPlotly({
    sel <- years_sel()
    d <- fp_by_year[fp_by_year$footprint == input$fp_name, ] |>
      mutate(source = if_else(source == "Hansen loss", "Hansen forest cleared", "USFS canopy fell 15+ points"),
             in_range = year >= sel[1] & year <= sel[2])
    plot_ly(d, x = ~year, y = ~acres, color = ~source, type = "bar",
            colors = c("Hansen forest cleared" = "#e6550d", "USFS canopy fell 15+ points" = "#2b8cbe"),
            marker = list(opacity = ifelse(d$in_range, 1, 0.3)),
            hovertemplate = "%{x}: %{y:,.0f} acres<extra>%{fullData.name}</extra>") |>
      layout(barmode = "group", xaxis = list(title = "", dtick = 1),
             yaxis = list(title = "Acres", tickformat = ",d"),
             legend = list(orientation = "h", x = 0, y = 1.15),
             margin = list(t = 10, b = 30, l = 55, r = 10)) |>
      config(displayModeBar = FALSE)
  })

  output$fp_compare_chart <- renderPlotly({
    zones <- c(FP_ZONE[[input$fp_name]], names(FP_LINE_GROUPS))
    labs <- c(FP_GROUP_LAB[[input$fp_name]], unname(FP_LINE_GROUPS))
    d <- fp_metrics[[input$fp_metric]] |>
      filter(zone %in% zones) |>
      mutate(group = factor(labs[match(zone, zones)], levels = labs))
    plot_ly(d, x = ~year, y = ~value, color = ~group, colors = FP_LINE_COLS,
            type = "scatter", mode = "lines+markers",
            hovertemplate = "%{x}: %{y:.1f}%<extra>%{fullData.name}</extra>") |>
      layout(xaxis = list(title = "", dtick = 2), yaxis = list(title = FP_METRIC_LABEL[[input$fp_metric]]),
             legend = list(orientation = "h", x = 0, y = -0.15, font = list(size = 10)),
             margin = list(t = 10, b = 30, l = 55, r = 10)) |>
      config(displayModeBar = FALSE)
  })
}
