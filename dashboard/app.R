# Keweenaw + Houghton canopy explorer
# From the project root: shiny::runApp("dashboard")

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(leaflet)
  library(sf)
  library(dplyr)
  library(plotly)
})

processed_candidates <- c(
  file.path(getwd(), "data/processed"),
  file.path(getwd(), "../data/processed")
)
processed <- processed_candidates[dir.exists(processed_candidates)][1]

if (is.na(processed)) {
  stop(
    "Cannot find data/processed. Checked:\n- ",
    paste(processed_candidates, collapse = "\n- "),
    "\nRun scripts/01_harmonize.R from the project root first."
  )
}

loss_stats <- read.csv(file.path(processed, "loss_by_county_year.csv")) |>
  filter(year >= 2010, year <= 2024)
tcc_stats <- read.csv(file.path(processed, "tcc_by_county_year.csv"))
loss_rds <- file.path(processed, "loss_by_year.rds")
loss_poly <- if (file.exists(loss_rds)) {
  readRDS(loss_rds)
} else {
  st_read(file.path(processed, "loss_by_year.gpkg"), quiet = TRUE)
}
tcc_decline_rds <- file.path(processed, "tcc_decline_2010_2025.rds")
tcc_decline_poly <- if (file.exists(tcc_decline_rds)) {
  readRDS(tcc_decline_rds)
} else if (file.exists(file.path(processed, "tcc_decline_2010_2025.gpkg"))) {
  st_read(file.path(processed, "tcc_decline_2010_2025.gpkg"), quiet = TRUE)
} else {
  NULL
}
counties <- st_read(file.path(processed, "counties.gpkg"), quiet = TRUE) |>
  st_transform(4326)
bb <- st_bbox(counties)

loss_annual <- loss_stats |>
  group_by(year) |>
  summarise(acres = sum(acres), .groups = "drop")
loss_cumul <- loss_annual |>
  mutate(cum_acres = cumsum(acres))

# Named lookup vectors so summary text updates without recomputing tables.
acres_by_year <- setNames(loss_annual$acres, loss_annual$year)
cum_acres_by_year <- setNames(loss_cumul$cum_acres, loss_cumul$year)
tcc_by_year <- tcc_stats |>
  group_by(year) |>
  summarise(mean_tcc = mean(mean_tcc), .groups = "drop")
tcc_mean_by_year <- setNames(tcc_by_year$mean_tcc, tcc_by_year$year)

county_colors <- c(Houghton = "#2d6a4f", Keweenaw = "#bc6c25")
# Hansen: dark orange (older) → light orange/cream (newer)
year_pal <- colorNumeric(
  palette = colorRampPalette(c("#7c2d12", "#c2410c", "#ea580c", "#fb923c", "#ffedd5"))(15),
  domain = c(2010, 2024),
  na.color = "transparent"
)
HANSEN_EARLIER <- "#ea580c"
HANSEN_CURRENT <- "#9a3412"
TCC_BLUE <- "#2563eb"
TCC_BLUE_OUTLINE <- "#1e3a8a"
tcc_drop_pal <- colorNumeric(
  palette = c("#bfdbfe", "#60a5fa", "#2563eb", "#1e3a8a"),
  domain = c(15, 80),
  na.color = TCC_BLUE
)

theme <- bs_theme(version = 5, bootswatch = "minty", primary = "#2d6a4f")

summary_stat_box <- function(title_id, value_id, theme_class) {
  tags$div(
    class = paste("card mb-2", theme_class),
    tags$div(
      class = "card-body py-2 px-3",
      tags$div(class = "small opacity-75", textOutput(title_id, inline = TRUE)),
      tags$div(class = "fs-4 fw-semibold", textOutput(value_id, inline = TRUE))
    )
  )
}

ui <- page_sidebar(
  title = "Keweenaw & Houghton Forest Canopy Change Explorer, 2010-2025",
  theme = theme,
  fillable = TRUE,
  tags$style(HTML("
    .bslib-sidebar-layout > .sidebar {
      scrollbar-width: auto;
      scrollbar-color: #6c757d #e9ecef;
    }
    .bslib-sidebar-layout > .sidebar::-webkit-scrollbar {
      width: 14px;
    }
    .bslib-sidebar-layout > .sidebar::-webkit-scrollbar-track {
      background: #e9ecef;
    }
    .bslib-sidebar-layout > .sidebar::-webkit-scrollbar-thumb {
      background: #6c757d;
      border-radius: 7px;
      border: 2px solid #e9ecef;
    }
    .bslib-sidebar-layout > .sidebar::-webkit-scrollbar-thumb:hover {
      background: #495057;
    }
  ")),
  sidebar = sidebar(
    width = 400,
    sliderInput(
      "year", "Year",
      min = 2010, max = 2024, value = 2010, step = 1, sep = "",
      width = "100%", animate = animationOptions(interval = 1200, loop = FALSE)
    ),
    radioButtons(
      "basemap", "Background",
      choices = c(
        "Satellite" = "imagery",
        "Dark map" = "dark",
        "Light map" = "light"
      ),
      selected = "imagery",
      inline = TRUE
    ),
    radioButtons(
      "color_mode", "Map colors",
      choices = c(
        "This year vs earlier" = "highlight",
        "Color by year" = "by_year"
      ),
      selected = "highlight"
    ),
    checkboxInput("show_polygons", "Show Hansen disturbance (orange)", value = TRUE),
    checkboxInput(
      "show_tcc_decline",
      "Show NLCD TCC drop ≥15 pp, 2010–2025 (blue by magnitude)",
      value = TRUE
    ),
    uiOutput("color_key"),
    tags$p(
      class = "small text-muted mb-1",
      tags$b("Disturbance boxes"), " (dark green/pink) = acres of stand-replacing loss from ",
      tags$a(href = "https://glad.earthengine.app/view/global-forest-change",
             "Hansen Global Forest Change", target = "_blank"), ", masked to areas with \u2265 30% tree cover in 2010.",
      tags$br(),
      tags$b("Canopy box"), " (light green) = mean percent tree canopy from the ",
      tags$a(href = "https://data.fs.usda.gov/geodata/rastergateway/treecanopycover/",
             "USFS/NLCD Tree Canopy Cover", target = "_blank"),
      " product, averaged only over pixels with \u2265 30% tree cover in 2010\u2014a separate dataset that does not drive the map or acre totals."
    ),
    summary_stat_box("box_year_title", "box_year_value", "text-bg-primary"),
    summary_stat_box("box_cumul_title", "box_cumul_value", "text-bg-secondary"),
    summary_stat_box("box_tcc_title", "box_tcc_value", "text-bg-success"),
    card(
      card_header("Disturbance acres by year"),
      plotlyOutput("loss_chart", height = "240px")
    ),
    card(
      card_header("Mean tree canopy %"),
      plotlyOutput("tcc_chart", height = "240px")
    ),
    card(
      class = "bg-light",
      card_header(class = "fw-bold small", "What am I seeing?"),
      tags$div(
        class = "small p-2",
        tags$p(
          class = "mb-1",
          tags$b("Map patches and disturbance acres"),
          " come from the ",
          tags$a(href = "https://glad.earthengine.app/view/global-forest-change",
                 "Hansen Global Forest Change"),
          " dataset. Hansen flags pixels where Landsat imagery detected a ",
          tags$b("stand-replacing"), " loss event (forest to non-forest) in a given year.",
          " This is ", tags$em("not"), " a user-adjustable threshold\u2014the algorithm decides internally what counts as stand-replacement."
        ),
        tags$p(
          class = "mb-1",
          tags$b("Mean canopy %"),
          " (green box and chart) is a separate product: the ",
          tags$a(href = "https://data.fs.usda.gov/geodata/rastergateway/treecanopycover/",
                 "USFS/NLCD annual Tree Canopy Cover", target = "_blank"),
          " percentage, averaged over forested pixels (\u2265 30% tree cover in 2010) in both counties.",
          " It provides context on the overall canopy trend but does ", tags$em("not"), " drive the map or acre totals."
        ),
        tags$p(
          class = "mb-1",
          tags$b("Blue TCC decline"),
          " shows pixels where USFS/NLCD tree canopy % fell by ",
          tags$b("at least 15 percentage points"),
          " from 2010 to 2025 (county extent only\u2014not limited to the 30% forest mask used for Hansen).",
          " Fill is ramped by drop size (pale = just over 15 pp; deep blue = larger drops).",
          " This catches openings Hansen may miss, including places that were already partially open in 2010.",
          " It is not dated by year and does not change the acre boxes."
        ),
        tags$p(
          class = "mb-0",
          "Disturbance includes harvest, blowdown, insects, fire, and other clearing\u2014it is ", tags$b("not a harvest inventory."),
          " Hansen data end in 2024. Hover a patch for its year."
        )
      )
    )
  ),
  card(
    full_screen = TRUE,
    class = "h-100",
    card_header("Where stand-replacing disturbance happened. Map data from Hansen Global Forest Loss. Satellite basemap: Esri World Imagery (~2020\u20132023 composites)."),
    tags$style(HTML("#map { height: calc(100vh - 120px); min-height: 520px; }")),
    leafletOutput("map", width = "100%", height = "calc(100vh - 120px)")
  )
)

server <- function(input, output, session) {
  yr <- reactive(as.integer(input$year))

  output$color_key <- renderUI({
    hansen_key <- if (identical(input$color_mode, "highlight")) {
      tags$div(
        class = "small mb-2",
        tags$div(
          class = "d-flex align-items-center gap-2 mb-1",
          tags$span(style = paste0(
            "display:inline-block;width:18px;height:12px;background:", HANSEN_EARLIER, ";"
          )),
          tags$span("Hansen: 2010 to year prior")
        ),
        tags$div(
          class = "d-flex align-items-center gap-2 mb-1",
          tags$span(style = paste0(
            "display:inline-block;width:18px;height:12px;background:", HANSEN_CURRENT, ";"
          )),
          tags$span(paste("Hansen selected year:", yr()))
        )
      )
    } else {
      tags$div(
        class = "small mb-2",
        p(class = "mb-1", "Hansen: darker orange = older; light = newer. Dark outline = slider year."),
        tags$div(
          class = "d-flex justify-content-between",
          tags$span("2010"),
          tags$span("2024")
        ),
        tags$div(style = paste0(
          "height:10px;border-radius:2px;background:linear-gradient(to right,",
          paste(colorRampPalette(c("#7c2d12", "#c2410c", "#ea580c", "#fb923c", "#ffedd5"))(8), collapse = ","),
          ");"
        ))
      )
    }
    tcc_key <- tags$div(
      class = "small mb-2",
      tags$div("NLCD TCC drop 2010→2025 (pale = 15 pp, deep = larger)"),
      tags$div(style = paste0(
        "height:10px;border-radius:2px;background:linear-gradient(to right,",
        "#bfdbfe,#60a5fa,#2563eb,#1e3a8a);"
      ))
    )
    tags$div(hansen_key, tcc_key)
  })

  output$box_year_title <- renderText(paste("Disturbance in", yr()))
  output$box_year_value <- renderText({
    acres <- acres_by_year[[as.character(yr())]]
    paste(format(round(acres), big.mark = ","), "acres")
  })

  output$box_cumul_title <- renderText(paste0("Cumulative 2010\u2013", yr()))
  output$box_cumul_value <- renderText({
    acres <- cum_acres_by_year[[as.character(yr())]]
    paste(format(round(acres), big.mark = ","), "acres")
  })

  output$box_tcc_title <- renderText(paste("Mean tree canopy cover in", yr()))
  output$box_tcc_value <- renderText({
    tcc <- tcc_mean_by_year[[as.character(yr())]]
    paste0(sprintf("%.1f", tcc), "%")
  })

  output$loss_chart <- renderPlotly({
    d <- loss_stats
    d$selected <- d$year == yr()
    max_acres <- max(d$acres, na.rm = TRUE)
    plot_ly(d, x = ~year, y = ~acres, color = ~county, colors = county_colors,
            type = "bar",
            customdata = ~year,
            hovertemplate = paste(
              "Year %{x}<br>",
              "Acres %{y:,.0f} acres<br>",
              "County %{fullData.name}",
              "<extra></extra>"
            )) |>
      layout(
        barmode = "stack",
        xaxis = list(title = "", dtick = 1),
        yaxis = list(
          title = "Acres",
          tickformat = ",.0f",
          range = c(0, max_acres * 1.15)
        ),
        legend = list(orientation = "h", font = list(size = 9), x = 0, y = -0.70, xanchor = "left"),
        shapes = list(list(
          type = "line", x0 = yr(), x1 = yr(), y0 = 0, y1 = 1, yref = "paper",
          line = list(color = "#111", width = 2)
        )),
        margin = list(t = 4, b = 28, l = 50, r = 8)
      ) |>
      config(displayModeBar = FALSE)
  })

  output$tcc_chart <- renderPlotly({
    min_tcc <- min(tcc_stats$mean_tcc, na.rm = TRUE)
    max_tcc <- max(tcc_stats$mean_tcc, na.rm = TRUE)
    plot_ly(tcc_stats, x = ~year, y = ~mean_tcc, color = ~county,
            colors = county_colors, type = "scatter", mode = "lines+markers",
            hovertemplate = paste(
              "Year %{x}<br>",
              "%{y:.0f}%<br>",
              "County %{fullData.name}",
              "<extra></extra>"
            )) |>
      layout(
        xaxis = list(title = "", dtick = 1, range = c(2009.5, 2025.5)),
        yaxis = list(
          title = "Percent",
          tickformat = ".0f",
          range = c(max(0, floor(min_tcc) - 2), ceiling(max_tcc) + 2)
        ),
        legend = list(orientation = "h", font = list(size = 9), x = 0, y = -0.70, xanchor = "left"),
        shapes = list(list(
          type = "line", x0 = yr(), x1 = yr(), y0 = 0, y1 = 1, yref = "paper",
          line = list(color = "#111", width = 2)
        )),
        margin = list(t = 4, b = 28, l = 40, r = 8)
      ) |>
      config(displayModeBar = FALSE)
  })

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 8)) |>
      addMapPane("counties", zIndex = 410) |>
      addMapPane("tcc_decline", zIndex = 415) |>
      addMapPane("loss", zIndex = 420) |>
      addProviderTiles(providers$Esri.WorldImagery, group = "tiles") |>
      addPolygons(
        data = counties,
        group = "counties",
        options = pathOptions(pane = "counties"),
        fill = FALSE,
        color = "#1a1a1a",
        weight = 1.6,
        opacity = 0.95
      ) |>
      fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
  })

  observe({
    basemap <- input$basemap
    proxy <- leafletProxy("map") |>
      clearTiles() |>
      clearGroup("counties")

    if (identical(basemap, "imagery")) {
      proxy <- proxy |>
        addProviderTiles(providers$Esri.WorldImagery, group = "tiles") |>
        addPolygons(
          data = counties,
          group = "counties",
          options = pathOptions(pane = "counties"),
          fill = FALSE,
          color = "#1a1a1a",
          weight = 1.6,
          opacity = 0.95
        )
    } else if (identical(basemap, "dark")) {
      proxy <- proxy |>
        addProviderTiles(providers$CartoDB.DarkMatter, group = "tiles") |>
        addPolygons(
          data = counties,
          group = "counties",
          options = pathOptions(pane = "counties"),
          fillColor = "#2b2b2b",
          fillOpacity = 0.45,
          color = "#111111",
          weight = 1.6,
          opacity = 1
        )
    } else {
      proxy <- proxy |>
        addProviderTiles(providers$CartoDB.Positron, group = "tiles") |>
        addPolygons(
          data = counties,
          group = "counties",
          options = pathOptions(pane = "counties"),
          fillColor = "#3a3a3a",
          fillOpacity = 0.78,
          color = "#1a1a1a",
          weight = 1.4,
          opacity = 1
        )
    }
    proxy
  })

  observe({
    proxy <- leafletProxy("map") |> clearGroup("tcc_decline")
    if (!isTRUE(input$show_tcc_decline)) return()
    if (is.null(tcc_decline_poly) || nrow(tcc_decline_poly) == 0) return()

    has_drop <- "drop_pp" %in% names(tcc_decline_poly)
    fill <- if (has_drop) {
      tcc_drop_pal(pmin(as.numeric(tcc_decline_poly$drop_pp), 80))
    } else {
      TCC_BLUE
    }

    proxy |>
      addPolygons(
        data = tcc_decline_poly,
        group = "tcc_decline",
        options = pathOptions(pane = "tcc_decline"),
        fillColor = fill,
        fillOpacity = 0.45,
        color = TCC_BLUE_OUTLINE,
        weight = 0.4,
        opacity = 0.7,
        label = ~label
      )
  })

  observe({
    proxy <- leafletProxy("map") |> clearGroup("loss")
    if (!isTRUE(input$show_polygons)) return()

    shown <- loss_poly |> filter(year <= yr())
    if (nrow(shown) == 0) return()

    earlier <- shown |> filter(year < yr())
    current <- shown |> filter(year == yr())
    highlight <- identical(input$color_mode, "highlight")

    if (nrow(earlier) > 0) {
      if (highlight) {
        proxy <- proxy |>
          addPolygons(
            data = earlier, group = "loss",
            options = pathOptions(pane = "loss"),
            fillColor = HANSEN_EARLIER, fillOpacity = 0.85,
            color = "#9a3412", weight = 0.4, opacity = 0.9,
            label = ~as.character(year)
          )
      } else {
        proxy <- proxy |>
          addPolygons(
            data = earlier, group = "loss",
            options = pathOptions(pane = "loss"),
            fillColor = ~year_pal(year), fillOpacity = 0.88,
            color = ~year_pal(year), weight = 0.4, opacity = 0.95,
            label = ~as.character(year)
          )
      }
    }
    if (nrow(current) > 0) {
      if (highlight) {
        proxy <- proxy |>
          addPolygons(
            data = current, group = "loss",
            options = pathOptions(pane = "loss"),
            fillColor = HANSEN_CURRENT, fillOpacity = 0.95,
            color = "#ffffff", weight = 1.1, opacity = 1,
            label = ~paste(year, "(this year)")
          )
      } else {
        proxy <- proxy |>
          addPolygons(
            data = current, group = "loss",
            options = pathOptions(pane = "loss"),
            fillColor = ~year_pal(year), fillOpacity = 0.95,
            color = "#1c1917", weight = 1.4, opacity = 1,
            label = ~paste(year, "(this year)")
          )
      }
    }
  })
}

if (!identical(Sys.getenv("KEE_DEPLOY_FROM_ROOT"), "true")) {
  shinyApp(ui, server)
}
