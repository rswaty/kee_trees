# Keweenaw + Houghton canopy explorer
# From the project root: shiny::runApp("dashboard")

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(leaflet)
  library(sf)
  library(dplyr)
  library(plotly)
  library(viridisLite)
})

cwd <- normalizePath(getwd())
processed_candidates <- c(
  file.path(cwd, "data/processed"),
  file.path(cwd, "../data/processed"),
  file.path(cwd, "../../data/processed")
)
existing_processed <- processed_candidates[dir.exists(processed_candidates)]

if (length(existing_processed) == 0) {
  stop(
    "Cannot find data/processed. Checked:\n- ",
    paste(processed_candidates, collapse = "\n- "),
    "\nRun R/01_harmonize.R from the project root first."
  )
}

processed <- existing_processed[[1]]

loss_stats <- read.csv(file.path(processed, "loss_by_county_year.csv")) |>
  filter(year >= 2010, year <= 2024)
tcc_stats <- read.csv(file.path(processed, "tcc_by_county_year.csv"))
loss_poly <- st_read(file.path(processed, "loss_by_year.gpkg"), quiet = TRUE)
counties <- st_read(file.path(processed, "counties.gpkg"), quiet = TRUE) |>
  st_transform(4326)
bb <- st_bbox(counties)

loss_annual <- loss_stats |>
  group_by(year) |>
  summarise(acres = sum(acres), .groups = "drop")
loss_cumul <- loss_annual |>
  mutate(cum_acres = cumsum(acres))

county_colors <- c(Houghton = "#2d6a4f", Keweenaw = "#bc6c25")
year_pal <- colorNumeric(
  palette = colorRampPalette(c("#1b4332", "#40916c", "#95d5b2", "#d8f3dc", "#ffffff"))(15),
  domain = c(2010, 2024),
  na.color = "transparent"
)
GOLD <- "#FFD166"
PURPLE <- "#9b5de5"

theme <- bs_theme(version = 5, bootswatch = "minty", primary = "#2d6a4f")

ui <- page_sidebar(
  title = "Keweenaw & Houghton Forest Canopy Change Explorer, 2010-2025",
  theme = theme,
  fillable = TRUE,
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
    uiOutput("color_key"),
    tags$p(
      class = "small text-muted mb-1",
      tags$b("Disturbance boxes"), " (dark green/pink) = acres of stand-replacing loss from ",
      tags$a(href = "https://glad.earthengine.app/view/global-forest-change",
             "Hansen Global Forest Change", target = "_blank"), ", masked to areas with \u2265 30% tree cover in 2000.",
      tags$br(),
      tags$b("Canopy box"), " (light green) = mean percent tree canopy from the ",
      tags$a(href = "https://data.fs.usda.gov/geodata/rastergateway/treecanopycover/",
             "USFS/NLCD Tree Canopy Cover", target = "_blank"),
      " product, averaged only over pixels with \u2265 30% tree cover in 2000\u2014a separate dataset that does not drive the map or acre totals."
    ),
    uiOutput("box_year"),
    uiOutput("box_cumul"),
    uiOutput("box_tcc"),
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
          " percentage, averaged over forested pixels (\u2265 30% tree cover in 2000) in both counties.",
          " It provides context on the overall canopy trend but does ", tags$em("not"), " drive the map or acre totals."
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
    if (identical(input$color_mode, "highlight")) {
      tags$div(
        class = "small mb-2",
        tags$div(
          class = "d-flex align-items-center gap-2 mb-1",
          tags$span(style = paste0(
            "display:inline-block;width:18px;height:12px;background:", GOLD, ";"
          )),
          tags$span("2010 to year prior to highlighted one")
        ),
        tags$div(
          class = "d-flex align-items-center gap-2",
          tags$span(style = paste0(
            "display:inline-block;width:18px;height:12px;background:", PURPLE, ";"
          )),
          tags$span(paste("Selected year:", yr()))
        )
      )
    } else {
      tags$div(
        class = "small mb-2",
        p(class = "mb-1", "Darker greens are older disturbances. Purple outline = the slider year."),
        tags$div(
          class = "d-flex justify-content-between",
          tags$span("2010"),
          tags$span("2024")
        ),
        tags$div(style = paste0(
          "height:10px;border-radius:2px;background:linear-gradient(to right,",
          paste(colorRampPalette(c("#1b4332", "#40916c", "#95d5b2", "#d8f3dc", "#ffffff"))(8), collapse = ","),
          ");"
        ))
      )
    }
  })

  output$box_year <- renderUI({
    acres <- loss_annual$acres[loss_annual$year == yr()]
    value_box(
      title = paste("Disturbance in", yr()),
      value = paste(format(round(acres), big.mark = ","), "acres"),
      theme = "primary"
    )
  })

  output$box_cumul <- renderUI({
    acres <- loss_cumul$cum_acres[loss_cumul$year == yr()]
    value_box(
      title = paste("Cumulative 2010–", yr(), sep = ""),
      value = paste(format(round(acres), big.mark = ","), "acres"),
      theme = "secondary"
    )
  })

  output$box_tcc <- renderUI({
    tcc <- tcc_stats |>
      filter(year == yr()) |>
      summarise(m = mean(mean_tcc)) |>
      pull(m)
    value_box(
      title = paste("Mean tree canopy cover in", yr()),
      value = paste0(sprintf("%.1f", tcc), "%"),
      theme = "success"
    )
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
    shown <- loss_poly |> filter(year <= yr())
    proxy <- leafletProxy("map") |> clearGroup("loss")
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
            fillColor = GOLD, fillOpacity = 0.9,
            color = "#7a5c00", weight = 0.4, opacity = 0.9,
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
            fillColor = PURPLE, fillOpacity = 0.95,
            color = "#ffffff", weight = 1.1, opacity = 1,
            label = ~paste(year, "(this year)")
          )
      } else {
        proxy <- proxy |>
          addPolygons(
            data = current, group = "loss",
            options = pathOptions(pane = "loss"),
            fillColor = ~year_pal(year), fillOpacity = 0.95,
            color = PURPLE, weight = 1.4, opacity = 1,
            label = ~paste(year, "(this year)")
          )
      }
    }
  })
}

shinyApp(ui, server)
