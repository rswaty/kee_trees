# Keweenaw + Houghton canopy explorer
# From the project root: shiny::runApp("dashboard")

Sys.setenv(PROJ_NETWORK = "OFF")

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(leaflet)
  library(terra)
  library(sf)
  library(dplyr)
  library(plotly)
})

proj_root <- if (dir.exists("data/processed")) {
  normalizePath(".")
} else if (dir.exists("../data/processed")) {
  normalizePath("..")
} else {
  stop("Cannot find data/processed. Run R/01_harmonize.R from the project root first.")
}

processed <- file.path(proj_root, "data/processed")
tcc_dir <- file.path(proj_root, "data/TCC_Houghton_Keweenaw")

loss <- rast(file.path(processed, "hansen_lossyear.tif"))
cover <- rast(file.path(processed, "hansen_treecover2000.tif"))
tcc_2010 <- rast(file.path(tcc_dir, "HK_TCC_2010.tif"))
tcc_2010 <- subst(tcc_2010, c(254, 255), NA)
tcc_2010 <- mask(tcc_2010, loss)

loss_stats <- read.csv(file.path(processed, "loss_by_county_year.csv"))
tcc_stats <- read.csv(file.path(processed, "tcc_by_county_year.csv"))
patch_stats <- read.csv(file.path(processed, "loss_patch_stats.csv"))
patch_sizes <- read.csv(file.path(processed, "loss_patch_sizes.csv"))

counties <- st_read(file.path(processed, "counties.gpkg"), quiet = TRUE) |>
  st_transform(4326)
bb <- st_bbox(counties)

tcc_cache <- new.env(parent = emptyenv())
tcc_cache[["2010"]] <- tcc_2010

get_tcc <- function(year) {
  key <- as.character(year)
  if (!exists(key, envir = tcc_cache, inherits = FALSE)) {
    r <- rast(file.path(tcc_dir, sprintf("HK_TCC_%d.tif", year)))
    r <- subst(r, c(254, 255), NA)
    r <- mask(r, loss)
    assign(key, r, envir = tcc_cache)
  }
  get(key, envir = tcc_cache, inherits = FALSE)
}

loss_year_code <- function(year) as.integer(year) - 2000

mask_forest <- function(r, use_forest) {
  if (isTRUE(use_forest)) ifel(cover >= 30, r, NA) else r
}

county_colors <- c(Houghton = "#2d6a4f", Keweenaw = "#bc6c25")

theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  primary = "#2d6a4f"
)

ui <- page_sidebar(
  title = "Keweenaw & Houghton tree canopy change",
  theme = theme,
  sidebar = sidebar(
    width = 340,
    p("Pixel-level explorer for USFS/NLCD tree canopy cover and Hansen stand-replacing disturbance. Loss is not a harvest inventory."),
    sliderInput(
      "year", "Year",
      min = 2010, max = 2025, value = 2022, step = 1, sep = "",
      ticks = FALSE, width = "100%"
    ),
    div(
      class = "d-flex gap-2",
      actionButton("play", "Play years", class = "btn-primary btn-sm"),
      actionButton("stop", "Pause", class = "btn-outline-secondary btn-sm")
    ),
    radioButtons(
      "mode", "Map layer",
      choices = c(
        "Loss that year" = "loss",
        "Cumulative loss (2010 through year)" = "cumulative",
        "Canopy % (TCC)" = "tcc",
        "Canopy change since 2010" = "tcc_change"
      ),
      selected = "loss"
    ),
    checkboxInput("forest_mask", "Mask Hansen to ≥30% tree cover in 2000", TRUE),
    sliderInput(
      "min_change", "Hide |TCC change| smaller than (percentage points)",
      min = 0, max = 20, value = 5, step = 1
    ),
    accordion(
      accordion_panel(
        "Methods",
        tags$ul(
          tags$li(tags$b("TCC:"), " USFS NLCD annual percent tree canopy, 30 m, 2010–2025."),
          tags$li(tags$b("Hansen lossyear:"), " year of stand-replacing disturbance, 2001–2024. Reprojected to the TCC grid."),
          tags$li("Charts use pixels inside Houghton and Keweenaw. Disturbance includes harvest, blowdown, insects, and other clearing.")
        )
      )
    )
  ),
  layout_columns(
    col_widths = c(12, 4, 4, 4),
    card(full_screen = TRUE, leafletOutput("map", height = "520px")),
    card(card_header("Disturbance acres"), plotlyOutput("loss_chart", height = "260px")),
    card(card_header("Mean canopy %"), plotlyOutput("tcc_chart", height = "260px")),
    card(card_header("Patch size (selected year)"), plotlyOutput("patch_chart", height = "260px"))
  )
)

server <- function(input, output, session) {
  playing <- reactiveVal(FALSE)

  observeEvent(input$play, playing(TRUE))
  observeEvent(input$stop, playing(FALSE))

  observe({
    if (!playing()) return()
    invalidateLater(900, session)
    isolate({
      nxt <- input$year + 1
      if (nxt > 2025) nxt <- 2010
      updateSliderInput(session, "year", value = nxt)
    })
  })

  map_raster <- reactive({
    year <- input$year
    mode <- input$mode
    if (mode == "loss") {
      code <- loss_year_code(min(year, 2024))
      r <- ifel(loss == code, loss, NA)
      r <- mask_forest(r, input$forest_mask)
      list(r = r, kind = "lossyear")
    } else if (mode == "cumulative") {
      end_code <- loss_year_code(min(year, 2024))
      r <- ifel(loss >= 10 & loss <= end_code, loss, NA)
      r <- mask_forest(r, input$forest_mask)
      list(r = r, kind = "lossyear")
    } else if (mode == "tcc") {
      list(r = get_tcc(year), kind = "tcc")
    } else {
      r <- get_tcc(year) - tcc_2010
      if (input$min_change > 0) {
        r <- ifel(abs(r) >= input$min_change, r, NA)
      }
      list(r = r, kind = "change")
    }
  })

  output$map <- renderLeaflet({
    leaflet(counties, options = leafletOptions(minZoom = 8)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(
        fill = FALSE, color = "#1b4332", weight = 2, opacity = 0.9
      ) |>
      fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
  })

  observe({
    spec <- map_raster()
    r <- spec$r
    kind <- spec$kind
    proxy <- leafletProxy("map") |>
      clearImages() |>
      clearControls()

    vals <- values(r)
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0) return()

    if (kind == "lossyear") {
      pal <- colorNumeric("YlOrRd", domain = c(10, 24), na.color = "transparent")
      proxy |>
        addRasterImage(r, colors = pal, opacity = 0.85, project = TRUE,
                       maxBytes = 32 * 1024 * 1024) |>
        addLegend(
          pal = pal, values = c(10, 24), title = "Loss year",
          labFormat = labelFormat(transform = function(x) x + 2000)
        )
    } else if (kind == "tcc") {
      pal <- colorNumeric("YlGn", domain = c(0, 100), na.color = "transparent")
      proxy |>
        addRasterImage(r, colors = pal, opacity = 0.8, project = TRUE,
                       maxBytes = 32 * 1024 * 1024) |>
        addLegend(pal = pal, values = c(0, 100), title = "Canopy %")
    } else {
      pal <- colorNumeric(
        "RdBu", domain = c(-30, 30), reverse = TRUE, na.color = "transparent"
      )
      proxy |>
        addRasterImage(r, colors = pal, opacity = 0.85, project = TRUE,
                       maxBytes = 32 * 1024 * 1024) |>
        addLegend(pal = pal, values = c(-30, 30), title = "TCC change (pp)")
    }
  })

  output$loss_chart <- renderPlotly({
    d <- loss_stats |>
      filter(year >= 2010, year <= 2024)
    plot_ly(d, x = ~year, y = ~acres, color = ~county, colors = county_colors,
            type = "bar") |>
      layout(
        barmode = "stack",
        xaxis = list(title = "", dtick = 1, tickangle = -45),
        yaxis = list(title = "acres"),
        legend = list(orientation = "h"),
        shapes = list(list(
          type = "line", x0 = input$year, x1 = input$year,
          y0 = 0, y1 = 1, yref = "paper",
          line = list(color = "#333", dash = "dot")
        )),
        margin = list(t = 10)
      ) |>
      config(displayModeBar = FALSE)
  })

  output$tcc_chart <- renderPlotly({
    plot_ly(tcc_stats, x = ~year, y = ~mean_tcc, color = ~county,
            colors = county_colors, type = "scatter", mode = "lines+markers") |>
      layout(
        xaxis = list(title = "", dtick = 1, tickangle = -45),
        yaxis = list(title = "mean TCC %", rangemode = "tozero"),
        legend = list(orientation = "h"),
        shapes = list(list(
          type = "line", x0 = input$year, x1 = input$year,
          y0 = 0, y1 = 1, yref = "paper",
          line = list(color = "#333", dash = "dot")
        )),
        margin = list(t = 10)
      ) |>
      config(displayModeBar = FALSE)
  })

  output$patch_chart <- renderPlotly({
    yr <- min(input$year, 2024)
    d <- patch_sizes |> filter(year == yr)
    if (nrow(d) == 0) {
      return(plotly_empty() |> layout(title = "No Hansen loss in 2025"))
    }
    med <- patch_stats$median_acres[patch_stats$year == yr]
    plot_ly(d, x = ~acres, type = "histogram",
            marker = list(color = "#2d6a4f")) |>
      layout(
        xaxis = list(title = "patch acres", type = "log"),
        yaxis = list(title = "count"),
        annotations = list(list(
          x = 0.5, y = 1.05, xref = "paper", yref = "paper", showarrow = FALSE,
          text = sprintf("%d: %s patches, median %.1f ac",
                         yr, format(nrow(d), big.mark = ","), med)
        )),
        margin = list(t = 30)
      ) |>
      config(displayModeBar = FALSE)
  })
}

shinyApp(ui, server)
