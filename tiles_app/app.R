# Fast MapLibre + PMTiles explorer (tiles hosted on Cloudflare R2).
# From project root: shiny::runApp("tiles_app")
#
# Yes — summary boxes and charts are included (from lightweight CSVs).
# Map geometry comes from R2 PMTiles (not in-memory county polygons).

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(mapgl)
  library(dplyr)
  library(plotly)
})

proj_root <- if (dir.exists("data/processed")) {
  normalizePath(".")
} else if (dir.exists("../data/processed")) {
  normalizePath("..")
} else {
  stop("Cannot find data/processed. Run from kee_trees project root.")
}
processed <- file.path(proj_root, "data/processed")

loss_stats <- read.csv(file.path(processed, "loss_by_county_year.csv")) |>
  filter(year >= 2010, year <= 2024)
tcc_stats <- read.csv(file.path(processed, "tcc_by_county_year.csv"))

loss_annual <- loss_stats |>
  group_by(year) |>
  summarise(acres = sum(acres), .groups = "drop")
loss_cumul <- loss_annual |>
  mutate(cum_acres = cumsum(acres))

acres_by_year <- setNames(loss_annual$acres, loss_annual$year)
cum_acres_by_year <- setNames(loss_cumul$cum_acres, loss_cumul$year)
tcc_mean_by_year <- tcc_stats |>
  group_by(year) |>
  summarise(mean_tcc = mean(mean_tcc), .groups = "drop") |>
  (\(d) setNames(d$mean_tcc, d$year))()

county_colors <- c(Houghton = "#2d6a4f", Keweenaw = "#bc6c25")

TILE_BASE <- "https://pub-f86fa74bacfc40fa980ffc4d276a0036.r2.dev"

# Year may be string or number in tiles; coerce before comparing.
# Important: do NOT put this filter on add_fill_layer — mapgl keeps that as a
# "base" filter and ANDs it with set_filter(), which would lock the map to 2010.
hansen_filter <- function(year, mode = c("cumulative", "single")) {
  mode <- match.arg(mode)
  year <- as.integer(year)
  y <- list("to-number", list("get", "year"))
  if (identical(mode, "single")) {
    list("==", y, year)
  } else {
    list("all", list(">=", y, 2010L), list("<=", y, year))
  }
}

# Brighter = newer — pops on the default dark basemap (still readable on light).
hansen_year_colors <- colorRampPalette(
  c("#7c2d12", "#c2410c", "#ea580c", "#fb923c", "#ffedd5")
)(15)

# TCC drop magnitude (pp): pale = just over threshold, deep blue = large drop.
tcc_drop_colors <- c("#bfdbfe", "#60a5fa", "#2563eb", "#1e3a8a")
tcc_fill_ramp <- list(
  "interpolate", list("linear"),
  list("to-number", list("get", "drop_pp")),
  15, tcc_drop_colors[[1]],
  30, tcc_drop_colors[[2]],
  50, tcc_drop_colors[[3]],
  80, tcc_drop_colors[[4]]
)

# Free styles only (no Mapbox/MapTiler key). Satellite = Esri World Imagery raster.
basemap_choices <- c(
  "Dark (default)" = "dark",
  "Fiord (muted dark)" = "fiord",
  "Light" = "positron",
  "Streets" = "liberty",
  "Satellite" = "satellite"
)

satellite_style <- list(
  version = 8L,
  name = "Esri World Imagery",
  sources = list(
    esri_imagery = list(
      type = "raster",
      tiles = list(
        "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
      ),
      tileSize = 256L,
      attribution = "Tiles &copy; Esri — Source: Esri, Maxar, Earthstar Geographics, and the GIS User Community"
    )
  ),
  layers = list(
    list(id = "esri_imagery", type = "raster", source = "esri_imagery", minzoom = 0, maxzoom = 22)
  )
)

basemap_style_url <- function(id) {
  switch(
    id,
    dark = openfreemap_style("dark"),
    fiord = openfreemap_style("fiord"),
    positron = openfreemap_style("positron"),
    liberty = openfreemap_style("liberty"),
    satellite = satellite_style,
    openfreemap_style("dark")
  )
}
# to-number so color match works whether tiles store year as string or number
hansen_fill_ramp <- {
  yrs <- 2010:2024
  pairs <- vector("list", length(yrs) * 2L)
  for (i in seq_along(yrs)) {
    pairs[[2L * i - 1L]] <- yrs[[i]]
    pairs[[2L * i]] <- hansen_year_colors[[i]]
  }
  c(list("match", list("to-number", list("get", "year"))), pairs, list("#ea580c"))
}

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

theme <- bs_theme(version = 5, bootswatch = "minty", primary = "#2d6a4f")

ui <- page_sidebar(
  title = "Keweenaw & Houghton — fast tile explorer",
  theme = theme,
  fillable = TRUE,
  sidebar = sidebar(
    width = 380,
    sliderInput(
      "year", "Hansen loss year",
      min = 2010, max = 2024, value = 2010, step = 1, sep = "",
      width = "100%",
      animate = animationOptions(interval = 1200, loop = FALSE)
    ),
    radioButtons(
      "hansen_mode", "Hansen view",
      choices = c(
        "Cumulative through year" = "cumulative",
        "Selected year only" = "single"
      ),
      selected = "cumulative"
    ),
    checkboxInput("show_hansen", "Show Hansen loss (orange ramp by year)", TRUE),
    checkboxInput(
      "show_tcc",
      "Show NLCD TCC drop ≥15 pp (blue ramp by magnitude)",
      TRUE
    ),
    selectInput(
      "basemap", "Basemap",
      choices = basemap_choices,
      selected = "dark",
      width = "100%"
    ),
    tags$div(
      class = "small mb-2",
      tags$div("Hansen year (dark = older, bright = newer)"),
      tags$div(
        class = "d-flex justify-content-between",
        tags$span("2010"), tags$span("2024")
      ),
      tags$div(style = paste0(
        "height:10px;border-radius:2px;background:linear-gradient(to right,",
        paste(hansen_year_colors, collapse = ","), ");"
      )),
      tags$div(class = "mt-2", "NLCD TCC drop 2010→2025 (pale = 15 pp, deep = larger)"),
      tags$div(
        class = "d-flex justify-content-between",
        tags$span("15 pp"), tags$span("80+ pp")
      ),
      tags$div(style = paste0(
        "height:10px;border-radius:2px;background:linear-gradient(to right,",
        paste(tcc_drop_colors, collapse = ","), ");"
      ))
    ),
    tags$p(
      class = "small text-muted",
      "Orange = Hansen stand-replacing loss by year (slider). ",
      "Blue = USFS/NLCD tree canopy % drop from 2010 to 2025 where the drop is ≥15 percentage points ",
      "(not the same as Hansen; no year; county mask only). Deeper blue = larger canopy drop. ",
      "Both miss some visible clearing; click a patch for details. ",
      "Summaries/charts still use the CSV acre / mean-TCC stats."
    ),
    summary_stat_box("box_year_title", "box_year_value", "text-bg-primary"),
    summary_stat_box("box_cumul_title", "box_cumul_value", "text-bg-secondary"),
    summary_stat_box("box_tcc_title", "box_tcc_value", "text-bg-success"),
    card(
      card_header("Disturbance acres by year"),
      plotlyOutput("loss_chart", height = "220px")
    ),
    card(
      card_header("Mean tree canopy %"),
      plotlyOutput("tcc_chart", height = "220px")
    )
  ),
  card(
    full_screen = TRUE,
    class = "h-100",
    card_header("MapLibre + PMTiles (open in browser if RStudio Viewer looks empty)"),
    maplibreOutput("map", height = "75vh")
  )
)

server <- function(input, output, session) {
  yr <- reactive(as.integer(input$year))

  output$box_year_title <- renderText(paste("Disturbance in", yr()))
  output$box_year_value <- renderText({
    paste(format(round(acres_by_year[[as.character(yr())]]), big.mark = ","), "acres")
  })
  output$box_cumul_title <- renderText(paste0("Cumulative 2010\u2013", yr()))
  output$box_cumul_value <- renderText({
    paste(format(round(cum_acres_by_year[[as.character(yr())]]), big.mark = ","), "acres")
  })
  output$box_tcc_title <- renderText(paste("Mean tree canopy cover in", yr()))
  output$box_tcc_value <- renderText({
    paste0(sprintf("%.1f", tcc_mean_by_year[[as.character(yr())]]), "%")
  })

  output$loss_chart <- renderPlotly({
    d <- loss_stats
    max_acres <- max(d$acres, na.rm = TRUE)
    plot_ly(
      d, x = ~year, y = ~acres, color = ~county, colors = county_colors,
      type = "bar",
      hovertemplate = "Year %{x}<br>Acres %{y:,.0f}<br>%{fullData.name}<extra></extra>"
    ) |>
      layout(
        barmode = "stack",
        xaxis = list(title = "", dtick = 1),
        yaxis = list(title = "Acres", tickformat = ",.0f", range = c(0, max_acres * 1.15)),
        legend = list(orientation = "h", font = list(size = 9), x = 0, y = -0.55),
        shapes = list(list(
          type = "line", x0 = yr(), x1 = yr(), y0 = 0, y1 = 1, yref = "paper",
          line = list(color = "#111", width = 2)
        )),
        margin = list(t = 4, b = 40, l = 50, r = 8)
      ) |>
      config(displayModeBar = FALSE)
  })

  output$tcc_chart <- renderPlotly({
    min_tcc <- min(tcc_stats$mean_tcc, na.rm = TRUE)
    max_tcc <- max(tcc_stats$mean_tcc, na.rm = TRUE)
    plot_ly(
      tcc_stats, x = ~year, y = ~mean_tcc, color = ~county,
      colors = county_colors, type = "scatter", mode = "lines+markers",
      hovertemplate = "Year %{x}<br>%{y:.0f}%<br>%{fullData.name}<extra></extra>"
    ) |>
      layout(
        xaxis = list(title = "", dtick = 1, range = c(2009.5, 2025.5)),
        yaxis = list(
          title = "Percent", tickformat = ".0f",
          range = c(max(0, floor(min_tcc) - 2), ceiling(max_tcc) + 2)
        ),
        legend = list(orientation = "h", font = list(size = 9), x = 0, y = -0.55),
        shapes = list(list(
          type = "line", x0 = yr(), x1 = yr(), y0 = 0, y1 = 1, yref = "paper",
          line = list(color = "#111", width = 2)
        )),
        margin = list(t = 4, b = 40, l = 40, r = 8)
      ) |>
      config(displayModeBar = FALSE)
  })

  output$map <- renderMaplibre({
    maplibre(
      style = basemap_style_url("dark"),
      center = c(-88.41, 47.30),
      zoom = 9,
      scrollZoom = TRUE
    ) |>
      add_pmtiles_source(
        id = "tcc-tiles",
        url = paste0(TILE_BASE, "/tcc_decline.pmtiles")
      ) |>
      add_pmtiles_source(
        id = "hansen-tiles",
        url = paste0(TILE_BASE, "/hansen_loss.pmtiles")
      ) |>
      add_fill_layer(
        id = "tcc_decline",
        source = "tcc-tiles",
        source_layer = "tcc_decline",
        fill_color = tcc_fill_ramp,
        fill_opacity = 0.55,
        popup = paste0(
          "<strong>NLCD TCC canopy drop</strong><br>",
          "{drop_pp} percentage points (2010→2025)<br>",
          "Class acres (this drop size): {acres}"
        ),
        visibility = "visible"
      ) |>
      add_fill_layer(
        id = "hansen",
        source = "hansen-tiles",
        source_layer = "hansen",
        fill_color = hansen_fill_ramp,
        fill_opacity = 0.8,
        # No initial filter: mapgl ANDs layer "base" filter with set_filter().
        popup = "<strong>Hansen stand-replacing loss</strong><br>Year: {year}",
        visibility = "visible"
      )
  })

  # Keep Hansen filter in sync with the year slider / mode.
  # mapgl ANDs set_filter() with the layer's initial filter — so we never set
  # an initial filter on add_fill_layer (see hansen_filter comment above).
  apply_hansen_view <- function() {
    proxy <- maplibre_proxy("map")
    if (!isTRUE(input$show_hansen)) {
      set_layout_property(proxy, layer_id = "hansen", name = "visibility", value = "none")
      return(invisible())
    }
    set_layout_property(proxy, layer_id = "hansen", name = "visibility", value = "visible")
    set_filter(proxy, layer_id = "hansen", filter = hansen_filter(input$year, input$hansen_mode))
  }

  apply_tcc_visibility <- function() {
    vis <- if (isTRUE(input$show_tcc)) "visible" else "none"
    set_layout_property(
      maplibre_proxy("map"),
      layer_id = "tcc_decline",
      name = "visibility",
      value = vis
    )
  }

  observeEvent(
    list(input$year, input$hansen_mode, input$show_hansen),
    apply_hansen_view(),
    ignoreInit = FALSE
  )
  # Re-apply once after paint (isolate: onFlushed is not a reactive consumer).
  session$onFlushed(function() isolate(apply_hansen_view()), once = TRUE)

  observeEvent(input$show_tcc, apply_tcc_visibility(), ignoreInit = FALSE)

  # Switch basemap; preserve overlay layers, then re-apply filter/visibility.
  observeEvent(input$basemap, {
    set_style(
      maplibre_proxy("map"),
      style = basemap_style_url(input$basemap),
      preserve_layers = TRUE
    )
    apply_hansen_view()
    apply_tcc_visibility()
  }, ignoreInit = TRUE)
}

if (!identical(Sys.getenv("KEE_DEPLOY_FROM_ROOT"), "true")) {
  shinyApp(ui, server)
}
