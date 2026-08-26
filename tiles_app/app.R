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
fdist_acres_path <- file.path(processed, "landfire_fdist_by_agent.csv")
fdist_acres <- if (file.exists(fdist_acres_path)) {
  read.csv(fdist_acres_path)
} else {
  data.frame(agent = character(), label = character(), acres = numeric())
}

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

# Prefer same-origin tiles (www/tiles or data/tiles). shinyapps' static file
# server ignores HTTP Range, which breaks PMTiles — so we serve tiles ourselves
# with byte-range support (see ui function below). R2 / jsDelivr are fallbacks.
TILE_BASE_R2 <- "https://pub-f86fa74bacfc40fa980ffc4d276a0036.r2.dev"
# raw.githubusercontent.com supports HTTP Range + CORS (jsDelivr cached a bad size once).
TILE_BASE_GITHUB <- "https://raw.githubusercontent.com/rswaty/kee_trees/main/www/tiles"
tile_dir_candidates <- c(
  file.path(proj_root, "www", "tiles"),
  file.path(proj_root, "data", "tiles")
)
local_tile_dir <- NULL
for (d in tile_dir_candidates) {
  if (file.exists(file.path(d, "hansen_loss.pmtiles")) &&
      file.exists(file.path(d, "tcc_decline.pmtiles"))) {
    local_tile_dir <- d
    break
  }
}

serve_pmtiles_range <- function(path, request) {
  info <- file.info(path)
  if (is.na(info$size)) {
    return(shiny::httpResponse(404L, content_type = "text/plain", content = "Not found"))
  }
  file_size <- as.integer(info$size)
  range_header <- request$HTTP_RANGE
  if (is.null(range_header) || !nzchar(range_header)) {
    return(shiny::httpResponse(
      status = 200L,
      content_type = "application/octet-stream",
      content = readBin(path, what = "raw", n = file_size),
      headers = list(
        "Accept-Ranges" = "bytes",
        "Content-Length" = as.character(file_size),
        "Access-Control-Allow-Origin" = "*",
        "Access-Control-Expose-Headers" = "Accept-Ranges, Content-Range, Content-Length, ETag"
      )
    ))
  }
  m <- regmatches(range_header, regexec("^bytes=([0-9]+)-([0-9]*)$", range_header))[[1]]
  if (length(m) < 2) {
    return(shiny::httpResponse(416L, content_type = "text/plain", content = "Invalid Range"))
  }
  start <- as.integer(m[2])
  end <- if (identical(m[3], "") || is.na(m[3])) file_size - 1L else as.integer(m[3])
  if (is.na(start) || is.na(end) || start > end || start >= file_size) {
    return(shiny::httpResponse(
      416L,
      content_type = "text/plain",
      content = "Range Not Satisfiable",
      headers = list("Content-Range" = paste0("bytes */", file_size))
    ))
  }
  end <- min(end, file_size - 1L)
  nbytes <- end - start + 1L
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  seek(con, where = start, origin = "start")
  data <- readBin(con, what = "raw", n = nbytes)
  shiny::httpResponse(
    status = 206L,
    content_type = "application/octet-stream",
    content = data,
    headers = list(
      "Accept-Ranges" = "bytes",
      "Content-Range" = sprintf("bytes %d-%d/%d", start, end, file_size),
      "Content-Length" = as.character(nbytes),
      "Access-Control-Allow-Origin" = "*",
      "Access-Control-Expose-Headers" = "Accept-Ranges, Content-Range, Content-Length, ETag"
    )
  )
}

app_tile_url <- function(session, filename) {
  host <- session$clientData$url_hostname
  on_shinyapps <- !is.null(host) && grepl("shinyapps\\.io$", host, ignore.case = TRUE)

  # Local runApp: serve via ui() Range handler (same origin).
  # shinyapps.io does not forward /kee_tiles/* into R, so use GitHub raw there
  # (www/tiles on main; supports HTTP Range + CORS).
  if (!isTRUE(on_shinyapps) && !is.null(local_tile_dir)) {
    proto <- session$clientData$url_protocol
    port <- session$clientData$url_port
    path <- session$clientData$url_pathname
    req(nzchar(host))
    if (is.null(path) || !nzchar(path)) path <- "/"
    if (!grepl("/$", path)) path <- paste0(path, "/")
    port_part <- if (!is.null(port) && nzchar(port) && !(port %in% c("80", "443"))) {
      paste0(":", port)
    } else {
      ""
    }
    return(paste0(proto, "//", host, port_part, path, "kee_tiles/", filename))
  }

  paste0(TILE_BASE_GITHUB, "/", filename)
}

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

# LANDFIRE disturbance types — bright, distinct hues for dark/light basemaps.
fdist_agent_colors <- c(
  harvest_remove = "#39FF14",
  mech_unknown = "#00E5FF",
  insects = "#FF2EEA",
  fire = "#FF3B00"
)
fdist_plain_labels <- c(
  harvest_remove = "Timber harvest or clearing",
  mech_unknown = "Other mechanical change",
  insects = "Insects or disease",
  fire = "Fire"
)
fdist_fill_ramp <- match_expr(
  column = "agent",
  values = names(fdist_agent_colors),
  stops = unname(fdist_agent_colors),
  default = "#FFD60A"
)
has_fdist_tiles <- any(file.exists(file.path(tile_dir_candidates, "landfire_fdist.pmtiles"))) ||
  nrow(fdist_acres) > 0


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

summary_stat_box <- function(title_id, value_id, theme_class, source = NULL) {
  tags$div(
    class = paste("card mb-3", theme_class),
    tags$div(
      class = "card-body py-2 px-3",
      tags$div(class = "small opacity-75", textOutput(title_id, inline = TRUE)),
      tags$div(class = "fs-4 fw-semibold", textOutput(value_id, inline = TRUE)),
      if (!is.null(source)) {
        tags$div(class = "small opacity-75 mt-1", source)
      }
    )
  )
}

theme <- bs_theme(version = 5, bootswatch = "minty", primary = "#2d6a4f")

fdist_total_acres <- if (nrow(fdist_acres) > 0) sum(fdist_acres$acres, na.rm = TRUE) else 0

app_ui <- page_sidebar(
  title = "Keweenaw & Houghton — fast tile explorer",
  theme = theme,
  fillable = TRUE,
  sidebar = sidebar(
    width = 380,
    tags$style(HTML("
      .kee-sidebar-gap > * { margin-bottom: 1rem !important; }
      .kee-sidebar-gap .form-group { margin-bottom: 1rem !important; }
      .kee-sidebar-gap .shiny-input-container { margin-bottom: 1rem !important; }
      .kee-sidebar-gap .card { margin-bottom: 1.15rem !important; }
    ")),
    tags$div(
      class = "kee-sidebar-gap",
    sliderInput(
      "year", "Hansen loss year",
      min = 2010, max = 2024, value = 2024, step = 1, sep = "",
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
      "Show USFS FIA TCC drop ≥15 pp (blue ramp by magnitude)",
      TRUE
    ),
    if (isTRUE(has_fdist_tiles)) {
      checkboxInput(
        "show_fdist",
        "Show LANDFIRE disturbances (cause of change)",
        TRUE
      )
    },
    selectInput(
      "basemap", "Basemap",
      choices = basemap_choices,
      selected = "dark",
      width = "100%"
    ),
    tags$div(
      class = "small",
      tags$div("Hansen year (dark = older, bright = newer)"),
      tags$div(
        class = "d-flex justify-content-between",
        tags$span("2010"), tags$span("2024")
      ),
      tags$div(style = paste0(
        "height:10px;border-radius:2px;background:linear-gradient(to right,",
        paste(hansen_year_colors, collapse = ","), ");"
      )),
      tags$div(class = "mt-3", "USFS FIA TCC drop 2010→2025 (pale = 15 pp, deep = larger)"),
      tags$div(
        class = "d-flex justify-content-between",
        tags$span("15 pp"), tags$span("80+ pp")
      ),
      tags$div(style = paste0(
        "height:10px;border-radius:2px;background:linear-gradient(to right,",
        paste(tcc_drop_colors, collapse = ","), ");"
      )),
      if (isTRUE(has_fdist_tiles)) {
        tagList(
          tags$div(
            class = "mt-3 mb-1",
            "LANDFIRE disturbance type (2014–2024)"
          ),
          tags$div(
            class = "small text-muted mb-2",
            "LANDFIRE records disturbances over 2014–2024; individual patches are not dated to a single year."
          ),
          tags$div(
            class = "small",
            style = "display:flex;flex-direction:column;gap:0.65rem;",
            lapply(names(fdist_agent_colors), function(a) {
              tags$div(
                style = "display:flex;align-items:center;gap:0.5rem;",
                tags$span(style = paste0(
                  "flex:0 0 auto;width:16px;height:16px;border-radius:2px;background:",
                  fdist_agent_colors[[a]],
                  ";box-shadow:0 0 0 1px rgba(0,0,0,0.25);"
                )),
                tags$span(fdist_plain_labels[[a]])
              )
            })
          )
        )
      }
    ),
    tags$p(
      class = "small text-muted",
      "Orange = Hansen stand-replacing loss by year (slider). ",
      "Blue = USFS FIA tree canopy % drop ≥15 pp (2010–2025). ",
      if (isTRUE(has_fdist_tiles)) {
        paste0(
          "Bright green / cyan / magenta / red-orange = LANDFIRE disturbance cause (2014–2024). ",
          "“Other mechanical change” is not confirmed harvest. "
        )
      },
      "Click a patch for details."
    ),
    tags$p(
      class = "small text-muted",
      "Summary boxes below are county totals from processed tables ",
      "(not drawn from the map tiles): Hansen loss acres, USFS FIA TCC means",
      if (isTRUE(has_fdist_tiles)) ", and LANDFIRE disturbance acres." else "."
    ),
    summary_stat_box(
      "box_year_title", "box_year_value", "text-bg-primary",
      source = "Source: Hansen Global Forest Change loss acres"
    ),
    summary_stat_box(
      "box_cumul_title", "box_cumul_value", "text-bg-secondary",
      source = "Source: Hansen Global Forest Change (summed by year)"
    ),
    summary_stat_box(
      "box_tcc_title", "box_tcc_value", "text-bg-success",
      source = "Source: USFS FIA Total Canopy Cover (county mean)"
    ),
    if (isTRUE(has_fdist_tiles) && nrow(fdist_acres) > 0) {
      summary_stat_box(
        "box_fdist_title", "box_fdist_value", "text-bg-warning",
        source = "Source: LANDFIRE FDist (2014–2024), all mapped types"
      )
    },
    card(
      card_header("Disturbance acres by year"),
      plotlyOutput("loss_chart", height = "220px")
    ),
    card(
      card_header("Mean tree canopy %"),
      plotlyOutput("tcc_chart", height = "220px")
    ),
    if (isTRUE(has_fdist_tiles) && nrow(fdist_acres) > 0) {
      card(
        card_header("LANDFIRE disturbance acres by type (2014–2024)"),
        plotlyOutput("fdist_chart", height = "240px")
      )
    },
    tags$hr(),
    tags$div(
      class = "small text-muted",
      tags$p(
        class = "fw-semibold mb-2",
        "What is mapped where (forest vs all land)"
      ),
      tags$p(
        class = "mb-2",
        "All layers are clipped to Houghton and Keweenaw Counties. ",
        "They are ", tags$em("not"), " forced onto the same forest-only mask, ",
        "so footprints can differ."
      ),
      tags$ul(
        class = "mb-2 ps-3",
        tags$li(
          tags$strong("Hansen loss (map + acre boxes/chart): "),
          "limited to pixels with USFS FIA tree canopy ≥ 30% in 2010, ",
          "so acre totals emphasize stand-replacing loss in forested areas."
        ),
        tags$li(
          tags$strong("Mean tree canopy % (box/chart): "),
          "averaged only over that same 2010 ≥ 30% canopy mask."
        ),
        tags$li(
          tags$strong("USFS FIA TCC drop ≥ 15 pp (map): "),
          "county extent only—no 30% forest gate—so canopy loss on ",
          "partially open or already-thin cover can still appear."
        ),
        if (isTRUE(has_fdist_tiles)) {
          tags$li(
            tags$strong("LANDFIRE disturbances (map + acres/chart): "),
            "county extent and disturbance type only—no forest mask. ",
            "Includes change on non-forest and low-canopy land ",
            "(especially “other mechanical change”). ",
            "Window is 2014–2024; patches are not dated to a single year."
          )
        }
      ),
      tags$p(
        class = "mb-0",
        "Use layers together for context, not as identical forest-only footprints. ",
        "Summary boxes come from processed county tables, not from counting map tiles."
      )
    )
    ) # kee-sidebar-gap
  ),
  card(
    full_screen = TRUE,
    class = "h-100",
    card_header(
      tags$strong("Map"),
      " — Blank in RStudio/Positron Viewer? Use ",
      tags$em("Open in Browser"),
      "."
    ),
    maplibreOutput("map", height = "75vh")
  )
)

# Intercept /kee_tiles/* with Range support (PMTiles needs 206 responses).
ui <- function(request) {
  path <- request$PATH_INFO
  if (is.null(path)) path <- ""
  if (grepl("(^|/)kee_tiles/", path) && !is.null(local_tile_dir)) {
    if (identical(request$REQUEST_METHOD, "OPTIONS")) {
      return(shiny::httpResponse(
        status = 204L,
        content = "",
        headers = list(
          "Access-Control-Allow-Origin" = "*",
          "Access-Control-Allow-Methods" = "GET, HEAD, OPTIONS",
          "Access-Control-Allow-Headers" = "Range, If-Match, *",
          "Access-Control-Expose-Headers" = "Accept-Ranges, Content-Range, Content-Length, ETag",
          "Access-Control-Max-Age" = "3600"
        )
      ))
    }
    fname <- sub(".*(^|/)kee_tiles/", "", path)
    fname <- sub("\\?.*$", "", fname)
    fpath <- file.path(local_tile_dir, fname)
    if (file.exists(fpath)) {
      return(serve_pmtiles_range(fpath, request))
    }
    return(shiny::httpResponse(404L, content_type = "text/plain", content = "Tile not found"))
  }
  app_ui
}

server <- function(input, output, session) {
  yr <- reactive(as.integer(input$year))

  output$box_year_title <- renderText(paste("Hansen loss in", yr()))
  output$box_year_value <- renderText({
    paste(format(round(acres_by_year[[as.character(yr())]]), big.mark = ","), "acres")
  })
  output$box_cumul_title <- renderText(paste0("Hansen loss cumulative 2010\u2013", yr()))
  output$box_cumul_value <- renderText({
    paste(format(round(cum_acres_by_year[[as.character(yr())]]), big.mark = ","), "acres")
  })
  output$box_tcc_title <- renderText(paste("Mean tree canopy cover in", yr()))
  output$box_tcc_value <- renderText({
    paste0(sprintf("%.1f", tcc_mean_by_year[[as.character(yr())]]), "%")
  })
  output$box_fdist_title <- renderText("LANDFIRE disturbances (2014–2024)")
  output$box_fdist_value <- renderText({
    paste(format(round(fdist_total_acres), big.mark = ","), "acres")
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

  output$fdist_chart <- renderPlotly({
    req(nrow(fdist_acres) > 0)
    d <- fdist_acres
    d$label <- unname(fdist_plain_labels[d$agent])
    d$label[is.na(d$label)] <- d$agent[is.na(d$label)]
    # Vertical bars (coord flip from prior horizontal layout), largest first.
    d <- d[order(-d$acres), , drop = FALSE]
    d$label <- factor(d$label, levels = d$label)
    cols <- unname(fdist_agent_colors[d$agent])
    plot_ly(
      d, x = ~label, y = ~acres, type = "bar",
      marker = list(color = cols),
      hovertemplate = "%{x}<br>%{y:,.0f} acres<extra></extra>"
    ) |>
      layout(
        xaxis = list(title = "", tickangle = -25),
        yaxis = list(title = "Acres", tickformat = ",.0f"),
        margin = list(t = 8, b = 70, l = 50, r = 8),
        showlegend = FALSE
      ) |>
      config(displayModeBar = FALSE)
  })

  output$map <- renderMaplibre({
    # Need host/path so same-origin tile URLs resolve on shinyapps and localhost.
    req(session$clientData$url_hostname)
    hansen_url <- app_tile_url(session, "hansen_loss.pmtiles")
    tcc_url <- app_tile_url(session, "tcc_decline.pmtiles")
    fdist_url <- app_tile_url(session, "landfire_fdist.pmtiles")

    m <- maplibre(
      style = basemap_style_url("dark"),
      center = c(-88.41, 47.30),
      zoom = 9,
      scrollZoom = TRUE
    ) |>
      add_pmtiles_source(id = "tcc-tiles", url = tcc_url) |>
      add_pmtiles_source(id = "hansen-tiles", url = hansen_url)

    if (isTRUE(has_fdist_tiles)) {
      m <- m |>
        add_pmtiles_source(id = "fdist-tiles", url = fdist_url) |>
        add_fill_layer(
          id = "landfire_fdist",
          source = "fdist-tiles",
          source_layer = "landfire_fdist",
          fill_color = fdist_fill_ramp,
          fill_opacity = 0.8,
          popup = paste0(
            "<strong>LANDFIRE disturbance (2014–2024)</strong><br>",
            "{label}<br>",
            "{acres} acres of this type in the map area<br>",
            "<em>Patches are not dated to a single year</em>"
          ),
          visibility = "visible"
        )
    }

    m <- m |>
      add_fill_layer(
        id = "tcc_decline",
        source = "tcc-tiles",
        source_layer = "tcc_decline",
        fill_color = tcc_fill_ramp,
        fill_opacity = 0.55,
        popup = paste0(
          "<strong>USFS FIA TCC canopy drop</strong><br>",
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
        popup = "<strong>Hansen stand-replacing loss</strong><br>Year: {year}",
        visibility = "visible"
      )
    m
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

  apply_fdist_visibility <- function() {
    if (!isTRUE(has_fdist_tiles)) return(invisible())
    vis <- if (isTRUE(input$show_fdist)) "visible" else "none"
    set_layout_property(
      maplibre_proxy("map"),
      layer_id = "landfire_fdist",
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
  session$onFlushed(function() {
    isolate({
      apply_hansen_view()
      apply_fdist_visibility()
    })
  }, once = TRUE)

  observeEvent(input$show_tcc, apply_tcc_visibility(), ignoreInit = FALSE)
  if (isTRUE(has_fdist_tiles)) {
    observeEvent(input$show_fdist, apply_fdist_visibility(), ignoreInit = FALSE)
  }

  # Switch basemap; preserve overlay layers, then re-apply filter/visibility.
  observeEvent(input$basemap, {
    set_style(
      maplibre_proxy("map"),
      style = basemap_style_url(input$basemap),
      preserve_layers = TRUE
    )
    apply_hansen_view()
    apply_tcc_visibility()
    apply_fdist_visibility()
  }, ignoreInit = TRUE)
}

if (!identical(Sys.getenv("KEE_DEPLOY_FROM_ROOT"), "true")) {
  # uiPattern must match /kee_tiles/* so the Range httpResponse handler runs
  # (default uiPattern is "/" only — PMTiles would 404 and the map shows basemap alone).
  shinyApp(ui, server, uiPattern = ".*")
}
