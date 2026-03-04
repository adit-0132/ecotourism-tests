library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ecotourism)

# Data loading 
data(manta_rays,        package = "ecotourism")
data(gouldian_finch,    package = "ecotourism")
data(glowworms,         package = "ecotourism")
data(orchids,           package = "ecotourism")
data(weather,           package = "ecotourism")
data(weather_stations,  package = "ecotourism")

organisms <- list(
  "Manta Ray"      = "manta_rays",
  "Gouldian Finch" = "gouldian_finch",
  "Glowworms"      = "glowworms",
  "Orchids"        = "orchids"
)

organism_icons <- list(
  "manta_rays"      = "🦈",
  "gouldian_finch"  = "🦜",
  "glowworms"       = "✨",
  "orchids"         = "🌸"
)

month_names <- c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
)

get_data <- function(organism_key) get(organism_key)

# Helper: card with download button in header 
card_with_dl <- function(header_label, dl_id, ...) {
  card(
    card_header(
      tags$div(
        style = "display:flex; justify-content:space-between; align-items:center;",
        tags$span(header_label),
        downloadButton(
          dl_id,
          label = "",
          icon  = icon("download"),
          style = paste(
            "padding: 2px 8px;",
            "font-size: 0.75rem;",
            "background: transparent;",
            "border: 1px solid #B8CCA8;",
            "color: #2D5A27;",
            "border-radius: 4px;",
            "line-height: 1.4;",
            "box-shadow: none;"
          )
        )
      )
    ),
    ...
  )
}

# Theme 
app_theme <- bs_theme(
  version      = 5,
  bg           = "#F5F0E8",
  fg           = "#1C2B1A",
  primary      = "#2D5A27",
  secondary    = "#5C8A4A",
  success      = "#3A7D44",
  info         = "#4A7C8A",
  base_font    = font_google("Lora"),
  heading_font = font_google("Playfair Display"),
  code_font    = font_google("Source Code Pro")
) |>
  bs_add_rules("
    body {
      background-color: #F5F0E8;
      background-image:
        radial-gradient(circle at 20% 50%, rgba(45,90,39,0.04) 0%, transparent 50%),
        radial-gradient(circle at 80% 20%, rgba(92,138,74,0.04) 0%, transparent 50%);
    }
    .sidebar {
      background-color: #1C2B1A !important;
      border-right: none !important;
      box-shadow: 4px 0 20px rgba(0,0,0,0.15);
    }
    .sidebar .sidebar-title {
      color: #C8D9A0 !important;
      font-family: 'Playfair Display', serif !important;
      font-size: 1.4rem !important;
      letter-spacing: 0.02em;
      padding-bottom: 0.5rem;
      border-bottom: 1px solid rgba(200,217,160,0.2);
      margin-bottom: 1rem;
    }
    .sidebar label {
      color: #A8C080 !important;
      font-size: 0.75rem !important;
      text-transform: uppercase;
      letter-spacing: 0.1em;
      font-family: 'Lora', serif;
    }
    .sidebar .form-select,
    .sidebar .form-control {
      background-color: #243322 !important;
      border: 1px solid #3A5235 !important;
      color: #E8ECD8 !important;
      border-radius: 6px;
    }
    .sidebar .form-select:focus {
      border-color: #7AB648 !important;
      box-shadow: 0 0 0 3px rgba(122,182,72,0.15) !important;
    }
    .sidebar hr {
      border-color: rgba(200,217,160,0.15);
      margin: 1.2rem 0;
    }
    .sidebar small, .sidebar .help-text {
      color: #6A8A5A !important;
      font-size: 0.72rem !important;
    }
    .card {
      background-color: #FDFAF4 !important;
      border: 1px solid #D8CEB8 !important;
      border-radius: 10px !important;
      box-shadow: 0 2px 12px rgba(28,43,26,0.06) !important;
    }
    .card-header {
      background-color: #F0EAD8 !important;
      border-bottom: 1px solid #D8CEB8 !important;
      color: #2D5A27 !important;
      font-family: 'Playfair Display', serif !important;
      font-size: 0.95rem !important;
      font-weight: 600;
      letter-spacing: 0.01em;
      padding: 0.65rem 1rem !important;
    }
    .stat-box {
      background: linear-gradient(135deg, #2D5A27, #3A7D44);
      border-radius: 8px;
      padding: 0.9rem 1rem;
      color: white;
      margin-bottom: 0.6rem;
    }
    .stat-box .stat-value {
      font-size: 1.8rem;
      font-family: 'Playfair Display', serif;
      font-weight: 700;
      line-height: 1;
      color: #C8E89A;
    }
    .stat-box .stat-label {
      font-size: 0.72rem;
      text-transform: uppercase;
      letter-spacing: 0.08em;
      opacity: 0.8;
      margin-top: 0.2rem;
    }
    .best-time-card {
      background: linear-gradient(135deg, #1C2B1A 0%, #2D5A27 100%);
      border-radius: 10px;
      padding: 1rem 1.2rem;
      color: #E8ECD8;
      border: 1px solid #3A5235;
    }
    .best-time-card .month-badge {
      display: inline-block;
      background-color: #7AB648;
      color: white;
      border-radius: 20px;
      padding: 0.2rem 0.8rem;
      font-size: 0.8rem;
      font-weight: 600;
      margin: 0.15rem;
    }
    .best-time-card h6 {
      color: #A8C080;
      font-size: 0.7rem;
      text-transform: uppercase;
      letter-spacing: 0.1em;
      margin-bottom: 0.5rem;
    }
    .organism-icon {
      font-size: 1.4rem;
      margin-right: 0.4rem;
    }
    .app-title {
      font-family: 'Playfair Display', serif;
      font-size: 1.1rem;
      color: #4a6608ff;
      line-height: 1.2;
    }
    .app-subtitle {
      font-size: 0.7rem;
      color: #6A8A5A;
      text-transform: uppercase;
      letter-spacing: 0.12em;
    }
    .leaflet-container { border-radius: 8px; }
    .leaflet-bottom.leaflet-right {
      margin-bottom: 5px;
      margin-right: 5px;
    }
    .nav-tabs .nav-link { color: #5C8A4A !important; font-size: 0.82rem; }
    .nav-tabs .nav-link.active {
      color: #2D5A27 !important;
      border-bottom: 2px solid #2D5A27 !important;
      font-weight: 600;
    }
    /* hide default download button text/border */
    .btn-dl { all: unset; cursor: pointer; }
  ")

# UI 
ui <- page_sidebar(
  theme = app_theme,
  title = tags$div(
    tags$div(class = "app-subtitle", "Atlas of Living Australia"),
    tags$div(class = "app-title",    "Wildlife Spotter")
  ),

  sidebar = sidebar(
    width = 260,
    open  = "open",
    selectInput("organism", "Organism",
                choices = organisms, selected = "manta_rays"),
    selectInput("month", "Filter by Month",
                choices  = c("All months" = 0, setNames(1:12, month_names)),
                selected = 0),
    hr(),
    checkboxGroupInput(
      "record_type", "Record Type",
      choices  = c("Human Observation"  = "HUMAN_OBSERVATION",
                   "Preserved Specimen" = "PRESERVED_SPECIMEN",
                   "Observation"        = "OBSERVATION",
                   "Material Sample"    = "MATERIAL_SAMPLE"),
      selected = c("HUMAN_OBSERVATION", "PRESERVED_SPECIMEN",
                   "OBSERVATION",       "MATERIAL_SAMPLE")
    ),
    hr(),
    tags$small("Data: Atlas of Living Australia, 2014-2024"),
    tags$br(),
    tags$small("Package: ecotourism (GSoC 2025)")
  ),

  # Top row: map + right panels 
  layout_columns(
    col_widths  = c(8, 4),
    row_heights = "auto",
    gap         = "1rem",

    # Map card with download button
    card(
      card_header(
        tags$div(
          style = "display:flex; justify-content:space-between; align-items:center;",
          uiOutput("map_header"),
          downloadButton(
            "dl_map",
            label = "",
            icon  = icon("download"),
            style = paste(
              "padding: 2px 8px; font-size: 0.75rem;",
              "background: transparent; border: 1px solid #B8CCA8;",
              "color: #2D5A27; border-radius: 4px;",
              "line-height: 1.4; box-shadow: none;"
            )
          )
        )
      ),
      leafletOutput("map", height = "550px"),
      full_screen = TRUE
    ),

    # Right column
    layout_columns(
      col_widths = c(12),

      card(
        card_header("Overview"),
        uiOutput("stats_boxes")
      ),

      card(
        card_header("Best Time to Visit"),
        uiOutput("best_time_ui")
      ),

      card_with_dl(
        "Sightings by Month", "dl_month",
        plotOutput("month_chart", height = "160px")
      )
    )
  ),

  # Bottom row: weather + year trend
  layout_columns(
    col_widths = c(6, 6),
    gap        = "1rem",

    card_with_dl(
      "Weather at Sighting Time", "dl_weather",
      plotOutput("weather_temp_chart", height = "180px")
    ),

    card_with_dl(
      "Sightings Over the Years", "dl_year",
      plotOutput("year_chart", height = "180px")
    )
  )
)

# Server
server <- function(input, output, session) {

  base_data <- reactive({ get_data(input$organism) })

  filtered <- reactive({
    d <- base_data()
    if (!is.null(input$record_type) && length(input$record_type) > 0)
      d <- d |> filter(record_type %in% input$record_type)
    if (as.integer(input$month) != 0)
      d <- d |> filter(month == as.integer(input$month))
    d
  })

  with_weather <- reactive({
    filtered() |>
      left_join(weather |> select(ws_id, date, temp, min, max, prcp),
                by = c("ws_id", "date"))
  })

  peak_months <- reactive({
    base_data() |>
      count(month) |> arrange(desc(n)) |>
      slice_head(n = 3) |> pull(month)
  })

  # Reusable plot builders (used for render + download)
  build_month_plot <- function() {
    d <- base_data() |>
      count(month) |>
      mutate(
        month_name = factor(month_names[month], levels = month_names),
        peak = month %in% peak_months()
      )
    sel_month <- as.integer(input$month)
    ggplot(d, aes(x = month_name, y = n, fill = peak)) +
      geom_col(width = 0.7) +
      {if (sel_month != 0)
        geom_col(data = d |> filter(month == sel_month),
                 aes(x = month_name, y = n),
                 fill = "#F4A261", width = 0.7)} +
      scale_fill_manual(values = c("TRUE" = "#2D5A27", "FALSE" = "#B8CCA8")) +
      scale_x_discrete(labels = substr(month_names, 1, 3)) +
      labs(x = NULL, y = "Sightings") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none",
            panel.grid.major.x = element_blank(),
            axis.text.x = element_text(size = 8),
            plot.background = element_rect(fill = "white", colour = NA),
            panel.background = element_rect(fill = "white", colour = NA))
  }

  build_weather_plot <- function() {
    d <- with_weather() |>
      filter(!is.na(temp)) |>
      mutate(month_name = factor(month_names[month], levels = month_names))
    if (nrow(d) == 0) {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "No weather data for current selection",
                 colour = "#888", size = 4) +
        theme_void(base_size = 11)
    } else {
      ggplot(d, aes(x = month_name, y = temp)) +
        geom_boxplot(fill = "#5C8A4A", colour = "#2D5A27",
                     alpha = 0.6, outlier.size = 1) +
        scale_x_discrete(labels = substr(month_names, 1, 3)) +
        labs(x = NULL, y = "Temperature (C)") +
        theme_minimal(base_size = 11) +
        theme(panel.grid.major.x = element_blank(),
              axis.text.x = element_text(size = 8),
              plot.background = element_rect(fill = "white", colour = NA),
              panel.background = element_rect(fill = "white", colour = NA))
    }
  }

  build_year_plot <- function() {
    d <- base_data() |> count(year)
    ggplot(d, aes(x = year, y = n)) +
      geom_area(fill = "#2D5A27", alpha = 0.2) +
      geom_line(colour = "#2D5A27", linewidth = 1) +
      geom_point(colour = "#7AB648", size = 2.5) +
      labs(x = NULL, y = "Sightings") +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = "white", colour = NA),
            panel.background = element_rect(fill = "white", colour = NA))
  }

  # Render outputs 
  output$map_header <- renderUI({
    icon <- organism_icons[[input$organism]]
    sel  <- names(organisms)[organisms == input$organism]
    mo   <- if (as.integer(input$month) == 0) "All Year"
            else month_names[as.integer(input$month)]
    tags$span(
      tags$span(class = "organism-icon", icon), sel,
      tags$small(style = "color:#888; margin-left:8px;", mo)
    )
  })

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 4, maxZoom = 12)) |>
      addProviderTiles("CartoDB.Positron",
                       options = tileOptions(opacity = 0.85)) |>
      setView(lng = 134, lat = -25, zoom = 4) |>
      setMaxBounds(lng1 = 105, lat1 = -48, lng2 = 160, lat2 = -8)
  })

  observe({
    d <- filtered()
    leafletProxy("map", data = d) |>
      clearMarkers() |>
      addCircleMarkers(
        lng = ~obs_lon, lat = ~obs_lat,
        radius = 5, color = "#2D5A27",
        fillColor = "#7AB648", fillOpacity = 0.7, weight = 1,
        popup = ~paste0(
          "<div style='font-family:serif;'>",
          "<strong>", format(as.Date(date), "%d %b %Y"), "</strong><br>",
          "<span style='color:#666;font-size:0.85em;'>", record_type, "</span><br>",
          "<span style='color:#666;font-size:0.85em;'>Station: ", ws_id, "</span>",
          "</div>"
        )
      )
  })

  output$stats_boxes <- renderUI({
    d <- filtered()
    tagList(
      tags$div(class = "stat-box",
        tags$div(class = "stat-value", format(nrow(d), big.mark = ",")),
        tags$div(class = "stat-label", "Total Sightings")
      ),
      tags$div(class = "stat-box",
        tags$div(class = "stat-value", length(unique(d$year))),
        tags$div(class = "stat-label", "Years of Data")
      )
    )
  })

  output$best_time_ui <- renderUI({
    peaks <- peak_months()
    peak_labels <- month_names[peaks]
    wdata <- base_data() |>
      left_join(weather |> select(ws_id, date, temp, prcp),
                by = c("ws_id", "date")) |>
      filter(!is.na(temp), month %in% peaks)
    avg_temp <- if (nrow(wdata) > 0) round(mean(wdata$temp, na.rm = TRUE), 1) else "N/A"
    avg_prcp <- if (nrow(wdata) > 0) round(mean(wdata$prcp, na.rm = TRUE), 1) else "N/A"
    tags$div(
      class = "best-time-card",
      tags$h6("Peak Months"),
      tags$div(lapply(peak_labels, function(m) tags$span(class = "month-badge", m))),
      tags$hr(style = "border-color:rgba(255,255,255,0.1); margin:0.7rem 0"),
      tags$h6("Typical Conditions"),
      tags$div(style = "font-size:0.85rem; color:#C8E89A;",
        paste0("Avg temp: ", avg_temp, " C"), tags$br(),
        paste0("Avg rainfall: ", avg_prcp, " mm/day")
      )
    )
  })

  output$month_chart        <- renderPlot({ build_month_plot()   }, bg = "transparent")
  output$weather_temp_chart <- renderPlot({ build_weather_plot() }, bg = "transparent")
  output$year_chart         <- renderPlot({ build_year_plot()    }, bg = "transparent")

  # Download handlers
  dl_plot <- function(plot_fn, filename) {
    downloadHandler(
      filename = function() paste0(filename, "_", Sys.Date(), ".png"),
      content  = function(file) {
        ggplot2::ggsave(file, plot = plot_fn(), width = 8, height = 5,
                        dpi = 150, bg = "white")
      }
    )
  }

  output$dl_month   <- dl_plot(build_month_plot,   "sightings_by_month")
  output$dl_weather <- dl_plot(build_weather_plot, "weather_at_sighting")
  output$dl_year    <- dl_plot(build_year_plot,    "sightings_over_years")

  # Map download — exports current filtered sightings as CSV
  output$dl_map <- downloadHandler(
    filename = function() paste0("sightings_map_", input$organism, "_", Sys.Date(), ".csv"),
    content  = function(file) {
      write.csv(
        filtered() |> select(date, obs_lat, obs_lon, record_type, ws_id),
        file, row.names = FALSE
      )
    }
  )
}

shinyApp(ui, server)
