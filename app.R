library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ecotourism)

# Data prep 
organisms <- list(
  "Gouldian Finch" = "gouldian_finch",
  "Manta Ray"      = "manta_rays",
  "Glowworms"      = "glowworms",
  "Orchids"        = "orchids"
)

month_names <- setNames(1:12, month.name)

# UI
ui <- page_sidebar(
  title = "Australian Wildlife Explorer",
  theme = bs_theme(bootswatch = "flatly", primary = "#2E7D32"),
  
  sidebar = sidebar(
    width = 280,
    selectInput("organism", "🦜 Organism",
                choices = organisms,
                selected = "gouldian_finch"),
    selectInput("month", "📅 Month",
                choices = month_names,
                selected = 7),
    hr(),
    helpText("Data: Atlas of Living Australia, 2014–2024")
  ),
  
  layout_columns(
    col_widths = c(8, 4),
    
    card(
      card_header("Sighting Locations"),
      leafletOutput("map", height = "500px")
    ),
    
    card(
      card_header("Sightings Over Time"),
      plotOutput("trend_chart", height = "220px"),
      card_header("Weather Summary"),
      tableOutput("weather_summary")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive: get the right dataset based on organism selection
  occurrence_data <- reactive({
    get(input$organism)  # retrieves e.g. gouldian_finch from ecotourism
  })
  
  # Reactive: filter to selected month
  filtered <- reactive({
    occurrence_data() |>
      filter(month == as.integer(input$month))
  })
  
  # Reactive: join with weather
  with_weather <- reactive({
    filtered() |>
      left_join(weather_data, by = c("ws_id", "date"))
  })
  
  # Base map (renders once)
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = 134, lat = -25, zoom = 4)
  })
  
  # Update map markers reactively
  observe({
    d <- filtered()
    leafletProxy("map", data = d) |>
      clearMarkers() |>
      addCircleMarkers(
        lng = ~obs_lon, lat = ~obs_lat,
        radius = 4, color = "#2E7D32",
        fillOpacity = 0.7,
        popup = ~paste0("<b>Date:</b> ", date,
                        "<br><b>Time:</b> ", time)
      )
  })
  
  # Trend chart
  output$trend_chart <- renderPlot({
    filtered() |>
      count(year) |>
      ggplot(aes(x = year, y = n)) +
      geom_line(color = "#2E7D32", linewidth = 1) +
      geom_point(color = "#2E7D32", size = 2) +
      labs(x = NULL, y = "Sightings") +
      theme_minimal()
  })
  
  # Weather summary table
  output$weather_summary <- renderTable({
    with_weather() |>
      summarise(
        `Avg Max Temp (°C)` = round(mean(temp_max, na.rm = TRUE), 1),
        `Avg Rainfall (mm)` = round(mean(prcp, na.rm = TRUE), 1),
        `Total Sightings`   = n()
      )
  })
}

# Launch 
shinyApp(ui, server)