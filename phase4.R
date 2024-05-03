library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(leaflet)

ui <- fluidPage(
  titlePanel("Phase 4: Economic Impact Analysis Based on NTL Data"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dateRange", "Select Date Range:",
                     start = Sys.Date() - 365, end = Sys.Date()),
      actionButton("btn_process", "Process Data")
    ),
    mainPanel(
      leafletOutput("mapVisual"),
      plotOutput("impactPlot")
    )
  )
)

server <- function(input, output) {
  # Reactive value to store processed data
  processed_data <- reactiveVal()
  
  observeEvent(input$btn_process, {
    # Example: Fetching and processing data
    battle_dates <- seq(as.Date(input$dateRange[1]), as.Date(input$dateRange[2]), by = "day")
    previous_dates <- seq(as.Date(input$dateRange[1]) - 365, as.Date(input$dateRange[2]) - 365, by = "day")
    
    # Simulate fetching NTL data for specified dates
    sol_battle <- simulate_ntl_data(battle_dates)  # Replace with actual data fetching function
    sol_previous_year <- simulate_ntl_data(previous_dates)  # Replace with actual data fetching function
    
    # Assume fixed_GDP_SOLxDUM_NC_X is available globally or obtained from another reactive context
    result <- process_SOL_summaries(sol_battle, sol_previous_year, fixed_GDP_SOLxDUM_NC_X, battle_dates)
    processed_data(result)
  })
  
  output$mapVisual <- renderLeaflet({
    req(processed_data())
    data <- processed_data()
    leaflet(data) %>%
      addTiles() %>%
      addPolygons(fillColor = ~colorQuantile("YlOrRd", EstGDPdelta)(EstGDPdelta), fillOpacity = 0.5)
  })
  
  output$impactPlot <- renderPlot({
    req(processed_data())
    data <- processed_data()
    ggplot(data, aes(x = NAME_1, y = EstGDPdelta, fill = EstGDPdelta)) +
      geom_col() +
      labs(title = "Estimated GDP Change by Region",
           x = "Region", y = "Estimated GDP Change") +
      theme_minimal()
  })
}

server <- function(input, output) {
  # Reactive value to store processed data
  processed_data <- reactiveVal()
  
  observeEvent(input$btn_process, {
    # Example: Fetching and processing data
    battle_dates <- seq(as.Date(input$dateRange[1]), as.Date(input$dateRange[2]), by = "day")
    previous_dates <- seq(as.Date(input$dateRange[1]) - 365, as.Date(input$dateRange[2]) - 365, by = "day")
    
    # Simulate fetching NTL data for specified dates
    sol_battle <- simulate_ntl_data(battle_dates)  # Replace with actual data fetching function
    sol_previous_year <- simulate_ntl_data(previous_dates)  # Replace with actual data fetching function
    
    # Assume fixed_GDP_SOLxDUM_NC_X is available globally or obtained from another reactive context
    result <- process_SOL_summaries(sol_battle, sol_previous_year, fixed_GDP_SOLxDUM_NC_X, battle_dates)
    processed_data(result)
  })
  
  output$mapVisual <- renderLeaflet({
    req(processed_data())
    data <- processed_data()
    leaflet(data) %>%
      addTiles() %>%
      addPolygons(fillColor = ~colorQuantile("YlOrRd", EstGDPdelta)(EstGDPdelta), fillOpacity = 0.5)
  })
  
  output$impactPlot <- renderPlot({
    req(processed_data())
    data <- processed_data()
    ggplot(data, aes(x = NAME_1, y = EstGDPdelta, fill = EstGDPdelta)) +
      geom_col() +
      labs(title = "Estimated GDP Change by Region",
           x = "Region", y = "Estimated GDP Change") +
      theme_minimal()
  })
}

# Simulate data fetching function
simulate_ntl_data <- function(dates) {
  # This is a placeholder function. Replace it with actual data fetching logic.
  data.frame(
    NAME_1 = sample(c("Region1", "Region2"), 10, replace = TRUE),
    date = sample(dates, 10, replace = TRUE),
    n_non_na_pixels = runif(10, 100, 200),
    ntl_mean = runif(10, 0.5, 1.5)
  )
}

# Function to process SOL summaries and calculate economic impacts
process_SOL_summaries <- function(sol_battle, sol_previous_year, model, battle_dates) {
  # Your existing function logic here
}
