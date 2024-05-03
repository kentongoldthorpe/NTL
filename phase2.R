library(shiny)
library(leaflet)
library(sf)

# Define Server logic for Phase 2
phase2_server <- function(input, output, session) {
  # Reactive value to hold the GADM data
  gadm_data <- reactiveVal()
  
  observeEvent(input$loadData, {
    req(input$countryCode)  # Ensure a country code is entered
    
    # Attempt to fetch GADM data for the specified country
    tryCatch({
      country_data <- gadm(country = input$countryCode, level = 1, path = tempdir()) %>% 
        st_as_sf()
      gadm_data(country_data)  # Store the GADM data if successful
    }, error = function(e) {
      # Error handling if the GADM data cannot be fetched
      showNotification("Failed to load GADM data. Please check the country code and ensure internet connectivity.", type = "error")
    })
  })
  
  # Render the Leaflet map
  output$gadmMap <- renderLeaflet({
    req(gadm_data())  # Ensure GADM data is loaded
    if (ncol(gadm_data()) > 0 && "some_column" %in% names(gadm_data())) {  # Replace 'some_column' with your actual column
      leaflet(gadm_data()) %>%
        addTiles() %>%  # Add default OpenStreetMap tiles
        addPolygons(fillColor = ~colorQuantile("YlOrRd", some_column)(some_column), fillOpacity = 0.5, color = "#444444", weight = 1)  # Color by an actual column
    } else {
      leaflet(gadm_data()) %>%
        addTiles() %>%  # Fallback if no suitable column found
        addPolygons(fillColor = "blue", fillOpacity = 0.5, color = "#444444", weight = 1)
    }
  })
}
