library(shiny)
library(DT)

# Updated UI for file upload, sheet selection, and data preview with tabbed panels
phase1_ui <- fluidPage(
  titlePanel("Phase 1: Upload and Process GDP Data"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose Excel File", accept = c(".xlsx")),
      uiOutput("sheetSelectUI"),  # Dynamic sheet selection
      numericInput("skipRows", "Number of rows to skip:", value = 0, min = 0),
      actionButton("loadData", "Load Data"),
      actionButton("processData", "Process Data"),  # Separate button for processing
      downloadButton("downloadProcessed", "Download Processed Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Raw Data", DTOutput("viewData")),  # Display raw data
        tabPanel("Processed Data", DTOutput("processedData"))  # Display processed data
      )
    )
  )
)

# Server logic for dynamic Excel sheet reading and data processing
phase1_server <- function(input, output, session) {
  # Reactive value for sheets and data
  sheets <- reactiveVal()
  
  # Observe file input and update sheet selection UI
  observe({
    req(input$file1)
    file_sheets <- excel_sheets(input$file1$datapath)
    sheets(file_sheets)
    updateSelectInput(session, "sheetSelected", choices = file_sheets)
  })
  
  # Dynamic UI for selecting the sheet
  output$sheetSelectUI <- renderUI({
    req(sheets())
    selectInput("sheetSelected", "Select Sheet:", choices = sheets())
  })
  
  # Load data when requested
  raw_data <- eventReactive(input$loadData, {
    req(input$file1)
    read_excel(input$file1$datapath, sheet = input$sheetSelected, skip = input$skipRows)
  })
  
  # Display raw data
  output$viewData <- renderDT({
    datatable(raw_data(), options = list(pageLength = 5, scrollX = TRUE))
  }, server = TRUE)
  
  # Process data when requested
  processed_data <- eventReactive(input$processData, {
    req(raw_data())  # Ensure data is loaded before processing
    data <- raw_data()
    # Implement data processing logic here
    data  # Placeholder for processed data
  })
  
  # Display processed data
  output$processedData <- renderDT({
    datatable(processed_data(), options = list(pageLength = 5, scrollX = TRUE))
  }, server = TRUE)
  
  # Download handler for processed data
  output$downloadProcessed <- downloadHandler(
    filename = function() { "processed_data.csv" },
    content = function(file) {
      write.csv(processed_data(), file, row.names = FALSE)
    }
  )
}
