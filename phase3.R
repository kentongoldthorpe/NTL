library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(stargazer)

ui <- fluidPage(
  titlePanel("Phase 3: Data Analysis and Visualization"),
  sidebarLayout(
    sidebarPanel(
      actionButton("btn_analyze", "Analyze Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Regression Summary", verbatimTextOutput("reg_summary")),
        tabPanel("Fitted vs Actual Plot", plotOutput("plot_fitted_vs_actual")),
        tabPanel("Data Table", DTOutput("data_table"))
      )
    )
  )
)

server <- function(input, output) {
  # Assume ntl_df_combined is available from global environment or passed reactively
  # You would set this up to be reactive based on inputs from Phase 1 and 2
  
  observeEvent(input$btn_analyze, {
    # Perform regressions and other computations here
    # This is a simplification of your provided code
    
    ntl_df_combined <- reactive({
      # Mock data setup; replace with actual data manipulation
      # Example:
      data.frame(
        NL_NAME_1 = c("Region1", "Region2"),
        date = c(2017, 2017),
        GDP = c(1000, 2000),
        SOL = c(500, 600),
        n_non_na_pixels = c(100, 150),
        ntl_mean = c(5, 4)
      ) %>% 
        mutate(SOL = n_non_na_pixels * ntl_mean)
    })
    
    # Create models and perform analysis
    fitted_models <- list(
      lm(GDP ~ SOL, data=ntl_df_combined())
    )
    
    # Output the stargazer summary
    output$reg_summary <- renderPrint({
      stargazer(fitted_models, type = "text")
    })
    
    # Plotting
    output$plot_fitted_vs_actual <- renderPlot({
      ggplot(ntl_df_combined(), aes(x = GDP, y = SOL)) + geom_point()
    })
    
    # Data table
    output$data_table <- renderDT({
      datatable(ntl_df_combined())
    })
  })
}

shinyApp(ui, server)
