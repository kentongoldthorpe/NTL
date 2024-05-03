library(shiny)

# Source phase scripts
source("home.r")
source("phase1.r")
source("phase2.r")
source("phase3.r")
source("phase4.r")

# Define UI
ui <- fluidPage(
  titlePanel("Multi-Phase Shiny App"),
  sidebarLayout(
    sidebarPanel(
      # Input: Specify navigation tabs
      tabsetPanel(
        id = "tabs",
        tabPanel("Home", home_ui),
        tabPanel("Phase 1", phase1_ui),
        tabPanel("Phase 2", phase2_ui),
        tabPanel("Phase 3", phase3_ui),
        tabPanel("Phase 4", phase4_ui)
      )
    ),
    mainPanel(
      textOutput("content")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Call server functions from phase scripts
  home_server(input, output, session)
  phase1_server(input, output, session)
  phase2_server(input, output, session)
  phase3_server(input, output, session)
  phase4_server(input, output, session)
}

# Run the application
shinyApp(ui = ui, server = server)
