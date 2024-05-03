# home.r
home_ui <- h3("Welcome to the Home Page")

home_server <- function(input, output, session) {
  output$content <- renderText("This is the content of the Home page.")
}
