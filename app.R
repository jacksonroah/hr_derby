# Simple test app.R
library(shiny)

ui <- fluidPage(
  h1("Test App"),
  p("If you see this, the app is working")
)

server <- function(input, output, session) {}

shinyApp(ui, server)