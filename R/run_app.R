library(shiny)
library(shinydashboard)

CAGApp <- function(...) {
  shinyApp(ui = ui, server = server)
}

