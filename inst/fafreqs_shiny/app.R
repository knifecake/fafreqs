library(shiny)
library(fafreqs)
library(gezellig)

ui <- fluidPage(
  fafreqs_widget_input("demo_loader", allow_marker_filtering = T)
)

server <- function(input, output, session) {

  freqt <- callModule(fafreqs_widget, "demo_loader", id = "demo_loader")
}

shinyApp(ui = ui, server = server)
