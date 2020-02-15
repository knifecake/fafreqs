library(shiny)
library(fafreqs)

# Define UI for application that draws a histogram
ui <- fluidPage(
  fafreqs_widget_input("demo_loader", allow_marker_filtering = F)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  freqt <- callModule(fafreqs_widget, "demo_loader", id = "demo_loader")

  observe({
    print(freqt())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
