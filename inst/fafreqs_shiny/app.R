library(shiny)
library(fafreqs)
library(gezellig)

ui <- fluidPage(
  titlePanel("fafreqs demo"),
  tabsetPanel(
    tabPanel(
      title = "Main",
      tags$div(
        style = "margin-top: 10px",
        sidebarLayout(
          sidebarPanel = sidebarPanel(
            fafreqs_widget_input("demo_loader",
                                 allow_scaling = TRUE,
                                 allow_rare_allele = TRUE,
                                 allow_marker_filtering = TRUE)),
          mainPanel = mainPanel(
            p(
              downloadButton("download_csv", "Download CSV"),
              downloadButton("download_familias", "Download Familias")
            ),
            verbatimTextOutput("freqt_description"),
            div(
              tableOutput("table"),
              class = "table-responsive"
            )
          )
        )
      )
    ),
    tabPanel(
      title = "About",
      includeMarkdown("about.md")
    )
  )
)

server <- function(input, output, session) {

  freqt <- callModule(fafreqs_widget, "demo_loader")

  output$table <- renderTable({
    as.data.frame(freqt())
  }, striped = TRUE, rownames = TRUE, digits = 5)

  output$freqt_description <- renderPrint({
    print(freqt())
  })

  output$download_csv <- downloadHandler(
    filename = "allele_frequencies.csv",
    content = function(file) {
      write_csv(freqt(), file)
    }
  )

  output$download_familias <- downloadHandler(
    filename = "allele_frequencies.txt",
    content = function(file) {
      write_familias(freqt(), file)
    }
  )
}

shinyApp(ui = ui, server = server)
