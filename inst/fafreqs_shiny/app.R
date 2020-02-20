library(shiny)
library(fafreqs)
library(gezellig)
library(markdown)

ui <- fluidPage(
  titlePanel("fafreqs demo"),
  tabsetPanel(
    tabPanel(
      title = "Explore",
      tags$div(
        style = "margin-top: 10px",
        sidebarLayout(
          sidebarPanel = sidebarPanel(
            fafreqs_widget_input("demo_loader",
                                 allow_scaling = TRUE,
                                 allow_rare_allele = TRUE,
                                 allow_marker_filtering = TRUE)),
          mainPanel = mainPanel(
            verbatimTextOutput("freqt_description"),
            div(
              tableOutput("table"),
              class = "table-responsive"
            )
          )
        )
      )
    ),

    #### Export panel
    tabPanel(
      title = "Export",
      fluidRow(
        column(
          width = 4,
          h3("Export to table file"),
          wellPanel(
            selectInput(
              "preset_table_export",
              "Target software",
              choices = c(
                "EuroForMix v1.11" = "euroformix",
                "CaseSolver v1.6" = "casesolver",
                "LRmix Studio v2.1.5" = "lrmixstudio",
                "relMix v1.3" = "relmix",
                "Custom" = "custom"
              )
            ),
            helpText("Choose one of the preset configurations above or adjust the options yourself. To select the exported markers or apply normalisation to the frequencies go to the Explore tab."),
            radioButtons(
              "orientation",
              label = "Table orientation",
              choices = c(
                "One marker per row and one allele per column" = "alleles_in_columns",
                "One allele per row and one marker per column" = "alleles_in_rows"
              )
            ),
            radioButtons(
              "field_separator",
              "Field separator",
              choices = c(
                "Comma (,) - CSV" = ",",
                "Semicolon (;)" = ";",
                "Space ( )" = " ",
                "Tab (\\t) - TSV" = "\t"
              )
            ),
            downloadButton("exported_table_download",
                           "Export")
          )
        ),

        column(
          width = 4,
          h3("Export to other formats"),
          wellPanel(
            downloadButton("exported_familias_download",
                           "Export to Familias")
          )
        )
      )
    ),

    #### About panel
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

  # change settings when selecting a preset export format
  observeEvent(input$preset_table_export, {
    csv_ar <- c("euroformix", "casesolver", "lrmixstudio")
    tsv_ar <- c("relmix")
    if (input$preset_table_export %in% csv_ar) {
      isolate({
        updateRadioButtons(session, "orientation", selected = "alleles_in_rows")
        updateRadioButtons(session, "field_separator", selected = ",")
      })
    } else if (input$preset_table_export %in% tsv_ar) {
      isolate({
        updateRadioButtons(session, "orientation", selected = "alleles_in_rows")
        updateRadioButtons(session, "field_separator", selected = "\t")
      })
    }
  })

  output$exported_table_download <- downloadHandler(
    filename = function() {
      if (input$field_separator %in% c(",", ";"))
        "frequencies.csv"
      else
        "frequencies.txt"
    },
    content = function(file) {
      df <- if (input$orientation == "alleles_in_rows") {
        df <- t(as.data.frame(freqt()))
        rownames_to_column(df, "Allele")
      } else {
        rownames_to_column(as.data.frame(freqt()), "Marker")
      }
      write.table(df, file, sep = input$field_separator, row.names = FALSE)
    }
  )

  output$exported_familias_download <- downloadHandler(
    filename = "frequencies.txt",
    content = function(file) {
      write_familias(freqt(), file)
    }
  )
}

shinyApp(ui = ui, server = server)
