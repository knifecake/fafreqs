library(shiny)
library(fafreqs)
library(gezellig)
library(markdown)

tweaks <- tags$head(
  tags$style(
    HTML(".btn-list .btn {
         width: 100%;
         margin-bottom: 1rem;
         display: block;
         }")
  )
)

ui <- fluidPage(
  tweaks,
  titlePanel("fafreqs - Forensic allele frequency databases"),
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
          h3("Export to other software"),
          tags$div(class = "btn-list",
            downloadButton("download_familias", "Export to Familias"),
            downloadButton("download_euroformix", "Export to EuroForMix"),
            downloadButton("download_casesolver", "Export to CaseSolver"),
            downloadButton("download_lrmixstudio", "Export to LRmixStudio"),
            downloadButton("download_relmix", "Export to relMix")
          )
        ),
        column(
          width = 4,
          h3("Export to a table file"),
          wellPanel(
            helpText("To select the exported markers or apply normalisation to the frequencies go to the Explore tab."),
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
            checkboxInput("et_sample_size", "Include sample sizes"),
            downloadButton("download_csv", "Download as CSV")
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

  ### Export to other programs
  output$download_familias <- downloadHandler(
    filename = "frequencies.txt",
    content = function(file) {
      write_familias(freqt(), file)
    }
  )

  download_csv_ar <- downloadHandler(
    filename = "frequencies.csv",
    content = function(file) {
      df <- t(as.data.frame(freqt()))
      df <- rownames_to_column(df, "Allele")
      write.table(df, file, sep = ",", row.names = FALSE, na = "0")
    }
  )

  download_tsv_ar <- downloadHandler(
    filename = "frequencies.tsv",
    content = function(file) {
      df <- t(as.data.frame(freqt()))
      df <- rownames_to_column(df, "Allele")
      write.table(df, file, sep = "\t", row.names = FALSE, na = "0")
    }
  )

  output$download_euroformix <- download_csv_ar
  output$download_casesolver <- download_csv_ar
  output$download_lrmixstudio <- download_csv_ar
  output$download_relmix <- download_tsv_ar


  ### Table downloader
  table_for_download <- reactive({
    # save the frequency table to avoid reloading it all the time
    ft <- freqt()

    # generated table
    df <- as.data.frame(ft)

    # include sample sizes
    if (isTruthy(input$et_sample_size) && !is.null(ft$SAMPLE_SIZES)) {
      df$N <- as.numeric(ft$SAMPLE_SIZES)
    }

    # transpose table
    if (input$orientation == "alleles_in_rows") {
      rownames_to_column(t(df), "Alleles")
    } else {
      rownames_to_column(df, "Markers")
    }
  })

  output$download_csv <- downloadHandler(
    filename = "frequencies.csv",
    content = function(file) {
      write.table(table_for_download(), file, sep = ",", row.names = FALSE, na = "0")
    }
  )
}

shinyApp(ui = ui, server = server)
