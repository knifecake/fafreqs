#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(fafreqs)

available_datasets = list('pop.STR - Europe (All)' = 'ft_popstr_europe',
                          'pop.STR - NW Spain' = 'ft_popstr_nw_spain',
                          'pop.STR - Israel (Carmel) - Druze' = 'ft_popstr_israel_carmel_druze',
                          'Custom' = 'custom')

available_markersets = list('Core 23' = 'core23',
                            'Core 23 + D6S1043' = 'core24',
                            'Illumina ForenSeq' = 'illumina_forenseq',
                            'Qiagen Investigator HDplex' = 'qiagen_investigator',
                            'Promega CS7' = 'promega_cs7',
                            'USC AIM-STRs' = 'usc_aim',
                            'NIST Mini-STRs' = 'nist_mini')

tweaks <-
  list(tags$head(tags$style(HTML("
                                 .multicol {
                                 //height: 150px;
                                 -webkit-column-count: 3; /* Chrome, Safari, Opera */
                                 -moz-column-count: 3;    /* Firefox */
                                 column-count: 3;
                                 -moz-column-fill: auto;
                                 -column-fill: auto;
                                 }
                                 "))
  ))

# Define UI for application that draws a histogram
ui <- fluidPage(
  tweaks,
  selectInput("preset_dataset", "Preset dataset", available_datasets),
  p(shinyTableFileLoaderInput('custom_freqt_file', 'or load your own frequency table')),
  selectInput('markerset_preset', 'Marker presets', available_markersets),
  textOutput('included_markers'),
  tags$div(align = 'left', class = 'multicol',
           checkboxGroupInput('markerset', 'Markers')
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # User provided frequency tables
  custom_freqt = callModule(shinyTableFileLoader, 'custom_freqt_file', id = 'custom_freqt_file',
                            columnHeaders = TRUE,
                            rowHeaders = TRUE)

  observe({
    if (isTruthy(custom_freqt())) {
      updateSelectInput(session, 'preset_dataset', selected = 'custom')
    }
  })

  # Raw dataset we are working with (either a preset one or a custom one)
  selected_dataset <- reactive({
    if (input$preset_dataset == 'custom') {
      freqt(custom_freqt(), name = 'Custom')
    } else {
      eval(parse(text = input$preset_dataset))
    }
  })

  # Update the list of markers when the raw dataset changes
  observe({
    print(markers(selected_dataset()))
    updateCheckboxGroupInput(session, 'markerset',
                             choiceNames = markers(selected_dataset()),
                             choiceValues = markers(selected_dataset()),
                             selected = eval(parse(text = input$markerset_preset)))
  })

  # Output dataset
  dataset <- reactive({
    filter_markers(selected_dataset(), input$markerset)
  })

  # Number of included markers output message
  output$included_markers = reactive({
    selected_markers = length(markers(dataset()))
    total_markers = length(markers(selected_dataset()))
    sprintf("%d out of %d markers selected", selected_markers, total_markers)
  })

}

# Run the application
shinyApp(ui = ui, server = server)

