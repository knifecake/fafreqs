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

available_datasets <- list('pop.STR - Europe (All)' = 'ft_popstr_europe',
                           'pop.STR - NW Spain' = 'ft_popstr_nw_spain',
                           'pop.STR - Israel (Carmel) - Druze' = 'ft_popstr_israel_carmel_druze',
                           'STRidER - Austria' = 'ft_strider_austria',
                           'STRidER - Belgium' = 'ft_strider_belgium',
                           'STRidER - Bosnia and Herzegowina' = 'ft_strider_bosnia_herzegowina',
                           'STRidER - Czech Republic' = 'ft_strider_czech_republic',
                           'STRidER - Denmark' = 'ft_strider_denmark',
                           'STRidER - Finland' = 'ft_strider_finland',
                           'STRidER - France' = 'ft_strider_france',
                           'STRidER - Germany' = 'ft_strider_germany',
                           'STRidER - Greece' = 'ft_strider_greece',
                           'STRidER - Hungary' = 'ft_strider_hungary',
                           'STRidER - Ireland' = 'ft_strider_ireland',
                           'STRidER - Montenegro' = 'ft_strider_montenegro',
                           'STRidER - Norway' = 'ft_strider_norway',
                           'STRidER - Poland' = 'ft_strider_poland',
                           'STRidER - Slovakia' = 'ft_strider_slovakia',
                           'STRidER - Slovenia' = 'ft_strider_slovenia',
                           'STRidER - Spain' = 'ft_strider_spain',
                           'STRidER - Sweden' = 'ft_strider_sweden',
                           'STRidER - Switzerland' = 'ft_strider_switzerland',
                           'Custom' = 'custom')

available_markersets <- list('Core 23' = 'core23',
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

  # Dataset input
  selectInput("preset_dataset", "Select a preset dataset", available_datasets),
  fileInput("custom_fam_file", "or load a Familias frequency file."),
  p(shinyTableFileLoaderInput('custom_freqt_file', 'or load your own frequency table')),

  # Marker input
  selectInput('markerset_preset', 'Marker presets', available_markersets),
  textOutput('included_markers'),
  tags$div(align = 'left', class = 'multicol',
           checkboxGroupInput('markerset', 'Markers')
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # User provided frequency tables
  custom_freqt <- callModule(shinyTableFileLoader, 'custom_freqt_file', id = 'custom_freqt_file',
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
  output$included_markers <- reactive({
    selected_markers = length(markers(dataset()))
    total_markers = length(markers(selected_dataset()))
    sprintf("%d out of %d markers selected", selected_markers, total_markers)
  })

}

# Run the application
shinyApp(ui = ui, server = server)
