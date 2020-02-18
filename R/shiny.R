library("shiny")


#' Frequency database loader input
#'
#' @param id a unique identifier per shiny"s documentation
#' @param allow_fafreqs_markersets allow the user to select one of the freqyency
#'   tables included in the \link{fafreqs} package
#' @param allow_import_familias allow the user to load a familias frequency file
#' @param allow_import_csv allow the user to load a custom frequency table
#' @param allow_marker_filtering allow the user to select which markers are to
#'   be used out of those present in the selected (or uploaded) frequency
#'   database
#'
#' @export
fafreqs_widget_input <- function(id,
                                 allow_fafreqs_markersets = TRUE,
                                 allow_import_familias = TRUE,
                                 allow_import_csv = TRUE,
                                 allow_marker_filtering = TRUE) {

  # create a namespace to avoid name collisions with other modules / other
  # instances of this one
  ns <- NS(id)

  marker_input <- tagList(
    selectInput(ns("markerset_preset"), "Marker presets", available_markersets),
    textOutput(ns("included_markers")),
    tags$div(align = "left", class = "multicol",
             checkboxGroupInput(ns("markerset"), "Markers"))
  )

  tagList(
    tweaks,

    # Dataset input
    if (allow_fafreqs_markersets)
      selectInput(ns("preset_dataset"), "Select a preset dataset", available_datasets),
    if (allow_import_familias)
      fileInput(ns("custom_fam_file"), "or load a Familias frequency file"),
    if (allow_import_csv)
      tabular_data_loader_input(ns("custom_freqt_file"),
                                allow_header_toggle = TRUE,
                                allow_rownames_toggle = TRUE,
                                na_string = NULL,
                                allowed_field_delimiters = NULL),

    # Marker input
    if (allow_marker_filtering)
      marker_input,
    tags$div()
  )
}

#' Frequency database loader
#'
#' Use with the \code{\link{callModule}} function.
#'
#' @param input the shiny input object
#' @param output the shiny output object
#' @param session the shiny session object
#' @param id the id given to the input
#'
#' @export
#'
fafreqs_widget <- function(input, output, session, id) {

  # Familias frequency tables
  familias_freqt <- reactive({
    if (isTruthy(input$custom_fam_file)) {
      read_familias(input$custom_fam_file$datapath, "Familias")
    }
  })

  custom_df <- callModule(tabular_data_loader, "custom_freqt_file")

  # User provided frequency tables
  custom_freqt <- reactive({
    print(custom_df())
    if (isTruthy(custom_df()))
      freqt(custom_df(), name = "Custom")
    else
      NULL
  })

  observe({
    print(custom_freqt())
    if (isTruthy(custom_freqt())) {
      updateSelectInput(session, "preset_dataset", selected = "custom")
    }
  })

  observe({
    if (isTruthy(familias_freqt())) {
      updateSelectInput(session, "preset_dataset", selected = "familias")
    }
  })

  # Raw dataset we are working with (either a preset one or a custom one)
  selected_dataset <- reactive({
    if (input$preset_dataset == "custom") {
      custom_freqt()
    } else if (input$preset_dataset == "familias") {
      familias_freqt()
    } else {
      eval(parse(text = input$preset_dataset))
    }
  })

  # Update the list of markers when the raw dataset changes
  observe({
    if (isTruthy(input$markerset_preset)) {
      selected_markers <- markers(selected_dataset())
      if (input$markerset_preset != "all") {
        selected_markers <- eval(parse(text = input$markerset_preset))
      }
      updateCheckboxGroupInput(session, "markerset",
                               choiceNames = markers(selected_dataset()),
                               choiceValues = markers(selected_dataset()),
                               selected = selected_markers)
    }
  })

  # Output dataset
  dataset <- reactive({
    markers <- markers(selected_dataset())

    if (isTruthy(input$markerset)) {
        markers <- input$markerset
    }
    filter_markers(selected_dataset(), markers)
  })

  # Number of included markers output message
  output$included_markers <- renderText({
    selected_markers <- length(markers(dataset()))
    total_markers <- length(markers(selected_dataset()))
    sprintf("%d out of %d markers selected", selected_markers, total_markers)
  })

  dataset
}



###### Static
available_datasets <- list("pop.STR - Europe (All)" = "ft_popstr_europe",
                           "pop.STR - NW Spain" = "ft_popstr_nw_spain",
                           "pop.STR - Israel (Carmel) - Druze" = "ft_popstr_israel_carmel_druze",
                           "STRidER - Austria" = "ft_strider_austria",
                           "STRidER - Belgium" = "ft_strider_belgium",
                           "STRidER - Bosnia and Herzegowina" = "ft_strider_bosnia_herzegowina",
                           "STRidER - Czech Republic" = "ft_strider_czech_republic",
                           "STRidER - Denmark" = "ft_strider_denmark",
                           "STRidER - Finland" = "ft_strider_finland",
                           "STRidER - France" = "ft_strider_france",
                           "STRidER - Germany" = "ft_strider_germany",
                           "STRidER - Greece" = "ft_strider_greece",
                           "STRidER - Hungary" = "ft_strider_hungary",
                           "STRidER - Ireland" = "ft_strider_ireland",
                           "STRidER - Montenegro" = "ft_strider_montenegro",
                           "STRidER - Norway" = "ft_strider_norway",
                           "STRidER - Poland" = "ft_strider_poland",
                           "STRidER - Slovakia" = "ft_strider_slovakia",
                           "STRidER - Slovenia" = "ft_strider_slovenia",
                           "STRidER - Spain" = "ft_strider_spain",
                           "STRidER - Sweden" = "ft_strider_sweden",
                           "STRidER - Switzerland" = "ft_strider_switzerland",
                           "From familias file" = "familias",
                           "Custom" = "custom")

available_markersets <- list("All" = "all",
                             "Core 23" = "core23",
                             "Core 23 + D6S1043" = "core24",
                             "Illumina ForenSeq" = "illumina_forenseq",
                             "Qiagen Investigator HDplex" = "qiagen_investigator",
                             "Promega CS7" = "promega_cs7",
                             "USC AIM-STRs" = "usc_aim",
                             "NIST Mini-STRs" = "nist_mini")

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
