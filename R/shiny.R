#' Frequency database loader input
#'
#' @param id a unique identifier per shiny"s documentation
#' @param allow_fafreqs_markersets allow the user to select one of the freqyency
#'   tables included in the \code{fafreqs} package
#' @param allow_import_familias allow the user to load a familias frequency file
#' @param allow_import_csv allow the user to load a custom frequency table
#' @param allow_marker_filtering allow the user to select which markers are to
#'   be used out of those present in the selected (or uploaded) frequency
#'   database
#' @param allow_scaling allow the user to scale the frequencies so that they sum
#'   up to 1
#' @param allow_rare_allele allow the user to add an allele so that the
#'   frequencies sum up to 1
#' @param allow_standarise_names allow the user to convert marker names to their
#'   standard form
#' @param allow_table_preview allow the user to preview the selected / loaded
#'   frequency table
#' @param allow_waiting_option show a placeholder option before the user makes a
#'   concious decision
#'
#' @importFrom utils data
#' @export
fafreqs_widget_input <- function(id,
                                 allow_fafreqs_markersets = TRUE,
                                 allow_import_familias = TRUE,
                                 allow_import_csv = TRUE,
                                 allow_marker_filtering = TRUE,
                                 allow_scaling = TRUE,
                                 allow_rare_allele = TRUE,
                                 allow_standarise_names = TRUE,
                                 allow_table_preview = TRUE,
                                 allow_waiting_option = TRUE) {

  # create a namespace to avoid name collisions with other modules / other
  # instances of this one
  ns <- shiny::NS(id)

  # prepare available datasets input
  datasets <- available_datasets()
  if (allow_waiting_option)
    datasets <- c("Choose a dataset..." = "", datasets)

  # prepare normalisation input
  normalisation_choices <- c("Off" = "off")
  if (allow_scaling)
    normalisation_choices <- c(normalisation_choices, "Scale up to one" = "scale")
  if (allow_rare_allele)
    normalisation_choices <- c(normalisation_choices, "Add 'rare' allele" = "rogue")

  normalisation_input <- if (allow_scaling || allow_rare_allele) {
    shiny::radioButtons(ns("normalise"), "Frequency normalisation",
                        choices = normalisation_choices)
  } else {
    shiny::tagList()
  }

  # prepare marker input
  marker_input <- if (allow_marker_filtering) {
    shiny::tagList(

      shiny::selectInput(ns("markerset"), "Filter included markers",
                         choices = list(), multiple = TRUE),
      shiny::textOutput(ns("included_markers")),
      shiny::helpText("Select markers to be used in the dataset manually in the box above or use the dropdown menu below to select all markers in one of the standard amplification kits."),
      shiny::selectInput(ns("markerset_preset"),
                         "Marker presets",
                         c("All" = "all",
                           get_marker_kit(just_names = TRUE)))
    )
  } else {
    shiny::tagList()
  }

  shiny::tagList(
    # Dataset input
    if (allow_table_preview)
      shiny::tags$p(shiny::actionLink(ns("table_preview_link"), "Preview frequency table"), class = "text-right"),
    if (allow_fafreqs_markersets)
      shiny::selectInput(ns("preset_dataset"), "Select a preset dataset", datasets),
    if (allow_import_familias)
      shiny::fileInput(ns("custom_fam_file"), "or load a Familias frequency file"),
    if (allow_import_csv)
      shiny::actionLink(ns("open_tabular_modal"), "or load your own table-like file"),
    normalisation_input,
    if (allow_standarise_names)
      shiny::checkboxInput(ns("standarise_names"), "Standarise marker names"),
    marker_input
  )
}

#' Frequency database loader
#'
#' Use with the \code{\link[shiny]{callModule}} function.
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
  familias_freqt <- shiny::reactive({
    if (shiny::isTruthy(input$custom_fam_file)) {
      read_familias(input$custom_fam_file$datapath, "Familias")
    }
  })

  # Custom table loader inside a modal
  shiny::observeEvent(input$open_tabular_modal, {
    shiny::showModal(shiny::modalDialog(
      title = "Load tabular frequency file",
      fade = FALSE,
      shiny::verticalLayout(
        shiny::helpText("Load a CSV, TSV or other tabular file. Specify the column separators and other settings to match the structure of your file. This program expects one marker per row and one allele per column. If your file is arranged the opposite way, tick the 'Transpose table' checkbox to exchange rows and columns."),
        gezellig::tabular_data_loader_input(session$ns("custom_freqt_file"),
                                            allow_header_toggle = TRUE,
                                            allow_rownames_toggle = TRUE,
                                            na_string = NULL,
                                            allowed_field_delimiters = NULL,
                                            ncols = 2),
        shiny::tags$p(" "),
        shiny::textOutput(session$ns("custom_freqt_msgs"),
                          container = shiny::tags$p)
      ),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton(session$ns("load_custom_freqt"), "Load", class = "btn-primary")
      )
    ))
  })

  custom_df_prev <- shiny::callModule(gezellig::tabular_data_loader, "custom_freqt_file")

  # describe the frequency table that would result from loading the current file
  output$custom_freqt_msgs <- shiny::renderText({
    cdfp <- custom_df_prev()
    if (shiny::isTruthy(cdfp)) {
      ms <- rownames(cdfp)
      sprintf("Detected %d markers: %s. If it looks right, click Load.",
              length(ms),
              paste(ms, collapse = ", "))
    } else {
      "Waiting for input."
    }
  })

  # hold frequency table until the user is done adjusting the loading parameters
  custom_df <- shiny::reactive({
    shiny::req(input$load_custom_freqt)
    shiny::removeModal()
    custom_df_prev()
  })

  # User provided frequency tables
  custom_freqt <- shiny::reactive({
    if (shiny::isTruthy(custom_df()))
      freqt(custom_df(), name = "Custom")
    else
      NULL
  })

  shiny::observe({
    if (shiny::isTruthy(custom_freqt())) {
      shiny::updateSelectInput(session, "preset_dataset", selected = "custom")
    }
  })

  shiny::observe({
    if (shiny::isTruthy(familias_freqt())) {
      shiny::updateSelectInput(session, "preset_dataset", selected = "familias")
    }
  })

  # Raw dataset we are working with (either a preset one or a custom one)
  selected_dataset <- shiny::reactive({
    ft <- if (input$preset_dataset == "custom") {
      custom_freqt()
    } else if (input$preset_dataset == "familias") {
      familias_freqt()
    } else {
      eval(parse(text = input$preset_dataset))
    }

    if (shiny::isTruthy(input$standarise_names)) {
      ft <- standarise_names(ft)
    }

    ft
  })

  # Update the list of markers when the raw dataset changes
  shiny::observe({
    if (shiny::isTruthy(input$markerset_preset)) {
      selected_markers <- markers(selected_dataset())
      if (input$markerset_preset != "all") {
        selected_markers <- get_marker_kit(input$markerset_preset)

        if (input$standarise_names)
          selected_markers <- standard_marker_names(selected_markers)
      }
      shiny::updateSelectInput(session, "markerset",
                               choices = markers(selected_dataset()),
                               selected = selected_markers)
    }
  })

  # Number of included markers output message
  output$included_markers <- shiny::renderText({
    selected_markers <- length(markers(dataset()))
    total_markers <- length(markers(selected_dataset()))
    sprintf("%d out of %d markers selected", selected_markers, total_markers)
  })

  # Table preview functionality
  shiny::observeEvent(input$table_preview_link, {
    shiny::showModal(
      shiny::modalDialog(
        title = "Frequency table preview",
        easyClose = TRUE,
        size = "l",
        shiny::helpText("Bear in mind that frequencies are rounded in this preview but they will not be rounded for the calculations."),
        shiny::tags$div(
          shiny::tableOutput(session$ns("table_preview")),
          class = "table-responsive"
        )
      )
    )
  })

  output$table_preview <- shiny::renderTable({
    as.data.frame(selected_dataset())
  }, spacing = "xs", na = "", rownames = TRUE, striped = TRUE)

  # Output dataset
  dataset <- shiny::reactive({
    # filter markers
    markers <- markers(selected_dataset())

    if (shiny::isTruthy(input$markerset)) {
        markers <- input$markerset
    }
    res <- filter_markers(selected_dataset(), markers)

    # scale / add rogue allele
    if (shiny::isTruthy(input$scale) || (shiny::isTruthy(input$normalise) && input$normalise == "scale")) {
      res <- normalise(res)
    } else if (shiny::isTruthy(input$rogue) || (shiny::isTruthy(input$normalise) && input$normalise == "rogue")) {
      res <- add_rogue_allele(res)
    }

    res
  })

  dataset
}



###### Static
available_datasets <- function() {
  ad <- data(package = "fafreqs")$results[,3]
  fts <- lapply(ad, function(t) { eval(parse(text = t)) })
  names(ad) <- lapply(fts, function(t) { t$NAME })
  ad["From Familias file"] = "familias"
  ad["Custom"] = "custom"

  ad
}

#' Launch the fafreqs GUI
#'
#' @seealso \code{\link{fafreqs_widget_input}}
#' @export
fafreqs_gui <- function() {
  shiny::runApp(system.file('fafreqs_shiny', package='fafreqs'))
}
