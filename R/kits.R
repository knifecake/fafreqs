analysis_kits <-
  list(
    list(
      abbrev = "core23",
      name = "Core 23 STRs",
      markers = c("CSF1PO", "D1S1656", "D2S441", "D2S1338", "D3S1358", "D5S818",
                  "D7S820", "D8S1179", "D10S1248", "D12S391", "D13S317", "D16S539",
                  "D18S51", "D19S433", "D21S11", "D22S1045", "FGA", "TH01", "TPOX",
                  "vWA", "SE33", "Penta_D", "Penta_E")
    ),

    list(
      abbrev = "core24",
      name   = "Core 23 STRs + D6S1043",
      markers = c("CSF1PO", "D1S1656", "D2S441", "D2S1338", "D3S1358", "D5S818",
                  "D7S820", "D8S1179", "D10S1248", "D12S391", "D13S317", "D16S539",
                  "D18S51", "D19S433", "D21S11", "D22S1045", "FGA", "TH01", "TPOX",
                  "vWA", "SE33", "Penta_D", "Penta_E", "D6S1043")
    ),

    list(
      abbrev = "illumina_forenseq",
      name = "Illumina ForenSeq",
      markers = c("CSF1PO", "D1S1656", "D2S1338", "D2S441", "D3S1358",
                  "D4S2408", "D5S818", "D6S1043", "D7S820", "D8S1179",
                  "D9S1122", "D10S1248", "D12S391", "D13S317", "D16S539",
                  "D17S1301", "D18S51", "D19S433", "D20S482", "D21S11",
                  "D22S1045", "FGA", "Penta_D", "Penta_E", "TH01",
                  "TPOX", "vWA")
    ),

    list(
      abbrev = "qiagen_investigator",
      name = "Qiagen Investigator HDplex",
      markers = c("D2S1360", "D3S1744", "D4S2366", "D5S2500", "D6S474",
                  "D7S1517", "D8S1132", "D10S2325", "D21S2055", "SE33",
                  "D12S391", "D18S51")
    ),

    list(
      abbrev = "promega_cs7",
      name = "Promega CS7",
      markers = c("LPL", "F13B", "FESFPS", "F13A01", "Penta_B", "Penta_C",
                  "Penta_D", "Penta_E")
    ),

    list(
      abbrev = "usc_aim",
      name = "USC AIM-STRs",
      markers = c("D1S1679", "D2S427", "D3S2406", "D3S4545", "D5S1457", "D7S2201",
                  "D9S1118", "D11S1304", "D12S297", "D14S1426", "D15S822",
                  "D21S102", "D9S1120")
    )
  )

#' Standard Amplification Kits
#'
#' This function returns information about the most comonly used amplification
#' kits. The \code{abbrev} and \code{name} params allow the user to search for
#' kits in this package. The \code{just_names} param controls the output of the
#' function. If no parameters are provided, it returns a list of all the kits
#' that this package includes.
#'
#' @param abbrev abbreviation code for the kit
#' @param name name of the kit
#' @param just_names wether to return the abbreviation of the matching kits in a
#'   named list or just the list of markers amplified by that kit.
#'
#' @return a (list of) kit which consists in a list of markers (if
#'   \code{just_names = FALSE}) or the abbreviation of the kit. If multiple kits
#'   match or no filtering parameters were provided a list of kits is returned.
#' @export
#'
#' @examples
#' # get a list of the names of all the available kits
#' get_marker_kit(just_names = TRUE)
#'
#' # get the markers included in the "Qiagen Investigator HDplex" kit
#' get_marker_kit(name = "Qiagen Investigator HDplex")
#'
#' # get the markers included in the "core23" (abbrev) kit
#' # this is useful for when you know the abbreviation
#' # of a kit which is shorter to type
#' get_marker_kit(abbrev = "core23")
get_marker_kit <- function(abbrev = NULL, name = NULL, just_names = FALSE) {
  ks <- if (!is.null(abbrev)) {
    # Find by abbreviation
    Filter(function(k) { k$abbrev == abbrev }, analysis_kits)
  } else if (!is.null(name)) {
    # Find by name
    Filter(function(k) { k$name == name }, analysis_kits)
  } else {
    # Return all kits
    analysis_kits
  }

  ret <- if (just_names) {
    # Just the names and the abbreviations (handy for the GUI)
    vs <- unlist(Map(function(k) { k$abbrev }, ks))
    ns <- unlist(Map(function(k) { k$name }, ks))
    names(vs) <- ns
    vs
  } else {
    # Just the markers
    Map(function(k) { k$markers }, ks)
  }

  if (length(ret) == 1) ret[[1]] else ret
}
