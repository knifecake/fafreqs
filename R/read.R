#' Parse common allele frequency data formats
#'
#' Reads a tabular data file and generates a \code{\link{freqt}} object.
#' Supports multiple common data sources:
#'
#' \describe{
#'
#' \item{\code{read_popstr}}{supports reading from CSV files downloaded from the
#' pop.STR project website. No modification of the files is required.}
#'
#' \item{\code{read_nist}}{Supports reading from CSV files generated from NIST
#' STRBase data. NIST data comes in MS Excel files with multiple sheets and
#' additional formating. This function expexts a CSV file with only one of those
#' sheets.}
#'
#' \item{\code{read_familias}}{Supports reading from a Familias frequency
#' database file.}
#'
#' \item{\code{read_strider}}{Supports extracting frequency data from the STRidER
#' 2.0 XML database. Requires that the parameter \code{origin} is supplied and
#' that it matches one of the keys in the STRidER database.}
#'
#' }
#'
#' @param filename the path to the relevant CSV file
#' @param name     the name to give to the dataset
#' @param origin   (only for read_strider) which population group to choose
#' @return a \code{\link{freqt}} object with the given data
#'
#' @name importing_data

#' @rdname importing_data
#' @export
read_popstr <- function(filename, name = "") {
  # load data
  data <- read.csv(filename, header = T, sep = ";", dec = ".")

  ncols <- ncol(data)

  # remove `freq_` prefix from allele column names
  prefix <- colnames(data[1:3])
  old    <- colnames(data[4:(ncols - 6)])
  suffix <- colnames(data[(ncols - 5):ncols])

  new <- substring(old, 6)
  colnames(data) <- c(prefix, new, suffix)

  df <- data[4:(ncols - 6)]
  row.names(df) <- data$STR

  freqt(df, name)
}

#' @rdname importing_data
#' @export
read_nist <- function(filename, name = "") {
  data <- read.csv(filename, header = T, sep = ";", dec = ".")

  df <- t(data[3:(nrow(data) - 4), 2:ncol(data)])
  colnames(df) <- as.character(data[3:(nrow(data) - 4), 1])

  freqt(df, name)
}

#' @rdname importing_data
#' @export
read_familias <- function(filepath, name) {
  conn <- file(description = filepath, open = "r")
  lines <- readLines(conn)

  tmp_markers <- list()

  i <- 1
  j <- 1
  while (i <= length(lines)) {

    marker_name <- lines[i]
    i <- i + 1

    afreq = list()

    while (i <= length(lines)) {
      if (lines[i] == "") break

      unpacked = unlist(strsplit(lines[i], "\t", fixed = TRUE))
      al <- unpacked[1]
      freq <- as.numeric(unpacked[2])

      afreq[al] <- freq

      i <- i + 1
    }

    tmp_markers[[j]] <- list(name = marker_name,
                             afreq = afreq)

    i <- i + 1
    j <- j + 1
  }

  close(conn)

  marker_names <- unlist(lapply(tmp_markers,
                                function(m) { m$name }))

  all_alleles <- unique(unlist(lapply(tmp_markers,
                                      function(m) { names(m$afreq) })))
  print(all_alleles)

  wide_freq <- lapply(tmp_markers, function(m) {
    lapply(all_alleles, function(a) {
      f = list()
      if (a %in% names(m$afreq)) {
        f[a] <- m$afreq[a]
      } else {
        f[a] <- NA
      }
      f
    })
  })

  names(wide_freq) <- marker_names

  freqt(as.data.frame(t(as.data.frame(lapply(wide_freq, unlist)))), name = name)
}

library(xml2)
read_strider <- function(filename, name = "", origin = "") {
  xml <- read_xml(filename)

  markers <- unlist(as_list(xml_find_all(xml, "/frequencies/marker/alleles[node()]/../name")))

  q <- sprintf("/frequencies/marker/alleles[node()]", origin)
  all_alleles <- unlist(as_list(xml_find_all(xml, q)))
  all_alleles <- unique(unlist(lapply(all_alleles, function(as) { strsplit(as, ", ") })))

  wide_freq <- lapply(markers, function(m) {
    lapply(all_alleles, function (a) {
      f <- list()
      q <- sprintf("/frequencies/marker[name = \"%s\"]/origin[@name = \"%s\"]/frequency[@allele = \"%s\"]",
                   m, origin, a)
      result <- xml_find_all(xml, q)
      if (length(result) > 0) {
        f[a] <- as.numeric(unlist(as_list(result)[1]))
      } else {
        f[a] <- 0
      }
      f
    })
  })

  names(wide_freq) <- markers

  freqt(as.data.frame(t(as.data.frame(lapply(wide_freq, unlist)))), name = name)
}
