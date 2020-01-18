#' Parse common allele frequency data formats
#'
#' Reads a tabular data file and generates a `freqt` object. Supports multiple
#' common data sources:
#'
#' \describe{
#'
#' \item{read_popstr}{supports reading from CSV files downloaded from the
#' pop.STR project website. No modification of the files is required.}
#'
#' \item{read_nist}{Supports reading from CSV files generated from NIST
#' STRBase data. NIST data comes in MS Excel files with multiple sheets and
#' additional formating. This function expexts a CSV file with only one of those
#' sheets.}
#'
#' }
#'
#' @param filename the path to the relevant CSV file
#' @param name     the name to give to the dataset
#' @return a `freqt` object with the given data
#'
#' @name importing_data

#' @rdname importing_data
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
read_nist <- function(filename, name = "") {
  data <- read.csv(filename, header = T, sep = ';', dec = '.')

  df <- t(data[3:(nrow(data) - 4), 2:ncol(data)])
  colnames(df) = as.character(data[3:(nrow(data) - 4) ,1])

  freqt(df, name)
}
