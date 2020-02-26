new_freqt <- function(x, name = NA_character_, sample_sizes = NULL, chroms = NULL, h_obs = NULL, data_source = NULL) {
  ft <- list(TABLE = x,
             NAME  = name,
             SAMPLE_SIZES = sample_sizes,
             CHROMS = chroms,
             H_OBS = h_obs,
             DATA_SOURCE = data_source)

  class(ft) <- "freqt"

  ft
}

validate_freqt <- function(x) {
  TRUE # all(names(x$SAMPLE_SIZES) %in% markers(x))
}

#' Create a \code{freqt} object
#'
#' @param x a \code{\link{data.frame}} where cell \code{(i, j)} contains the
#'   frequency of allele \code{j} of marker \code{i}. The row names of \code{x}
#'   should be the marker names.
#' @param name  the name of the frequency database
#' @param n a named list describing the sample size used to compute the allele
#'   frequencies for each marker
#' @param chroms a named list describing the chromosome name (or number) for each marker
#' @param h_obs a named list describing the observed heterozygosity for each
#'   marker
#' @param data_source a description of the data source
#'
#' @return a \code{freqt} objet
#' @export
freqt <- function(x, name = "", n = NULL, chroms = NULL, h_obs = NULL, data_source = NULL) {

  x <- as.data.frame(x)

  # convert 0 to NA
  x[x == 0] <- NA

  ft <- new_freqt(x,
                  name = name,
                  sample_sizes = n,
                  chroms = chroms,
                  h_obs = h_obs,
                  data_source = data_source)
  validate_freqt(ft)

  ft
}


#' Convert a \code{\link{freqt}} to a \code{\link{data.frame}}
#'
#' @param x a \code{\link{freqt}} object
#' @param ... further parameters
#' @param markers a character vector of markers to be included in the resulting
#'   dataframe. By default, all markers available in the dataset will be
#'   included.
#'
#' @return a \code{\link{data.frame}} with one allele per column and one marker
#'   per row. The \code{row.names} of the result are the names of the markers.
#' @export
as.data.frame.freqt <- function(x, ..., markers = NULL) {
  if (is.null(markers)) {
    markers <- rownames(x$TABLE)
  }

  x$TABLE[markers, colSums(x$TABLE != 0, na.rm = TRUE) > 0]
}

#' Printing frequency tables
#'
#' Print a \code{\link{freqt}} object
#'
#' This first calls \code{\link{as.data.frame.freqt}} and then prints the
#' resulting \code{\link{data.frame}}. The \code{data.frame} is returned
#' invisibly.
#'
#' @param x a \code{\link{freqt}} object
#' @param verbose wether to print the actual frequency table
#' @param ... further parameters
#'
#' @seealso \code{\link{as.data.frame.freqt}}
#'
#' @export
print.freqt <- function(x, verbose = FALSE, ...) {
  df <- as.data.frame(x)

  if (is.na(x$NAME))
    cat("Unnamed frequency table\n")
  else
    cat("Frequency table:", x$NAME, "\n")

  if (!is.null(x$SAMPLE_SIZES)) {
    ns <- as.numeric(x$SAMPLE_SIZES)
    min_n <- min(ns, na.rm = TRUE)
    max_n <- max(ns, na.rm = TRUE)

    if (min_n != max_n)
      cat(sprintf("Sample size: ranging from %d to %d individuals\n", min_n, max_n))
    else
      cat("Sample size: ", min_n, "individuals.\n")

    if (any(is.na(ns)))
      cat("(Some sample sizes unknown)\n")
  } else {
    cat("No sample size data available.\n")
  }

  if (!is.null(x$DATA_SOURCE)) {
    cat("Data source: ", x$DATA_SOURCE, "\n")
  }

  if (verbose)
    print(df)
  else {
    n_markers = length(markers(x))

    if (n_markers == 1)
      cat("\nData for 1 marker.\n")
    else
      cat(sprintf("\nData for %d markers.\n", length(markers(x))))
  }

  invisible(df)
}
