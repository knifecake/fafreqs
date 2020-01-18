new_freqt <- function(x, name = NA_character_) {
  ft <- list(TABLE = x,
             NAME  = name)

  class(ft) <- "freqt"

  ft
}

validate_freqt <- function(x) {
  TRUE
}

#' Create a `freqt` object
#'
#' @param x a `data.frame` where cell (i, j) contains the frequency of allele j
#'   of marker i. The row names of `x` should be the marker names.
#' @param name (optional, default = "") the name of the frequency database
#'
#' @return a `freqt` objet
#' @export
freqt <- function(x, name = "") {

  x <- as.data.frame(x)

  ft <- new_freqt(x, name = name)
  validate_freqt(ft)

  ft
}


#' Convert a `freqt` to a `data.frame`
#'
#' @param x a `freqt` object
#' @param ... further parameters
#' @param markers a character vector of markers to be included in the resulting
#'   dataframe. By default, all markers available in the dataset will be
#'   included.
#'
#' @return a `data.frame` with one allele per column and one marker per row. The
#'   `row.names` of the result are the names of the markers.
#' @export
as.data.frame.freqt <- function(x, ..., markers = NULL) {
  if (is.null(markers)) {
    markers = rownames(x$TABLE)
  }

  x$TABLE[markers, colSums(x$TABLE != 0, na.rm = TRUE) > 0]
}

#' Printing frequency tables
#'
#' Print a `freqt` object
#'
#' This first calls [as.data.frame.freqt()] and then prints the resulting
#' `data.frame`. The `data.frame` is returned invisibly.
#'
#' @param x a `freqt` object
#' @param ... further parameters
#'
#' @export
print.freqt <- function(x, ...) {
  df <- as.data.frame(x)

  print(df)

  invisible(df)
}
