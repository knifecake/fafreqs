new_freqt <- function(x, name = NA_character_) {
  ft <- list(TABLE = x,
             NAME  = name)

  class(ft) <- "freqt"

  ft
}

validate_freqt <- function(x) {
  TRUE
}

#' Create a \code{freqt} object
#'
#' @param x a \code{\link{data.frame}} where cell \code{(i, j)} contains the
#'   frequency of allele \code{j} of marker \code{i}. The row names of \code{x}
#'   should be the marker names.
#' @param name (optional, default = "") the name of the frequency database
#'
#' @return a \code{freqt} objet
#' @export
freqt <- function(x, name = "") {

  x <- as.data.frame(x)

  ft <- new_freqt(x, name = name)
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
#' @param ... further parameters
#'
#' @seealso \code{\link{as.data.frame.freqt}}
#'
#' @export
print.freqt <- function(x, ...) {
  df <- as.data.frame(x)

  df[df == 0] <- NA

  print(df)

  invisible(df)
}
