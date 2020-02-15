#' Write a frequency table
#'
#' Writes a frequency table in one of the implemented formats.
#'
#' \describe{
#'
#' \item{write_csv}{Writes to a CSV file in the standard one-marker-per-row,
#' one-allele-per-column format.}
#'
#' \item{write_familias}{Writes to a familias allele frequency database file.}
#'
#' }
#'
#' @param x a \code{\link{freqt}} object
#' @param filepath the path where to write the output file
#' @param included_markers markes to include in the exported file (default to
#'   all available markers)
#'
#' @name exporting_data
#' @seealso \link{importing_data}

#' @rdname exporting_data
#' @export
write_csv <- function(x, filepath, included_markers = NULL) {
  if (is.null(included_markers)) {
    included_markers <- markers(x)
  }

  write.csv(remove_all_na_columns(x$TABLE[included_markers, ]),
            filepath, na = "0")
}

#' @rdname exporting_data
#' @export
write_familias <- function(x, filepath, included_markers = NULL) {

  if (is.null(included_markers)) {
    included_markers <- markers(x)
  }

  out <- ""

  for (m in included_markers) {
    out <- paste0(out, m, "\n")
    for (a in alleles(x, m)) {
      out <- paste0(out, a, "\t", as.character(x$TABLE[m, a]), "\n")
    }
    out <- paste0(out, "\n")
  }

  # remove the two trailing newlines
  out <- substr(out, 1, nchar(out) - 2)

  write(out, filepath)
}
