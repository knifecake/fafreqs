#' Write a frequency table
#'
#' \description{
#'
#' \item{write_familias}{Writes to a familias allele frequency database file.}
#'
#' }
#'
#' @param x a \code{\link{freqt}} object
#'
#' @name exporting_data
#' @export

#' @rdname exporting_data
write_familias <- function(x, filepath) {
  out = ""

  for (m in markers(x)) {
    out = paste0(out, m, "\n")
    for (a in alleles(x, m)) {
      out = paste0(out, a, "\t", as.character(x$TABLE[m, a]), "\n")
    }
    out = paste0(out, "\n")
  }

  # remove the two trailing newlines
  out = substr(out, 1, nchar(out) - 2)

  write(out, filepath)
}
