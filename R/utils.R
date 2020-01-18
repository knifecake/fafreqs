empty_freqt <- freqt(data.frame())

#' Get markers in a \code{\link{freqt}} object
#'
#' @param x a \code{\link{freqt}} object
#' @return a character vector containing the markers for which frequency data is
#'   available in \code{x}
#'
#' @examples
#' markers(ft_popstr_europe)
#'
#' @seealso \code{\link{filterMarkers}}
#' @export
markers <- function(x) {
  rownames(x$TABLE)
}

#' Filter markers in a \code{\link{freqt}} object
#'
#' @param x a \code{\link{freqt}} object
#' @param markers a list of marker names
#' @return a \code{\link{freqt}} object which only contains those markers in
#'   \code{markers}
#'
#' @examples
#' # pop.STR european frequencies for only the marker TPOX
#' filter_markers(ft_popstr_europe, c('TPOX'))
#'
#' @seealso \code{\link{markers}}
#' @export
filter_markers <- function(x, markers) {
  x$TABLE = x$TABLE[markers,]

  x
}
