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

#' Get alleles of marker
#'
#' Returns all alleles found in the given \code{\link{freqt}} object for which
#' the frequency is greater than 0.
#'
#' @param x a \code{freqt} object
#' @param marker a marker name or index
#' @return a list of allele names
#'
#' @export
alleles.freqt <- function(x, marker) {
  colnames(x$TABLE[marker, !is.na(x$TABLE[marker, ])])
}

#' Convert a \code{\link{freqt}} object to the pedtools \code{locusAttributes}
#' format.
#'
#' This function is useful for loading the allele frequency databases found in
#' this package into pedtools or other pedsuite packages which use the pedtools
#' package such as forrel.
#'
#' @param x a \code{freqt} object
#' @return a \code{pedtools}-compatible list of lists following the
#'   \code{locusAttributes} definition
#'
#' @examples
#' p = nuclearPed
#' p = setMarkers(p, locusAttributes = to_pedtools(ft_nist_african_american))
#'
#' @seealso \code{\link{marker_attach}}
#'
#' @export
to_pedtools <- function(x) {
  ms = markers(x)

  lapply(ms, function(m) {
    list(
      alleles = alleles(x, m),
      afreq = as.numeric(x$TABLE[m, alleles(x, m)]),
      name = m
    )
  })
}
