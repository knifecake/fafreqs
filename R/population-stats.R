#' Calculate the expected heterozygosity
#'
#' This function calculates the expected heterozygosity of each of the markers
#' in the given \code{freqt} object according to the formula \deqn{H_e = 1 -
#' \sum_{i = 0}^n p_i^2,} where \eqn{n} is the number of alleles for the marker
#' in question and \eqn{p_i} is the probability of allele \eqn{i}.
#'
#' @param x a \code{\link{freqt}} object
#' @param ms a list of markers for which the expected heterozygosities will be
#'   calculated. By default they are calculated for every marker in the given
#'   \code{freqt} object.
#'
#' @return a numeric vector containing the expected heterozygosities
#' @export
#'
#' @source https://www.uwyo.edu/dbmcd/molmark/lect04/lect4.html
#'
#' @examples
#' # calculate the expected heterozygosity for just one marker
#' expected_heterozygosity(ft_popstr_algeria_mzab_mozabite, "CSF1PO")
#'
#' # calculate the expected heterozygosities for every marker
#' expected_heterozygosity(ft_popstr_algeria_mzab_mozabite)
expected_heterozygosity <- function(x, ms = NULL) {
  if (is.null(ms)) {
    ms = markers(x)
  }

  1 - rowSums(frequencies(x, ms)^2, na.rm = TRUE)
}
