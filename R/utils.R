empty_freqt <- freqt(data.frame())

#' Remove columns where every row is NA
#'
#' @param x a dataframe
#' @return `x` but with columns where every row was NA removed
#'
#' @source \url{https://stackoverflow.com/a/2644009/1646268}
remove_all_na_columns <- function(x) {
  x[, colSums(is.na(x)) < nrow(x)]
}

#' Get markers in a \code{\link{freqt}} object
#'
#' @param x a \code{\link{freqt}} object
#' @return a character vector containing the markers for which frequency data is
#'   available in \code{x}
#'
#' @examples
#' markers(ft_popstr_europe)
#'
#' @seealso \code{\link{filter_markers}}
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
  x$TABLE <- x$TABLE[markers, ]
  x$SAMPLE_SIZES <- x$SAMPLE_SIZES[markers]
  x$H_OBS <- x$H_OBS[markers]
  x$CHROMS <- x$CHROMS[markers]

  x
}

#' Standarise marker names
#'
#' @param x a \code{\link{freqt}} object
#'
#' @return an updated version of \code{x} where the marker names have been standarised
#' @export
#'
#' @seealso \code{\link{standard_marker_names}}
#'
#' @examples
#' # vWA is changed to VWA
#' markers(standarise_names(ft_nist_african_american))
standarise_names <- function(x) {
  std <- standard_marker_names(markers(x))
  rownames(x$TABLE) <- std

  if (!is.null(x$SAMPLE_SIZES))
    names(x$SAMPLE_SIZES) <- std

  if (!is.null(x$CHROMS))
    names(x$CHROMS) <- std

  if (!is.null(x$H_OBS))
    names(x$H_OBS) <- std

  x
}

#' Standarise marker names
#'
#' @param ms a marker name (or a character vector of marker names)
#'
#' @return an updated version of \code{ms} where the names have been standarised
#' @export
#'
#' @seealso \code{\link{standarise_names}}
#'
#' @examples
#' standard_marker_names("vWA") == "VWA"
#'
#' standard_marker_names(c("Penta D", "vWA")) == c("Penta_D", "VWA")
standard_marker_names <- function(ms) {
  gsub(" ", "_", paste0(toupper(substring(ms, 1, 1)), substring(ms, 2)))
}

#' Guess chromosome numbers
#'
#' @param x a \code{\link{freqt}} object
#' @param as_numeric whether to convert chromosome names "X" and "Y" to the
#'   number 23
#'
#' @return an updated version of \code{x} with the known chormosome numbers attached
#' @export
#'
#' @examples
#' guess_chromosome_numbers(ft_popstr_cambodia_cambodian)
guess_chromosome_numbers <- function(x, as_numeric = FALSE) {
  ns <- marker_metadata[markers(x), "Chrom"]

  if (as_numeric) {
    ns[ns == "X"] <- "23"
    ns[ns == "Y"] <- "23"
  }

  names(ns) <- markers(x)

  x$CHROMS <- ns

  x
}

#' Get alleles of marker
#'
#' Returns all alleles found in the given \code{\link{freqt}} object for which
#' the frequency is greater than 0.
#'
#' @param x a \code{freqt} object
#' @param ms a marker name or a list of marker names
#' @return a list of allele names
#'
#' @export
#'
#' @examples
#' # provided with only one marker, returns the possible alleles for that marker
#' alleles(ft_popstr_europe, "CSF1PO")
#'
#' # provided with more than one marker, returns the union of the
#' # possible alleles for each of the provided markers
#' alleles(ft_popstr_europe, c("CSF1PO", "FGA"))
alleles <- function(x, ms) {
  colnames(x$TABLE[ms, apply(!is.na(x$TABLE[ms, ]), 2, any)])
}

#' Get allele frequencies
#'
#' @param x a \code{\link{freqt}} object
#' @param ms a marker or marker vector
#' @param als an allele or allele vector. If none is provided all relevant
#'   alleles will be returned for the selected markers.
#'
#' @return depending on the number of alleles provided a single number or a data
#'   frame is returned with the request allele frequencies
#' @export
#'
#' @examples
#' # a single marker and allele returns a number
#' frequencies(ft_popstr_europe, "FGA", 23)
#'
#' # multiple alleles result in a data.frame
#' frequencies(ft_popstr_europe, "FGA", c(6, 23, 7, 23.2))
#'
#' # multiple markers and alleles also result in a data.frame
#' frequencies(ft_popstr_europe, c("CSF1PO", "FGA"), c(6, 23, 7, 23.2))
#'
#' # if alleles are not specified then every non-zero frequency is returned
#' frequencies(ft_popstr_europe, "FGA")
#'
#' # similarly, if multiple markers are provided any allele
#' # with a non-zero frequency in any of the markers is returned
#' frequencies(ft_popstr_europe, c("CSF1PO", "FGA"))
frequencies <- function(x, ms, als = NULL) {
  if (is.null(als)) {
    als <- alleles(x, ms)
  }
  x$TABLE[ms, as.character(als)]
}

#' Normalise a frequency database
#'
#' Scales a frequency database so that allele frequencies sum up to one.
#'
#' @param x a \code{\link{freqt}} object
#' @param ms a marker or a marker vector. If not provided, all markers are
#'   scaled.
#'
#' @return a scaled version of the given frequency database
#' @export
#'
#' @examples
#' n <- normalise(ft_popstr_europe)
#'
#' # this is useful to check, but, in general, you should not
#' # depend on the internal structure of a freqt object
#' rowSums(n$TABLE, na.rm = TRUE)
normalise <- function(x, ms = NULL) {
  if (is.null(ms)) {
    ms <- markers(x)
  }

  x$TABLE[ms, ] <- t(apply(x$TABLE[ms, ], 1, function(m) { m / sum(m, na.rm = TRUE) }))

  x
}

#' Add a rogue allele column
#'
#' Adds an extra allele to every marker with frequency equal to 1 minus the sum
#' of the frequencies of the alleles in that marker. Note: if the frequencies
#' sum up to greater than one, then the rogue allele column will have negative
#' frequencies.
#'
#' @param x a \code{\link{freqt}} object
#' @param ms a marker or marker vector. If left undefined, then all markers are
#'   processed.
#' @param name the name to give to the rogue allele
#'
#' @export
#'
#' @examples
#' add_rogue_allele(ft_popstr_europe)
add_rogue_allele <- function(x, ms = NULL, name = "rare") {
  if (is.null(ms)) {
    ms <- markers(x)
  }

  x$TABLE[ms, name] <- 1 - rowSums(x$TABLE[ms, ], na.rm = TRUE)

  x
}

#' Convert a \code{\link{freqt}} object to a list of marker objects
#'
#' This function converts a frequency table object into a list of pedtools \code{\link[pedtools]{marker}} objects.
#'
#' @param x a \code{freqt} object
#' @param ped a \code{\link[pedtools]{ped}} object
#' @param scale wether to scale allele frequencies so that they sum up to one (which is required by pedtools)
#' @return a list of \code{\link[pedtools]{marker}} objects
#'
#' @examples
#' library(pedtools)
#' p <- nuclearPed(1)
#' p <- setMarkers(p, m = to_pedtools_markers(ft_nist_african_american, p))
#'
#' @seealso \code{\link{to_pedtools_locusAttributes}}
#' @seealso \code{\link[pedtools]{marker_attach}}
#'
#' @export
to_pedtools_markers <- function(x, ped, scale = TRUE) {
  ms <- markers(x)

  if (scale)
    x <- normalise(x)

  lapply(ms, function(m) {
    pedtools::marker(ped,
      alleles = alleles(x, m),
      afreq = as.numeric(x$TABLE[m, alleles(x, m)]),
      name = m)
  })
}

#' Convert a \code{\link{freqt}} object to the pedtools \code{locusAttributes}
#' format.
#'
#' This function is useful for loading the allele frequency databases found in
#' this package into pedtools or other pedsuite packages which use the pedtools
#' package such as forrel.
#'
#' @param x a \code{freqt} object
#' @param scale wether to scale allele frequencies so that they sum up to one
#'   (which is required by pedtools)
#' @return a \code{pedtools}-compatible list of lists following the
#'   \code{locusAttributes} definition
#'
#' @examples
#' library(pedtools)
#' p <- nuclearPed(1)
#' p <- setMarkers(p, locusAttributes = to_pedtools_locusAttributes(ft_nist_african_american))
#'
#' @seealso \code{\link{to_pedtools_markers}}
#' @seealso \code{\link[pedtools]{marker_attach}}
#'
#' @export
to_pedtools_locusAttributes <- function(x, scale = TRUE) {
  ms <- markers(x)

  if (scale)
    x <- normalise(x)

  lapply(ms, function(m) {
    list(alleles = alleles(x, m),
         afreq = as.numeric(x$TABLE[m, alleles(x, m)]),
         name = m)
  })
}

#' Convert rownames to the first column
#'
#' @param x a \code{\link[base]{data.frame}}
#' @param name the column name for the row names
#'
#' @export
rownames_to_column <- function(x, name = "X") {
  df <- x
  rownames(df) <- NULL
  df <- cbind(rownames(x), df)
  colnames(df) <- c(name, colnames(x))

  df
}
