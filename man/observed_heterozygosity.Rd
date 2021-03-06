% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/population-stats.R
\name{observed_heterozygosity}
\alias{observed_heterozygosity}
\title{Observed Heterozygosity}
\usage{
observed_heterozygosity(x, ms = NULL)
}
\arguments{
\item{x}{a \code{\link{freqt}} object}

\item{ms}{a list of markers for which the observed heterozygosities will be
returned. By default they are returned for every marker in the given
\code{freqt} object.}
}
\value{
a numeric vector containing the observed heterozygosities
}
\description{
Observed Heterozygosity
}
\examples{
observed_heterozygosity(ft_ferdous2010_bangladesh_chakma)
}
