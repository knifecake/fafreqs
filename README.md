
# fafreqs

The `fafreqs` R package is a compilation of widely available allele
frequency databases for markers commonly used in forensic applications
(mainly STRs). It provides compatibility functions that enable
interoperability with other DNA-analysis related R packages such as
those part of the pedsuite.

In addition, it includes a shiny UI that makes it easy to export
datasets in various formats. The shiny UI also serves as a demonstration
of the shiny module that the package exposes, that may be utilised as an
input component in other shiny applications to facilitate frequency data
import.

## Installation

`fafreqs` is only available on GitHub so the `devtools` package is
required to install it. If you do not have it installed (or you are not
sure if you have it) install it from CRAN with the following command:

``` r
install.packages("devtools")
```

Once you have `devtools`, the `fafreqs` package can be installed with

``` r
devtools::install_github("knifecake/fafreqs")
```

## Example usage

Once installed, datasets become available after loading the package

``` r
library("fafreqs")

# datasets become available as objects
ft_popstr_europe
```

### Exporting allele frequency tables

All datasets available through the `fafreqs` package may be exported to
various formats. For now, exporting is possible to the standard
table-like format and to the format used by
[Familias](https://familias.no).

``` r
library(fafreqs)

# save the NIST African American dataset as a standard CSV file
write_csv(ft_nist_african_american, "nist_african_american.csv")

# save the NIST African American dataset as a Familias-compatible file
write_familias(ft_nist_african_american, "nist_african_american.txt")
```

### Usage with the pedsuite package family

`fafreqs` can be used to set so-called `locusAttributes` in `ped`
objects handled by the pedsuite set of R packages (see
[`pedtools`](https://github.com/magnusdv/pedtools) and
[`forrel`](https://github.com/magnusdv/forrel) for more information).

Once a pedigree is loaded or created but before adding known genotypes,
information about markers and allele frequencies can be loaded using the
`pedtools::setMarkers()` function. This function takes a special list as
its `locusAttributes` parameter that can be generated directly from a
`freqt` object by calling `to_pedtools`.

``` r
library(pedtools)
library(fafreqs)
```

    ## 
    ## Attaching package: 'fafreqs'

    ## The following object is masked from 'package:pedtools':
    ## 
    ##     alleles

``` r
p = nuclearPed(1)
p = setMarkers(p, locusAttributes = to_pedtools(ft_nist_african_american))

# allele frequencies for the markers described in ft_nist_african_american are
# attached to the pedigree
afreq(p, "CSF1PO")
```

    ##    7.0    8.0    9.0   10.0   11.0   12.0   13.0   14.0 
    ## 0.0556 0.0556 0.0395 0.2500 0.2485 0.2953 0.0468 0.0088

## Included datasets

The following datasets are included with the `fafreqs` package

  - **STRidER:** data from all available countries (Austria, Belgium,
    Bosnia and Herzegowina, Czech Republic, Denmark, Finland, France,
    Germany, Greece, Hungary, Ireland, Montenegro, Norway, Poland,
    Slovakia, Slovenia, Spain, Sweeden and Switzerland). More
    information can be found on the `data_strider` help page.

  - **pop.STR:** data from some population (meta-)groups is available
    (Europe, NW Spain, Israel Carmel Druze). More information can be
    found on the `data_popstr` help page.

  - **NIST 1036:** data from all ethnic groups is available (All,
    African American, Asian, Caucasic and Hispanic). More information
    can be found on the `data_nist` help page.

To access the help page for a particular dataset use `?data_xxxx` from
the R console. Alternatively, you may look up help for a particular
population group and data source by typing `?ft_dataset_name`
(e.g. `?ft_popstr_europe`).
