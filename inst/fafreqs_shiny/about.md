### Running this application on your own computer

You may run this application locally on your own computer by installing the `fafreqs` package and some additional dependencies. Paste the following code into an R interactive session
```R
install.packages(c("shiny", "markdown", "devtools"))
devtools::install_github("knifecake/gezellig")
devtools::install_github("knifecake/fafreqs")
```

Once installed you may run the application by issuing the commands

```R
library(fafreqs)
fafreqs_gui()
```
An internet browser should be launched automatically after the last command.

### About fafreqs

`fafreqs` is an compilation of allele frequency databases with a focus on DNA markers commonly used in forsensics. The frequency databases available on this package come from multiple sources and only minimal processing has been carried out to include them in this package. In general, research on allele frequencies generally focuses on a particular population group and this is reflected in this package.

The `fafreqs` package can be used in combination with other forensic tools in R such as [pedtools](https://github.com/magnusdv/pedtools), [forrel](https://github.com/magnusdv/forrel) and other packages from the pedsuite. In addition, it includes mechanisms to export the data in the standard CSV format with one allele per column and one marker per row as well as in the format used by the [Familias](https://familias.no) software.

To encourage adoption and make usage easier for users not familiar with the R ecosistem, the `fafreqs` package includes a [shiny](https://shiny.rstudio.com) module that provides a graphical user interface for the most common functionality. The `fafreqs_widget` shiny module can be included in other shiny applications which need frequency data. See [instructions on how to incorporate this widget](https://github.com/knifecake/fafreqs) (or part of it) to your own shiny application.

This application serves both as a demonstration of the module as well as a graphical tool for downlading the frequency databases available within the package. The main advandage of using this tool as opposed to getting the data from the sources is that this tool is able to export the data in a number of common formats as well as carry out some common preprocessing tasks such as normalising allele frequencies or selecting the markers to be included on the frequency table.

### Issues, contributing and authors

Users are kindly asked to report errors to the authors. There are two ways of doing this:

- The preferred one is by filling out the form at https://github.com/knifecake/fafreqs/issues/new, although it requires a GitHub account.
- Users unable to use the first method can send an email to [eliashernandis@gmail.com](mailto:eliashernandis@gmail.com).

`fafreqs` is free software and it welcomes contributions, either in the form of datasets or improvements to the code.

`fafreqs` was developed by [Elias Hernandis](https://hernandis.me/).
