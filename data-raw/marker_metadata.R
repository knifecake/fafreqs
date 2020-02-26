read_marker_metadata <- function(filename) {
  df <- read.table(filename, sep = ",", header = TRUE, as.is = TRUE, row.names = 1)

  df
}

marker_metadata <- read_marker_metadata("data-raw/marker_metadata.csv")
usethis::use_data(marker_metadata, overwrite = TRUE, internal = TRUE)
