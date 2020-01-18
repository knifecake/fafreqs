source("R/read.R")

ft_nist_caucasian <- read_nist("data-raw/nist-caucasian.csv",
                               "NIST - Caucasian")
usethis::use_data(ft_nist_caucasian, overwrite = TRUE)

ft_nist_african_american <- read_nist("data-raw/nist-african-american.csv",
                                      "NIST - African American")
usethis::use_data(ft_nist_african_american, overwrite = TRUE)

ft_nist_asian <- read_nist("data-raw/nist-asian.csv",
                           "NIST - Asian")
usethis::use_data(ft_nist_asian, overwrite = TRUE)

ft_nist_hispanic <- read_nist("data-raw/nist-hispanic.csv",
                              "NIST - Hispanic")
usethis::use_data(ft_nist_hispanic, overwrite = TRUE)

ft_nist_all <- read_nist("data-raw/nist-all.csv",
                         "NIST - All")
usethis::use_data(ft_nist_all, overwrite = TRUE)
