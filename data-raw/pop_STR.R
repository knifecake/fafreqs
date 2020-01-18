source("R/read.R")

ft_popstr_europe <- read_popstr("data-raw/popSTR-europe.csv",
                                "pop.STR - Europe (all regions)")
usethis::use_data(ft_popstr_europe, overwrite = TRUE)

ft_popstr_nw_spain <- read_popstr("data-raw/popSTR-NW-spain.csv",
                                  "pop.STR - North-West Spain")
usethis::use_data(ft_popstr_nw_spain, overwrite = TRUE)

ft_popstr_israel_carmel_druze <- read_popstr("data-raw/popSTR-israel-carmel-druze.csv",
                                             "pop.STR - Israel (Carmel) - Druze")
usethis::use_data(ft_popstr_israel_carmel_druze, overwrite = TRUE)
