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

ft_popstr_cambodia_cambodian <- read_popstr("data-raw/popSTR-cambodia-cambodian.csv",
                                            "pop.STR - Cambodia - Cambodian")
usethis::use_data(ft_popstr_cambodia_cambodian, overwrite = TRUE)

ft_popstr_china_dai <- read_popstr("data-raw/popSTR-china-dai.csv",
                                   "pop.STR - China - Dai")
usethis::use_data(ft_popstr_china_dai, overwrite = TRUE)

ft_popstr_china_daur <- read_popstr("data-raw/popSTR-china-daur.csv",
                                    "pop.STR - China - Daur")
usethis::use_data(ft_popstr_china_daur, overwrite = TRUE)

ft_popstr_china_han <- read_popstr("data-raw/popSTR-china-han.csv",
                                   "pop.STR - China - Han")
usethis::use_data(ft_popstr_china_han, overwrite = TRUE)

ft_popstr_japan_japanese <- read_popstr("data-raw/popSTR-japan-japanese.csv",
                                        "pop.STR - Japan - Japanese")
usethis::use_data(ft_popstr_japan_japanese, overwrite = TRUE)

ft_popstr_siberia_yakut <- read_popstr("data-raw/popSTR-siberia-yakut.csv",
                                       "pop.STR - Siberia - Yakut")
usethis::use_data(ft_popstr_siberia_yakut, overwrite = TRUE)
