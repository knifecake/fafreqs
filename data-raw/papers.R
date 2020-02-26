ft_ferdous2010_bangladesh_tripura <-
  read_allelic_ladder("data-raw/ferdous-2010-bangladesh-tripura.csv",
                      name = "Ferdous 2010 - Bangladesh - Tripura",
                      h_obs = 0,
                      source = "Ferdous, A., Ali, M. E., Alam, S., Hasan, M., Hossain, T., & Akhteruzzaman, S. (2010). Allele Frequencies of 10 Autosomal STR Loci from Chakma and Tripura Tribal Populations in Bangladesh. Molecular biology international, 2010.")
usethis::use_data(ft_ferdous2010_bangladesh_tripura, overwrite = TRUE)

ft_ferdous2010_bangladesh_chakma <-
  read_allelic_ladder("data-raw/ferdous-2010-bangladesh-chakma.csv",
                      name = "Ferdous 2010 - Bangladesh - Chakma",
                      h_obs = 0,
                      source = "Ferdous, A., Ali, M. E., Alam, S., Hasan, M., Hossain, T., & Akhteruzzaman, S. (2010). Allele Frequencies of 10 Autosomal STR Loci from Chakma and Tripura Tribal Populations in Bangladesh. Molecular biology international, 2010.")
usethis::use_data(ft_ferdous2010_bangladesh_chakma, overwrite = TRUE)
