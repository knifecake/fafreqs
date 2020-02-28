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

ft_zeyad2020_uae_ystrs <-
  read_allelic_ladder("data-raw/zeyad-2020-uae-ystrs.csv",
                      name = "Zeyad 2020 - United Arab Emirates - YSTRs",
                      source = "Zeyad, T., Adam, A., Alghafri, R., & Iratni, R. (2020). Study of 27 Y-STR markers in United Arab Emirates population. Forensic Science International: Reports, 100057.")
usethis::use_data(ft_zeyad2020_uae_ystrs, overwrite = TRUE)
