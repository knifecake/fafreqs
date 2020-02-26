ft_cho2003_south_korea <-
  read_allelic_ladder("data-raw/cho-2003-south-korea.csv",
                      name = "Cho 2003 - South Korea",
                      h_obs = 0,
                      source = "Cho, N. S., Hwang, J. H., Lee, Y. A., & Park, I. H. (2003). Population genetics of nine STR loci: TH01, TPOX, CSF1PO, vWA, FESFPS, F13A01, D13S317, D7S820 and D16S539 in a Korean population. Forensic science international, 137(1), 97-99.")
usethis::use_data(ft_cho2003_south_korea, overwrite = TRUE)

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
