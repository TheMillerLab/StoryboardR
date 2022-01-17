## code to prepare `storyboard` simulated dataset goes here
ctcae_v5 <- readxl::read_excel("data-raw/CTCAE_v5.0.xlsx")
usethis::use_data(ctcae_v5, overwrite = TRUE)
