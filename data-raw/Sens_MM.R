## code to prepare `Sens_MM` dataset goes here
Sens_MM <- readRDS(here('data-raw/Sens_MM.rds'))

usethis::use_data(Sens_MM, overwrite = TRUE)
