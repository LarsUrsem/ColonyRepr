## code to prepare `DATASET` dataset goes here

mean_smooths <- readRDS(here("data-raw/mean_smooths.rds"))

usethis::use_data(mean_smooths, overwrite = TRUE)


