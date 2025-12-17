## code to prepare `saturation_curve` dataset goes here

saturation_curve <- readRDS(here("data-raw/saturation_curve.rds"))


usethis::use_data(saturation_curve, overwrite = TRUE)
