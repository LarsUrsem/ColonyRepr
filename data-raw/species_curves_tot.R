## code to prepare `species_curves_tot` dataset goes here

species_curves_tot_PN <- readRDS(here("data-raw/species_curves_tot_PN.rds"))

usethis::use_data(species_curves_tot_PN, overwrite = TRUE)
