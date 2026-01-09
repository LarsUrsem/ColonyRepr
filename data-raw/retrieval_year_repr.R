## code to prepare `retrieval_year_repr` dataset goes here

# Load predefined and manipulated example tracking data
retrieval_year_repr <- readRDS(here('data-raw/retrieval_year_repr.rds')) %>%
  dplyr::filter(species == 'Black-legged kittiwake',
         colony == 'Sklinna')


usethis::use_data(retrieval_year_repr, overwrite = TRUE)
