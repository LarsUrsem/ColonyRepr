## code to prepare `example_tracks` dataset goes here

# Load predefined and manipulated example tracking data
example_tracks <- readRDS(here('data-raw/example_tracks.rds'))


usethis::use_data(example_tracks, overwrite = TRUE)
