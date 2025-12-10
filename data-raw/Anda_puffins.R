## code to prepare `Anda_puffins` dataset goes here

Anda_puffins <- readRDS(here("data-raw/Anda_puffins.rds"))

# Here we can further anonymise and manipulate the data before we make it publicly available.




usethis::use_data(Anda_puffins, overwrite = TRUE)
