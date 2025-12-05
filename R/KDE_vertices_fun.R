#' Extract kernel vertices from UD-object
#'
#' @param UD A UD object from adehabitatHR::kernelUD()
#' @param percent The percent KDE contour (e.g., 75)
#' @param species_colony_data Data frame with species/colony spatial data
#' @param mean_smooths_per_species Data frame with mean smoothing parameters per species
#'
#' @return An sf polygon object containing the kernel contour and metadata
#' @export
#'
#' @import dplyr
#' @import magrittr
#' @import sf
#' @import sp
#' @import adehabitatHR
KDE_vertices_fun <- function(UD,
                             percent,
                             species_colony_data,
                             mean_smooths_per_species,
                             projection){

  Vertice_sf <- getverticeshr(UD, percent) %>% st_as_sf() %>%
    st_transform(crs = 4326) %>%
    as_tibble() %>%
    rename(individ_Year = id) %>%
    left_join(species_colony_data %>%
                distinct(species, colony, individ_id, individ_Year, W_Year, Month),
              by = "individ_Year") %>%
    st_as_sf() %>%
    st_transform(crs = projection) %>%
    mutate(perc = percent,
           h_par = mean_smooths_per_species %>%
             dplyr::filter(species == unique(species_colony_data$species)) %>%
             pull(mean_h) %>%
             round(1))
  Vertice_sf$area <- as.numeric(st_area(Vertice_sf)) # Let's calculate area in units that we know (km2)
  Vertice_sf <- Vertice_sf %>%
    rename(area_km2 = area) %>%
    st_transform(crs = 4326) #Set to a general crs so we can combine tracks into one sf-tibble

  return(Vertice_sf)
}
