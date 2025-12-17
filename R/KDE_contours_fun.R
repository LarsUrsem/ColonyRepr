#' Extract kernel vertices from UD-object
#'
#' @param UD An `estUD` object as produced by `KDE_UN_fun()` or `adehabitatHR::kernelUD()`
#' @param KDE_contour The KDE contour percentage you're interested in (e.g., 75)
#' @param species_colony_data Data frame with species-colony spatial data (e.g. as produced by `KDE_filter_tracks_fun()` in .$Dataset)
#' @param h_par smoothing parameter that was used in `KDE_UN_fun()` (numeric). This is just to keep track of which smoothing parameter was used, it is not used in any analysis.
#' @param projection provide the projection character string that is appropriate for the assessed track (e.g. as returned from `KDE_UD_fun()`). This projection will be used to calculate the surface area of the constructed contour polygon.
#'
#' @return An sf-object containing the kernel contour as a polygon geometry and some additional track information
#' @export
#'
#' @import dplyr
#' @import magrittr
#' @import sf
#' @import sp
#' @import adehabitatHR
KDE_contours_fun <- function(UD,
                             KDE_contour,
                             species_colony_data,
                             h_par,
                             projection){

  # We take the UD-object and determine the contour
  contour_sf <- getverticeshr(UD, KDE_contour) %>% st_as_sf() %>%
    st_transform(crs = 4326) %>%
    as_tibble() %>%
    rename(individ_Year = id) %>% # Let's rename according to our original data
    left_join(species_colony_data %>%
                distinct(species, colony, individ_id, individ_Year, W_Year, month),
              by = "individ_Year") %>% # Include track information
    st_as_sf() %>%
    st_transform(crs = projection) %>% # Set the track-appropriate projection to later calculate surface area
    mutate('KDE_contour' = KDE_contour, # indicate the used kernel contour
           'h_par' = h_par) # Include smoothing parameter information

  contour_sf$area <- as.numeric(st_area(contour_sf)) # Calculate area in units that we know (km2; see ´projection´)
  contour_sf <- contour_sf %>%
    rename(area_km2 = area) %>%
    st_transform(crs = 4326) #Set to a general crs so we can combine tracks into one sf-tibble

  return(contour_sf)
}
