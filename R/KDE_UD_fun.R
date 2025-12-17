#' Determine Utilization Density
#'
#' @param migration_track individual-level migration track, with a longitude and latitude column
#' @param h_par smoothing parameter (numeric)
#'
#' @return Returns a list with two elements>
#' 1) An object of class `estUD` as produced by `adehabitatHR::kernelUD()`.
#' 2) The track-specific projection (character string)
#'
#' @export
#'
#' @import dplyr
#' @import magrittr
#' @import sf
#' @import sp
#' @import adehabitatHR
KDE_UD_fun <- function(migration_track,
                       h_par){

  # We want to drop any missing location information and also transform the tibble into an sf-object
  Ind_dat_sf <- migration_track %>%
    drop_na(longitude, latitude) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  # Below we determine an appropriate projection for the assessed track.
  # We use an Azimuthal Equidistant Projection, where the centre of the projection is based on the mean coordinates of the tracks
  # units are set km
  proj.aezd    <- paste0("+proj=aeqd  +lat_0=",
                         mean(range(st_coordinates(Ind_dat_sf)[,2])),
                         "  +lon_0=",
                         mean(range(st_coordinates(Ind_dat_sf)[,1])),
                         " +units=km")
  # Let's transform our tracking data according to the new projection.
  sp_dat_sub <- st_transform(Ind_dat_sf, proj.aezd)

  # We need to determine a grid to be assessed in determining the utilization distribution
  # We do this with the lines below, based on the assessed track
  bbox <- st_bbox(sp_dat_sub)
  buff <- 500

  test.grid  <- sp::GridTopology(cellcentre.offset=c(floor(bbox[1]) - buff,
                                                     floor(bbox[2]) - buff),
                                 cellsize=c(25,25),
                                 cells.dim=c(ceiling((bbox[3]-bbox[1]+buff*2)/25),
                                             ceiling((bbox[4]-bbox[2]+buff*2)/25)))
  test.point <- SpatialPoints(cbind(c(0),c(0)))
  test.pixel <- SpatialPixels(test.point, proj4string = CRS(proj.aezd), grid = test.grid)

  # Below we take our projected track, transform it to a Spatial-object and determine it's utilization density
  X <- sp_dat_sub %>%
    dplyr::select(individ_Year) %>%
    sf::as_Spatial() %>%
    # class() %>%
    adehabitatHR::kernelUD(h = h_par, test.pixel)

  results_list <- list("UD" = X,
                       "projection" = proj.aezd)

  return(results_list)

}



