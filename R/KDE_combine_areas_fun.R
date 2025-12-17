#' Function to iteratively combine individual-level kernels to estimate within-colony variation
#'
#' @param contours_sf An sf-object with individual-level kernels already filtered for a desired KDE contour percentage
#' @param tot_loc_data A dataframe with species-colony location information
#' @param n_iterations Set how many iterations you'd like to use
#'
#' @return A list containing:
#'   \describe{
#'     \item{Kernel_areas}{A tibble with KDE bootstrap results}
#'     \item{colony_projection}{The LAEA projection used}
#'   }
#' @export
#'
#' @import dplyr
#' @import sf
KDE_combine_areas_fun <- function(contours_sf,
                                  tot_loc_data, #Needed to determine and use appropriate projection for area calculations
                                  n_iterations = 20
){
  # Extract basic information
  species <- unique(contours_sf$species)
  colony <- unique(contours_sf$colony)
  month <- unique(contours_sf$month)

  # Create character vector for all tracks
  individual_migrations <- contours_sf %>%
    as_tibble() %>%
    distinct(individ_Year) %>%
    pull()

  # How many assessable individuals are in this species-colonies
  n_individuals <- contours_sf %>%
    as_tibble() %>%
    distinct(individ_id) %>%
    nrow()

  # Filter for the assessed tracks. To be used in calculating an appropriate projection (next step)
  colony_dat <- tot_loc_data %>%
    dplyr::filter(individ_Year %in% individual_migrations,
           month == month)


  # Determine appropriate projection for the species-colony:
  colony_proj.laea    <- paste0("+proj=laea +x_0=0 +y_0=0 +lon_0=",
                                mean(colony_dat$longitude),
                                " +lat_0=",
                                mean(colony_dat$latitude),
                                " +units=km"
  )

  # Which kernel contour was used?
  percentage <- contours_sf %>%
    as_tibble() %>%
    distinct(KDE_contour) %>%
    pull()

  # Which smoothing parameter was used?
  h_par <- contours_sf %>%
    as_tibble() %>%
    distinct(h_par) %>%
    pull()


  # Prepare for surface for-loop
  Kernel_areas <- tibble()
  for(i in 1:n_iterations){
    # i = 1

    # For reproducability
    set.seed(i)
    # Determine the order in which we'll iteratively include tracks from individuals (nested for-loop).'
    # This order will differ per iteration (surface for-loop)
    individ_id_order <- contours_sf %>%
      as_tibble() %>%
      distinct(individ_id) %>%
      pull() %>%
      sample()

    # Nested for-loop to iteratively include tracks from individuals
    for(n in 1:n_individuals){
      # n = 5

      selected_tracks <- contours_sf %>%
        group_by(individ_id) %>%
        sample_n(1) %>%
        ungroup() %>%
        sample_n(n)

      merged_kernels <- selected_tracks %>%
        st_transform(crs = colony_proj.laea) %>%
        st_union()

      kernel_area <- merged_kernels %>%
        st_area() %>%
        as.numeric()

      XX <- tibble(species = species,
                   colony = colony,
                   "month" = month,
                   n_inds = n,
                   it = i,
                   KDE_contour = percentage,
                   area = kernel_area,
                   "h_par" = h_par)

      Kernel_areas <- Kernel_areas %>%
        bind_rows(XX)

    }

  }

  results_list <- list("Kernel_areas" = Kernel_areas,
                       "colony_projection" = colony_proj.laea)
  return(results_list)
}
