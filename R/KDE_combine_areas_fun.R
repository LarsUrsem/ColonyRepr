#' Function to iteratively combine individual-level kernels to estimate within-colony variation
#'
#' @param vertices_sf An sf-object with individual-level kernels already filtered for a desired KDE contour percentage
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
KDE_combine_areas_fun <- function(vertices_sf,
                                  tot_loc_data, #Needed to determine and use appropriate projection for area calculations
                                  n_iterations = 20
){
  species <- unique(vertices_sf$species)
  colony <- unique(vertices_sf$colony)
  month <- unique(vertices_sf$Month)


  individual_migrations <- vertices_sf %>%
    as_tibble() %>%
    distinct(individ_Year) %>%
    pull()

  n_individuals <- vertices_sf %>%
    as_tibble() %>%
    distinct(individ_id) %>%
    nrow()


  colony_dat <- tot_loc_data %>%
    dplyr::filter(individ_Year %in% individual_migrations,
           Month == month)


  # Determine projection for the colony in general:
  colony_proj.laea    <- paste0("+proj=laea +x_0=0 +y_0=0 +lon_0=",
                                mean(colony_dat$longitude),
                                " +lat_0=",
                                mean(colony_dat$latitude),
                                " +units=km"
  )

  percentage <- vertices_sf %>%
    as_tibble() %>%
    distinct(perc) %>%
    pull()

  h_par <- vertices_sf %>%
    as_tibble() %>%
    distinct(h_par) %>%
    pull()


  Kernel_areas <- tibble()
  counter3 <- 0
  for(i in 1:n_iterations){
    # i = 1

    set.seed(i)
    individ_id_order <- vertices_sf %>%
      as_tibble() %>%
      distinct(individ_id) %>%
      pull() %>%
      sample()

    # counter3 <- counter3 +1
    # print(paste0("Progress: ",s3, " - ", col3," - ", p2, "% - ", counter3, " / ",length(individual_migrations)))

    for(n in 1:n_individuals){
      # n = 5

      selected_tracks <- vertices_sf %>%
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
                   perc = percentage,
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
