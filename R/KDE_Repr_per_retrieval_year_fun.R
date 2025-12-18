#' Function to determine representativeness of data collected up to and including certain retrieval years.
#'
#' @param contours_sf An sf-object with individual-level kernels for a single species-colony already filtered for a desired KDE contour percentage. contours_sf should also contain a column indicating retrieval year
#' @param n_iterations How many iterations should be used in the area-combining process
#'
#' @returns A tibble containing estimated representativeness after including data from a given retrieval year, thus assessing all data collected up to that point.
#' @export
#'
#' @import dplyr
#' @import sf
KDE_repr_per_retrieval_year_fun <- function(contours_sf,
                                        colony_projection,
                                        n_iterations = 20
){
  species <- unique(contours_sf$species)
  colony <- unique(contours_sf$colony)
  month <- unique(contours_sf$month)
  KDE_contour <- unique(contours_sf$KDE_contour)



  individual_migrations <- contours_sf %>%
    as_tibble() %>%
    distinct(individ_Year) %>%
    pull()

  n_individuals <- contours_sf %>%
    as_tibble() %>%
    distinct(individ_id) %>%
    nrow()


  retrieval_groups <- contours_sf %>%
    as_tibble() %>%
    distinct(retrieval_year, individ_Year) %>%
    arrange(retrieval_year)

  retrieval_years <- retrieval_groups %>%
    distinct(retrieval_year) %>%
    pull()

  Year_Repr <- tibble()
  for(y in retrieval_years){

    retrieved_tracks <- retrieval_groups %>%
      dplyr::filter(retrieval_year <= y) %>%
      pull(individ_Year)

    retrieved_contours <- contours_sf %>%
      as_tibble() %>%
      dplyr::filter(individ_Year %in% retrieved_tracks)

    n_individuals_included <- retrieved_contours %>%
      as_tibble() %>%
      distinct(individ_id) %>%
      nrow()

    retrieved_contours_sf <- retrieved_contours %>%
      st_as_sf()

    Combine <- KDE_combine_areas_fun(contours_sf = retrieved_contours_sf,
                                     colony_projection = colony_projection,
                                     n_iterations = 20)

    Repr <- KDE_saturation_curve(Combine$Kernel_areas)
    x <- Repr$Kernel_areas_summarised %>%
      dplyr::filter(n_inds == max(n_inds)) %>%
      mutate(retrieval_year = y,
             "KDE_contour" = KDE_contour
      ) %>%
      dplyr::select(species, colony, retrieval_year, n_inds, KDE_contour, mean, A_mean, B_mean, Repr)
    Year_Repr <- Year_Repr %>%
      bind_rows(x)
  }

  return(Year_Repr)
}
