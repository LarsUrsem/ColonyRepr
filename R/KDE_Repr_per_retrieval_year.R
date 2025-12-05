#' Function to determine representativeness of data collected up to and including certain retrieval years.
#'
#' @param vertices_sf An sf-object with individual-level kernels for a single species-colony already filtered for a desired KDE contour percentage
#' @param n_iterations How many iterations should be used in the area-combining process
#'
#' @returns A tibble containing estimated representativeness after including data from a given retrieval year, thus assessing all data collected up to that point.
#' @export
#'
#' @import dplyr
#' @import sf
KDE_repr_per_retrieval_year <- function(vertices_sf,
         n_iterations = 20
){
  species <- unique(vertices_sf$species)
  colony <- unique(vertices_sf$colony)
  month <- unique(vertices_sf$month)
  percentage <- unique(vertices_sf$perc)



  individual_migrations <- vertices_sf %>%
    as_tibble() %>%
    distinct(individ_Year) %>%
    pull()

  n_individuals <- vertices_sf %>%
    as_tibble() %>%
    distinct(individ_id) %>%
    nrow()


  retrieval_groups <- colony_dat %>%
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

    retrieved_vertices <- Colony_vertices %>%
      dplyr::filter(individ_Year %in% retrieved_tracks)

    n_individuals_included <- retrieved_vertices %>%
      as_tibble() %>%
      distinct(individ_id) %>%
      nrow()

    Combine <- KDE_combine_areas_fun(vertices_sf = retrieved_vertices,
                                     tot_loc_data = SEATRACK_tot,
                                     n_iterations = 20)

    Repr <- KDE_saturation_curve(Combine$Kernel_areas)
    Repr_perc <- Repr$Kernel_areas_summarised %>%
      dplyr::filter(n_inds == max(n_inds)) %>%
      pull(prop_of_100perc)
    Year_Repr <- Year_Repr %>%
      bind_rows(tibble("species" = species,
                       "colony" = colony,
                       "retrieval_year" = y,
                       "n_individuals" = n_individuals_included,
                       "perc" = p2,
                       "Repr" = Repr_perc
      ))
  }

  return(Year_Repr)
}
