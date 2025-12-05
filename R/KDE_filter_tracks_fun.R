#' Filter for tracks to be assessed in the colony representativeness analysis
#'
#' @param dataset A dataset with location estimates. Must include a species, colony and Month column (tibble)
#' @param species. A species' name (character string)
#' @param colony. A colony's name (character string)
#' @param month. A month (numeric)
#' @param min_points_per_track (numeric)
#' @param min_Ninds_per_col (numeric)
#'
#' @return A results list with the total filtered dataset and a vector containing the migration ids as strings
#' @export
#'
#' @import dplyr
#' @importFrom magrittr %>%
KDE_filter_tracks_fun <- function(dataset,
                                  species.,
                                  colony.,
                                  month. = 12,
                                  # h_par,
                                  min_points_per_track = 10,
                                  min_Ninds_per_col = 5){

  # filter for the spec-col data of interest
  spec_col_dat <- dataset %>%
    dplyr::filter(species == species.,
                  colony == colony.,
                  Month == month.)

  # Determine which tracks have 10 or more location estimates
  tracks_over_x_locations <- spec_col_dat %>%
    group_by(individ_Year) %>%
    count() %>%
    dplyr::filter(n >= min_points_per_track) %>%
    pull(individ_Year)

  if(length(tracks_over_x_locations) == 0) {return(paste0("Not enough migration tracks: ",length(tracks_over_x_locations)))}

  # Only include those tracks with 10 or more location estimates
  spec_col_dat2 <- spec_col_dat %>%
    dplyr::filter(individ_Year %in% tracks_over_x_locations)

  # Determine how many individuals these tracks belong to
  n_individuals <- n_distinct(spec_col_dat2$individ_id)

  if(n_individuals < min_Ninds_per_col){return(paste0("Not enough individuals in the colony: ",colony., " - ", n_individuals, " individuals"))}

  # Extract the migration track IDs for all tracks with 10 or more location estimates
  individual_tracks <- spec_col_dat2 %>%
    distinct(individ_Year) %>%
    pull()

  spec_col_dat_filtered <- spec_col_dat2 %>%
    dplyr::filter(individ_Year %in% individual_tracks)

  results_list <- list("Dataset" = spec_col_dat_filtered,
                       "Tracks"  = individual_tracks)


  return(results_list)

}

