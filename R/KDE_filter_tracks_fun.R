#' Filter for tracks with sufficient location estimates
#'
#' `KDE_filter_tracks_fun()` takes a tibble with (GLS-)tracking data from a colony and determines which migratory tracks (within a specified month) contain sufficient location estimates.
#' It then goes on to check if a colony has enough tracked individuals with sufficient location estimates.
#'
#' @param dataset A tibble with location estimates from a tracked colony. Must contain columns with species andcolony information.
#' Also, a month-column should be present, indicating in which month(numeric) a location estimate was made. As well as a column with a unique identifier per yearly migration track (`individ_Year`).
#' @param month. The month for which you wish to determine which tracks have sufficient location estimates (numeric)
#' @param min_points_per_track Set to the minimum number of location estimates per month you deem sufficient in your analysis (numeric)
#' @param min_Ninds_per_col Set to the minimum number of individuals with sufficient migration tracks that you deem appropriate for you analysis (numeric)
#'
#' @return Returns a list with two elements:
#' 1) a tibble containing all the migratory tracks of the colony that meet the required number of locations (as set in `min_points_per_track`);
#' 2) a vector with the names of the tracks that meet the requirements.
#' @export
#'
#' @import dplyr
#' @importFrom magrittr %>%
KDE_filter_tracks_fun <- function(dataset,
                                  # species.,
                                  # colony.,
                                  month. = 12,
                                  # h_par,
                                  min_points_per_track = 10,
                                  min_Ninds_per_col = 5){

  # filter for the spec-col data of interest
  spec_col_dat <- dataset %>%
    dplyr::filter(month == month.)

  # Determine which tracks have 10 or more location estimates
  tracks_over_x_locations <- spec_col_dat %>%
    group_by(individ_Year) %>%
    count() %>%
    dplyr::filter(n >= min_points_per_track) %>%
    pull(individ_Year)

  if(length(tracks_over_x_locations) == 0) {return(paste0("Not enough individuals with sufficient location estimates in the colony: ", n_individuals, " individuals"))}

  # Only include those tracks with 10 or more location estimates
  spec_col_dat2 <- spec_col_dat %>%
    dplyr::filter(individ_Year %in% tracks_over_x_locations)

  # Determine how many individuals these tracks belong to
  n_individuals <- n_distinct(spec_col_dat2$individ_id)

  if(n_individuals < min_Ninds_per_col){return(paste0("Not enough individuals with sufficient location estimates in the colony: ", n_individuals, " individuals"))}

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

