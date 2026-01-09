#' Individual example tracks
#'
#' A data frame containing tracks from several individuals
#'
#' @format Each row contains a location estimated linked to a specific date-time
#' \describe{
#'   \item{session_id}{unique identifier for the recorded track}
#'   \item{individ_id}{unique identifier for the tracked individual}
#'   \item{species}{describes which species was tracked}
#'   \item{colony}{describes which breeding colony the tracked individual is associated with}
#'   \item{longitude}{longitude information of the location estimate}
#'   \item{latitude}{latitude information of the location estimate}
#'   \item{sex}{sex information of the tracked individual}
#'   \item{Year}{The year associated with the date-time of the location estimate, determined from the date-time information}
#'   \item{month}{The month associated with the date-time of the location estimate, determined from the date-time information}
#'   \item{W_Year}{The winter (e.g. 2010/2011) associated with the location estimate. January - June relate to the previous winter and July - December relate to the upcoming winter (assuming breeding happens in the northern hemisphere)}
#'   \item{individ_year}{A unique identifier for the tracked individual in a specific winter}
#'   \item{retrieval_year}{}
#' }
"example_tracks"





#' Species specific smoothing parameters
#'
#' A data frame containing species-mean smoothing parameters, calculated from smoothing parameters determined using LSCV per winter track
#'
#' @format ...
#' \describe{
#'   \item{species}{smoothing parameters (h_par) are provided for 15 seabird species}
#'   \item{mean_h}{mean values from track-specific smoothing parameters determined using LSCV}
#' }
"mean_smooths"




#' Example data as would be returned from `KDE_saturation_curve_fun()`
#'
#' A data frame containing information on combined contours surface areas and the parameters of the Michaelis-Menten curve fitted to this data.
#'
#' @format ...
#' \describe{
#'   \item{species}{}
#'   \item{colony}{}
#'   \item{month}{}
#'   \item{n_inds}{}
#'   \item{it}{}
#'   \item{perc}{}
#'   \item{area}{}
#'   \item{h_par}{}
#'   \item{A_mean}{}
#'   \item{B_mean}{}
#'   \item{Repr}{}
#'
#' }
"saturation_curve"





#' Example data to assess uncertainty of the estimated colony representativeness
#'
#' A data frame example data that will help indicate the uncertainty around the estimated colony representativeness
#'
#' @format ...
#' \describe{
#'   \item{species}{}
#'   \item{colony}{}
#'   \item{N_ind}{}
#'   \item{A_mean}{}
#'   \item{B_mean}{}
#'   \item{Repr_5}{}
#'   \item{perc}{}
#' }
"Sens_MM"





#' Mutli-species data set for a grouped analysis
#'
#' A data frame containing many species-colonies and their estimated colony representativeness
#'
#' @format ...
#' \describe{
#'   \item{species}{}
#'   \item{colony}{}
#'   \item{N_inds}{}
#'   \item{KDE_contour}{}
#'   \item{Repr}{}
#'   \item{Desired_var_threshold}{}
#'   \item{N_inds_desired_perc}{}
#'   \item{A_mean}{}
#'   \item{B_mean}{}
#'   \item{Perc_desired}{}
#'   \item{PN_prop_missing}{}
#' }
"species_curves_tot_PN"


#' Data set with all contours of the example tracks
#'
#' Created and used in the vignettes
#'
#' @format ...
#' \describe{
#'   \item{species}{}
#'   \item{colony}{}
#'   \item{individ_id}{}
#'   \item{KDE_contour}{}
#'   \item{month}{}
#'   \item{individ_Year}{}
#'   \item{area_km2}{}
#'   \item{geometry}{}
#'   \item{W_Year}{}
#'   \item{retrieval_year}{}
#'   \item{h_par}{}
#' }
"contours_all"



#' Example output from `KDE_combine_areas_fun()`
#'
#' Created and used in the vignettes
#'
#' @format ...
#' \describe{
#'   \item{list}{}
#'
#' }
"Combine"



#' Example output from `KDE_Repr_per_retrieval_year()`
#'
#' Used in the vignettes
#'
#' @format ...
#' \describe{
#'   \item{XXX}{}
#'
#' }

"retrieval_year_repr"
