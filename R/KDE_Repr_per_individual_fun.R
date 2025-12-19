#' Function to determine a saturation curve per number of included individuals.
#'
#' @param combined_areas_df An tibble with combined-area information as produced by `KDE_combine_areas_fun()`.
#'
#' @returns A tibble containing estimated representativeness after including data from a given retrieval year, thus assessing all data collected up to that point.
#' @export
#' @keywords internal
#'
#' @import dplyr
#' @import sf
KDE_repr_per_individual_fun <- function(combined_areas_df){
  species <- unique(combined_areas_df$species)
  colony <- unique(combined_areas_df$colony)
  month <- unique(combined_areas_df$month)
  KDE_contour <- unique(combined_areas_df$KDE_contour)

  col_dat_sum <- combined_areas_df %>%
    group_by(species, colony, month, n_inds) %>%
    dplyr::summarise(mean = mean(area),
                     sd = sd(area)) %>%
    mutate(up_bound = mean+sd,
           low_bound = mean-sd)

  tot_N_individs <- max(combined_areas_df$n_inds)

  col_dat_tot <- tibble()
  for(n in 5:tot_N_individs){
    #n = 6

    col_dat_sum_n <- col_dat_sum %>%
      dplyr::filter(n_inds <= n)
    a_start <- max(col_dat_sum_n$mean)
    b_start <- col_dat_sum_n %>%
      mutate(b_start = abs(mean - a_start/2)) %>%
      arrange(b_start) %>%
      slice_head(n = 1) %>%
      pull(n_inds) %>%
      as.numeric()

    # Sometimes the modelling doesn't converge so here we introduce a try() to deal with that and provide NA's for those species-colonies where it didn't converge.
    nls_mean <- 0

    try({

      nls_mean <- nls(mean ~ a*n_inds/(b+n_inds), data = col_dat_sum_n, start = list(a = a_start, b = b_start),
                      algorithm = "port",
                      lower = c(0,1))
      A_mean_n <- coef(summary(nls_mean))[1,1]
      B_mean_n <- coef(summary(nls_mean))[2,1]

    })

    if(inherits(nls_mean, "numeric")){
      print(paste("FAILED TO CONVERGE: ",species, colony, KDE_contour,"KDE", sep = " - "))


      col_dat_sum_N <- col_dat_sum %>%
        dplyr::filter(n_inds == n) %>%
        mutate(#"h_par" = h_par,
               "KDE_contour" = KDE_contour,
               "A_mean" = NA,
               "B_mean" = NA,
               Repr = NA)
    }

    if(inherits(nls_mean, "nls")){

      col_dat_sum_N <- col_dat_sum_n %>%
        dplyr::filter(n_inds == n) %>%
        mutate(#"h_par" = h_par,
               "KDE_contour" = KDE_contour,
               "A_mean" = A_mean_n,
               "B_mean" = B_mean_n,
               Repr = 100/A_mean * mean)

    }

    col_dat_tot <- col_dat_tot %>%
      bind_rows(col_dat_sum_N)


  }
  return(col_dat_tot)

}
