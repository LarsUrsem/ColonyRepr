#' Fit saturation curve model to combined areas
#'
#' @param Kernel_areas Data frame with iterated combined areas
#'
#' @return A list with:
#'   \describe{
#'     \item{Kernel_areas}{Input data with A_mean and B_mean added}
#'     \item{Kernel_areas_summarised}{Summary table with fitted curve statistics}
#'     }
#'
#' @export
#'
#' @import stats
#' @import dplyr
KDE_saturation_curve <- function(Kernel_areas
){
  n_iterations <- max(Kernel_areas$it)
  species <- unique(Kernel_areas$species)
  colony <- unique(Kernel_areas$colony)
  month <- unique(Kernel_areas$month)
  h_par <- unique(Kernel_areas$h_par)
  perc <- unique(Kernel_areas$perc)

  col_dat_sum <- Kernel_areas %>%
    group_by(species, colony, h_par, month, n_inds) %>%
    dplyr::summarise(mean = mean(area),
                     sd = sd(area)) %>%
    mutate(up_bound = mean+sd,
           low_bound = mean-sd)

  a_start <- max(col_dat_sum$mean)
  b_start <- col_dat_sum %>%
    mutate(b_start = abs(mean - a_start/2)) %>%
    arrange(b_start) %>%
    slice_head(n = 1) %>%
    pull(n_inds) %>%
    as.numeric()


  # Sometimes the modelling doesn't converge so here we introduce a try() to deal with that and provide NA's for those species-colonies where it didn't converge.
  nls_mean <- 0

  try({

    nls_mean <- nls(mean ~ a*n_inds/(b+n_inds), data = col_dat_sum, start = list(a = a_start, b = b_start),
                    algorithm = "port",
                    lower = c(0,1))
    # summary(nls_mean)

    A_mean <- coef(summary(nls_mean))[1,1]
    B_mean <- coef(summary(nls_mean))[2,1]

  })

  if (inherits(nls_mean, "numeric")){
    print(paste("FAILED TO CONVERGE: ",species, colony, perc,"KDE", sep = " - "))

    Kernel_areas <- Kernel_areas %>%
      mutate("h_par" = h_par,
             A_mean = NA,
             B_mean = NA,
             Repr = NA)

    col_dat_sum <- col_dat_sum %>%
      mutate("h_par" = h_par,
             "KDE_contour" = perc,
             "A_mean" = NA,
             "B_mean" = NA,
             Repr = NA)
  }

  if (inherits(nls_mean, "nls")){

    Kernel_areas <- Kernel_areas %>%
      mutate("h_par" = h_par,
             "A_mean" = A_mean,
             "B_mean" = B_mean,
             Repr = 100/A_mean * area)

    col_dat_sum <- col_dat_sum %>%
      mutate("h_par" = h_par,
             "KDE_contour" = perc,
             "A_mean" = A_mean,
             "B_mean" = B_mean,
             Repr = 100/A_mean * mean)

  }


  results_list <- list("Kernel_areas" = Kernel_areas,
                       "Kernel_areas_summarised" = col_dat_sum)

  return(results_list)
}
