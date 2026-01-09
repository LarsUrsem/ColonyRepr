# Function to iteratively combine individual-level kernels to estimate within-colony variation

Function to iteratively combine individual-level kernels to estimate
within-colony variation

## Usage

``` r
KDE_combine_areas_fun(
  contours_sf,
  tot_loc_data,
  colony_projection = NULL,
  n_iterations = 20
)
```

## Arguments

- contours_sf:

  An sf-object with individual-level kernels already filtered for a
  desired KDE contour percentage

- tot_loc_data:

  A dataframe with species-colony location information

- colony_projection:

  (Optional) Provide a projection that is appropriate for the whole
  colony

- n_iterations:

  Set how many iterations you'd like to use

## Value

A list containing:

- Kernel_areas:

  A tibble with KDE bootstrap results

- colony_projection:

  The LAEA projection used
