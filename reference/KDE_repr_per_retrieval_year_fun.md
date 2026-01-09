# Function to determine representativeness of data collected up to and including certain retrieval years.

Function to determine representativeness of data collected up to and
including certain retrieval years.

## Usage

``` r
KDE_repr_per_retrieval_year_fun(
  contours_sf,
  colony_projection,
  n_iterations = 20
)
```

## Arguments

- contours_sf:

  An sf-object with individual-level kernels for a single species-colony
  already filtered for a desired KDE contour percentage. contours_sf
  should also contain a column indicating retrieval year

- colony_projection:

  Provide a projection that is appropriate for the whole colony

- n_iterations:

  How many iterations should be used in the area-combining process

## Value

A tibble containing estimated representativeness after including data
from a given retrieval year, thus assessing all data collected up to
that point.
