# Function to determine a saturation curve per number of included individuals.

Function to determine a saturation curve per number of included
individuals.

## Usage

``` r
KDE_repr_per_individual_fun(combined_areas_df)
```

## Arguments

- combined_areas_df:

  An tibble with combined-area information as produced by
  \`KDE_combine_areas_fun()\`.

## Value

A tibble containing estimated representativeness after including data
from a given retrieval year, thus assessing all data collected up to
that point.
