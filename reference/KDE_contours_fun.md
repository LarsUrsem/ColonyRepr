# Extract kernel vertices from UD-object

Extract kernel vertices from UD-object

## Usage

``` r
KDE_contours_fun(UD, KDE_contour, species_colony_data, h_par, projection)
```

## Arguments

- UD:

  An \`estUD\` object as produced by \`KDE_UN_fun()\` or
  \`adehabitatHR::kernelUD()\`

- KDE_contour:

  The KDE contour percentage you're interested in (e.g., 75)

- species_colony_data:

  Data frame with species-colony spatial data (e.g. as produced by
  \`KDE_filter_tracks_fun()\` in .\$Dataset)

- h_par:

  smoothing parameter that was used in \`KDE_UN_fun()\` (numeric). This
  is just to keep track of which smoothing parameter was used, it is not
  used in any analysis.

- projection:

  provide the projection character string that is appropriate for the
  assessed track (e.g. as returned from \`KDE_UD_fun()\`). This
  projection will be used to calculate the surface area of the
  constructed contour polygon.

## Value

An sf-object containing the kernel contour as a polygon geometry and
some additional track information
