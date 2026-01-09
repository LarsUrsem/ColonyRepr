# Filter for tracks with sufficient location estimates

\`KDE_filter_tracks_fun()\` takes a tibble with (GLS-)tracking data from
a colony and determines which migratory tracks (within a specified
month) contain sufficient location estimates. It then goes on to check
if a colony has enough tracked individuals with sufficient location
estimates.

## Usage

``` r
KDE_filter_tracks_fun(
  dataset,
  month. = 12,
  min_points_per_track = 10,
  min_Ninds_per_col = 5
)
```

## Arguments

- dataset:

  A tibble with location estimates from a tracked colony. Must contain
  columns with species andcolony information. Also, a month-column
  should be present, indicating in which month(numeric) a location
  estimate was made. As well as a column with a unique identifier per
  yearly migration track (\`individ_Year\`).

- month.:

  The month for which you wish to determine which tracks have sufficient
  location estimates (numeric)

- min_points_per_track:

  Set to the minimum number of location estimates per month you deem
  sufficient in your analysis (numeric)

- min_Ninds_per_col:

  Set to the minimum number of individuals with sufficient migration
  tracks that you deem appropriate for you analysis (numeric)

## Value

Returns a list with two elements: 1) a tibble containing all the
migratory tracks of the colony that meet the required number of
locations (as set in \`min_points_per_track\`); 2) a vector with the
names of the tracks that meet the requirements.
