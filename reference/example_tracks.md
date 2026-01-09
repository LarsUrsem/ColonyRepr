# Individual example tracks

A data frame containing tracks from several individuals

## Usage

``` r
example_tracks
```

## Format

Each row contains a location estimated linked to a specific date-time

- session_id:

  unique identifier for the recorded track

- individ_id:

  unique identifier for the tracked individual

- species:

  describes which species was tracked

- colony:

  describes which breeding colony the tracked individual is associated
  with

- longitude:

  longitude information of the location estimate

- latitude:

  latitude information of the location estimate

- sex:

  sex information of the tracked individual

- Year:

  The year associated with the date-time of the location estimate,
  determined from the date-time information

- month:

  The month associated with the date-time of the location estimate,
  determined from the date-time information

- W_Year:

  The winter (e.g. 2010/2011) associated with the location estimate.
  January - June relate to the previous winter and July - December
  relate to the upcoming winter (assuming breeding happens in the
  northern hemisphere)

- individ_year:

  A unique identifier for the tracked individual in a specific winter

- retrieval_year:
