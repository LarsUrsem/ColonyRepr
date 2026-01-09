# Vignette for assessing colony representativeness using ColonyRepr

``` r
library(ColonyRepr)
library(tidyverse)
library(sf)
library(units)
```

## Here an example of how to estimate within-colony variation in wintering site selection and how much of that variation in currently captured

### Data

We start by looking into the example data attached to this package

``` r
glimpse(example_tracks)
#> Rows: 1,288
#> Columns: 12
#> $ session_id     <chr> "N032_1882-06-02", "N032_1882-06-02", "N032_1882-06-02"…
#> $ individ_id     <chr> "NOS_5187199", "NOS_5187199", "NOS_5187199", "NOS_51871…
#> $ species        <chr> "species_name", "species_name", "species_name", "specie…
#> $ colony         <chr> "colony_name", "colony_name", "colony_name", "colony_na…
#> $ longitude      <dbl> 41.26119, 39.82857, 38.12465, 37.43089, 37.84067, 37.52…
#> $ latitude       <dbl> 52.21771, 52.08976, 51.78031, 51.77008, 52.01203, 51.82…
#> $ sex            <chr> "unknown", "unknown", "unknown", "unknown", "unknown", …
#> $ Year           <dbl> 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1…
#> $ month          <dbl> 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,…
#> $ W_Year         <chr> "1882/1883", "1882/1883", "1882/1883", "1882/1883", "18…
#> $ individ_Year   <chr> "NOS_5187199#1882/1883", "NOS_5187199#1882/1883", "NOS_…
#> $ retrieval_year <dbl> 1883, 1883, 1883, 1883, 1883, 1883, 1883, 1883, 1883, 1…
```

The structure of this data matches the SEATRACK data extracted using
getPositions() from the seatrackR-package. In case different data is
used, it needs the following columns:

- species (character)
- colony (character)
- individual_id (character)
- Track_id (character)(in the example data, first the non-breeding
  seasons was determined - e.g. 2020/2021 - which was then merged with
  the individual_id - e.g. “RINGNUMBER_2020/2021”)
- session_id (in case an individual could have been tracked on multiple
  occasions)(character)
- timestamp/date_time (dttm)
- longitude (numeric)
- latitude (numeric)

In addition, a smoothing parameter (h) is required to determine the
utilization distribution (using adehabitatHR::kernelUD()). For the
SEATRACK species, individual-level h-parameters were determined based on
location estimates from November-February. Next, the species-average was
calculated (and saved in a tibble: mean_smooths). These species-means
(Atlantic puffin = 35, Black-legged kittiwake = 55, Brünnich’s guillemot
= 35, Common eider = 27, Common guillemot = 35, European shag = 32,
Glaucous gull = 36, Great skua = 56, Herring gull = 32, Leach’s storm
petrel = 77, Lesser black-backed gull = 40, Little auk = 35, Northern
fulmar = 56, Northern gannet = 10, Razorbill = 35) were then used for
all individuals of the matching species.  
  

### Selecting migratory tracks

First, we’ll have to identify which tracks we deem complete enough to
get an accurate/representative sample of location estimates. We do that
using KDE_filter_tracks_fun(). This function will generate an output in
a list that contains two elements: 1) A dataset containing all the
tracks within the colony that meet the required number of locations to
be assessed (default = 10 location estimates per month); 2) a vector
with the names of the tracks that meet the requirements.

``` r
Tracks <- ColonyRepr::KDE_filter_tracks_fun(
      example_tracks, # This has to be a dataset filtered for your species-colony of interest.
      month. = 12 # Let's focus on december for now
    )

names(Tracks)
#> [1] "Dataset" "Tracks"
```

You’ll find that *Tracks* (a list) contains two elements named *Dataset*
and *Tracks*

These elements contain the original data set filtered for the tracks
with sufficient data.

``` r
glimpse(Tracks$Dataset)
#> Rows: 648
#> Columns: 12
#> $ session_id     <chr> "N032_1882-06-02", "N032_1882-06-02", "N032_1882-06-02"…
#> $ individ_id     <chr> "NOS_5187199", "NOS_5187199", "NOS_5187199", "NOS_51871…
#> $ species        <chr> "species_name", "species_name", "species_name", "specie…
#> $ colony         <chr> "colony_name", "colony_name", "colony_name", "colony_na…
#> $ longitude      <dbl> 13.927535, 13.544078, 14.205181, 18.955004, 15.683746, …
#> $ latitude       <dbl> 51.13594, 51.45798, 51.55879, 51.46185, 51.50836, 51.45…
#> $ sex            <chr> "unknown", "unknown", "unknown", "unknown", "unknown", …
#> $ Year           <dbl> 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1882, 1…
#> $ month          <dbl> 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,…
#> $ W_Year         <chr> "1882/1883", "1882/1883", "1882/1883", "1882/1883", "18…
#> $ individ_Year   <chr> "NOS_5187199#1882/1883", "NOS_5187199#1882/1883", "NOS_…
#> $ retrieval_year <dbl> 1883, 1883, 1883, 1883, 1883, 1883, 1883, 1883, 1883, 1…
Example_track_ids <- Tracks$Tracks[1:N_example_sample] # To provide an example
```

  
  

### Utilization densities

Now that we’ve established which tracks we want to assess, we can move
on to determining utilization densities for the selected tracks. We do
this using KDE_UD_fun(), which is essentially an altered version of
adehabitatHR::kernelUD().

``` r
UD_list <- list()

# We here filter for the appropriate smoothing parameter (h) for the Atlantic puffin
h_par <- mean_smooths %>%
  filter(species == "Atlantic puffin") %>%
  pull(mean_h)

# We iterate over the selected tracks to determine individual-level UDs and store these in UD_list
for (i in Example_track_ids){ # For this example we'll only do this for the first five tracks 
  UD_list[[i]] <- ColonyRepr::KDE_UD_fun(
    Tracks$Dataset %>% filter(individ_Year == i),
    h_par
  )
}
```

KDE_UD_fun() returns a list with two elements: 1) an estUDm-object as
produced by adehabitatHR::kernelUD() and 2) projection-string that was
based on the individual tracking data. We’ll use both these objects in
the next step.  
  

### Kernel density contours

Now we can use these track-level UDs to determine track-level kernel
density contours, for any contour percentage we’re interested in,
e.g. 50%.

``` r
contours_list <- lapply(UD_list, FUN = function(x) {
        ColonyRepr::KDE_contours_fun(
          UD = x$UD,
          projection = x$projection,
          KDE_contour = 50, # Set to contour value of interest
          species_colony_data = Tracks$Dataset,
          h_par
        )
      })
```

[`KDE_contours_fun()`](https://larsursem.github.io/ColonyRepr/reference/KDE_contours_fun.md)
will return an sf-object with a (multi)polygon as geometry, capturing
the area of the contour value of interest.

``` r
contours_list[[1]]
#> Simple feature collection with 1 feature and 10 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 10.0803 ymin: 50.06108 xmax: 19.79772 ymax: 52.10055
#> Geodetic CRS:  WGS 84
#> # A tibble: 1 × 11
#>   individ_Year      area_km2                  geometry species colony individ_id
#> * <chr>                <dbl>        <MULTIPOLYGON [°]> <chr>   <chr>  <chr>     
#> 1 NOS_5187199#1882…   66838. (((17.71038 50.17179, 17… specie… colon… NOS_51871…
#> # ℹ 5 more variables: W_Year <chr>, month <dbl>, retrieval_year <dbl>,
#> #   KDE_contour <dbl>, h_par <dbl>
```

  
At this point *contours_list* contains separate sf-dataframes per track,
which is not the format we can easily use in the next step. We’ll thus
apply bind_rows() to get all contour information into one sf-dataframe:

``` r
contours_all <- bind_rows(contours_list)
contours_all
#> Simple feature collection with 11 features and 10 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: -27.22379 ymin: 41.22821 xmax: 19.79772 ymax: 52.70801
#> Geodetic CRS:  WGS 84
#> # A tibble: 11 × 11
#>    individ_Year     area_km2                  geometry species colony individ_id
#>  * <chr>               <dbl>            <GEOMETRY [°]> <chr>   <chr>  <chr>     
#>  1 NOS_5187199#188…   66838. MULTIPOLYGON (((17.71038… specie… colon… NOS_51871…
#>  2 NOS_5187201#188…   34337. POLYGON ((2.406226 44.89… specie… colon… NOS_51872…
#>  3 NOS_5187202#188…   56204. MULTIPOLYGON (((2.215994… specie… colon… NOS_51872…
#>  4 NOS_5187203#188…   62285. MULTIPOLYGON (((-23.3925… specie… colon… NOS_51872…
#>  5 NOS_5187232#188…   54515. MULTIPOLYGON (((-25.2936… specie… colon… NOS_51872…
#>  6 NOS_5187188#188…   27359. POLYGON ((-8.277293 47.1… specie… colon… NOS_51871…
#>  7 NOS_5187256#188…   94122. MULTIPOLYGON (((-13.0679… specie… colon… NOS_51872…
#>  8 NOS_5187190#188…   46378. POLYGON ((-12.41866 44.1… specie… colon… NOS_51871…
#>  9 NOS_5187195#188…   54782. POLYGON ((-6.332669 48.3… specie… colon… NOS_51871…
#> 10 NOS_5187227#188…   37775. POLYGON ((-26.97192 41.7… specie… colon… NOS_51872…
#> 11 NOS_5187228#188…   45809. MULTIPOLYGON (((-23.2491… specie… colon… NOS_51872…
#> # ℹ 5 more variables: W_Year <chr>, month <dbl>, retrieval_year <dbl>,
#> #   KDE_contour <dbl>, h_par <dbl>
```

  
  

Let’s quickly visualise what we’ve done so far. We’ll take one of the
assessed tracks and plot it on a map (dark points), including the kernel
contour we’ve just created (blue polygon).
![](Pipeline_vignette_files/figure-html/Contours%20on%20map-1.png)

### Combining kernel areas

We can feed these track-specific polygons into the next function in our
pipeline: KDE_combine_areas_fun(). The function will iteratively overlay
track-level contours onto the same map and calculate the area covered by
the combined contours. It will start with just one contour (randomly
selected) and iteratively increase the number of contours by 1 to a
maximum of the total number of individuals tracked within the colony
(from n=1 to n=max is one sequence). If an individual is tracked over
multiple non-breeding seasons, only one track will be used in this
procedure. Each new iteration within a sequence will contain the same
information as used in the previous iteration (i.e. track-contours used
before will be used again), with the addition of a ‘new’ contour.  
To perform this procedure, the function will take an sf-dataframe with
track-level contours, as well as the dataset created by
KDE_filter_tracks_fun(). It will use this dataset to determine an
appropriate projection based on all available colony tracking data. As
KDE_combine_areas_fun() will perform an iteration to minimize sampling
bias, it also asks for an n_iterations argument (default = 20).

``` r
Combine <- ColonyRepr::KDE_combine_areas_fun(
        contours_sf = contours_all,
        tot_loc_data = Tracks$Dataset,
        n_iterations = 20
      )
class(Combine)
#> [1] "list"
names(Combine)
#> [1] "Kernel_areas"      "colony_projection"
```

As you can see, KDE_combine_areas_fun() will return a list with two
objects: 1) a dataframe with with the computed surface area (km2) per
iteration (including additional necessary information), and 2) the
(colony-appropriate) projection used to calculate the areas.  
We can roughly visualise this iteration process for our example data as
follows:

    #> Warning: `x` must be a one- or two-column data frame in `deframe()`.

![](Pipeline_vignette_files/figure-html/combining%20contours%20visualisation-1.png)

You can see that contours get added iteratively, and that a new surface
area is calculated. Surface area is thus expected to increase with every
added contour, but only if the new contour covers an area that hadn’t
been covered by the other contours yet. Otherwise, if no new area is
covered, the surface area of all contours combined will not increase.
For that reason, the increase in surface area in the last iteration is
relatively small, since the new contour overlaps a lot with one of the
contours that was already present.  
In this visualisation, it also becomes clear that contour sizes differ
among tracks, even though the same percentages were used to construct
the contours (50% in our example). As such, differences in sizes arise
from differences in movement behaviour during the assessed period
(December in our example). Individuals that move around will get bigger
contours compared to individuals that don’t move as much. This is just
good to be aware of.  
  

### Estimating within-colony variation

We can use the iteration-dataframe to first calculate a mean surface per
N-contours included in the calculation (a summarised-dataframe). Then,
we’ll fit a Michaelis-Menten function to the N-contours vs mean surface
area data. This can be done using KDE_saturation_curve_fun() as follows:

``` r
Saturation_info <- KDE_saturation_curve(Combine$Kernel_areas)
class(Saturation_info)
#> [1] "list"
names(Saturation_info)
#> [1] "Kernel_areas"            "Kernel_areas_summarised"
```

This will create another list as output, containing 1) the original
iteration-dataframe including parameter information from the best-fit
MM-curve (provided in the columns *A_mean* and *B_mean*), and 2) the
summarised-dataframe containing the same parameter information with the
addition of representativeness per iteration (*Repr*) in an extra
column. This is the ultimate value (as a percentage of the estimated
total within-colony variation) we’ve been after throughout this analysis
(*Repr* for *n_inds* == max(*n_inds*)).

``` r
Saturation_info$Kernel_areas_summarised %>% 
  filter(n_inds == max(n_inds)) %>% 
  pull(Repr)
#> [1] 10.77463
```

  
  

### Plotting the curve

For the final step of this analysis, we can visualise the
Michaelis-Menten curve fitted to our data:

![](Pipeline_vignette_files/figure-html/Plotting%20the%20curve-1.png)  
  
In this worked out example we only used ´r N_example_sample´ tracks from
our sample data. This is obviously not a lot of information, which
results in low colony representativeness (and also high uncertainty as
you’ll see later).  
A similar plot for a colony where many individuals have been tracked may
look something like this:

    #> `summarise()` has grouped output by 'species', 'colony', 'h_par', 'month',
    #> 'A_mean', 'B_mean'. You can override using the `.groups` argument.

![](Pipeline_vignette_files/figure-html/full%20saturation%20curve-1.png)
Now, since the Michaelis-Menten curve is fitted to a much higher number
of data points, it seems intuitive that the uncertainty around that fit
is much lower compared to our initial fit. In the next vignette, we’ll
explore how we can get a better understanding of that uncertainty.

  
  
