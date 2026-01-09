# Multi-colony analysis SEATRACK

``` r
library(ColonyRepr)
library(tidyverse)
library(ggridges)
```

To follow the steps described in this vignette, we must have determined
colony representativeness (or *Repr*) for a decent number of
species-colonies, preferably covering multiple species and colonies per
species.  
We’ve done that already and will now just load that data in a tibble
that contains information on species, colony, month, N tracked
individuals, used kernel contours, colony representativeness and
uncertainty. MONTH AND UNCERTAINTY ARE NOT THERE YET

``` r
glimpse(species_curves_tot_PN)
#> Rows: 3,195
#> Columns: 11
#> $ species               <chr> "Arctic tern", "Arctic tern", "Arctic tern", "Ar…
#> $ colony                <chr> "East Bay", "East Bay", "East Bay", "East Bay", …
#> $ N_inds                <int> 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, …
#> $ KDE_vertice           <dbl> 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, …
#> $ Repr                  <dbl> -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, …
#> $ Desired_var_threshold <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ N_inds_desired_perc   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ A_mean                <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ Perc_desired          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ B_mean                <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
#> $ PN_prop_missing       <dbl> 0.3333333, 0.3333333, 0.3333333, 0.3333333, 0.33…
```

We can take this dataset and use it to visualise the species-colonies
representativeness that we currently capture within SEATRACK.  
For example, we can make a histogram based on the *Repr* values from all
assessed species-colonies:
![](Grouped_files/figure-html/total%20histogram-1.png)

You can see that within the assessed dataset of many species-colonies,
there is a wide range of currently captured representativeness, from
around 10% to almost 100%. I’ve also indicated some particular
differences among species-colonies through the use of different fill
colors. We have many species-colonies that were not assessed in this
analysis due to a low sample size (orange; determined with the
`min_Ninds_per_col`-argument in
[`KDE_filter_tracks_fun()`](https://larsursem.github.io/ColonyRepr/reference/KDE_filter_tracks_fun.md)).
We also have several species-colonies that we identified to be affected
by polar night (blue) to a degree that inference of their location
estimates is likely biased towards non-polar night areas.  
  
We can also split this up per species to get a better overview on a
species-level:

![](Grouped_files/figure-html/species%20Repr%20ridges-1.png)

Now we can nicely see that variation in colony representativeness is
present among both species and colonies within a species. For the
species at the top of this plot we generally capture a high amount of
the estimated within-colony variation whereas we generally don’t capture
as much for the species at the bottom of this plot. Then there are also
several species where the histogram is very spread out, indicating
strong among-colony variation.  
  
Using the parameters of the Michaelis-Menten curves that were fitted to
each species-colony, we can also predict how many individuals we’d need
to track in order to obtain a desired colony representativess
percentage, for example 50%.
![](Grouped_files/figure-html/Desired%20Repr-1.png)
