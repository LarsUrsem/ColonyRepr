# Determine Utilization Density

Determine Utilization Density

## Usage

``` r
KDE_UD_fun(migration_track, h_par)
```

## Arguments

- migration_track:

  individual-level migration track, with a longitude and latitude column

- h_par:

  smoothing parameter (numeric)

## Value

Returns a list with two elements\> 1) An object of class \`estUD\` as
produced by \`adehabitatHR::kernelUD()\`. 2) The track-specific
projection (character string)
