# Calculate hourly temperature for a given day or multiple days

Calculate hourly temperature for a given day or multiple days

## Usage

``` r
.diurnal_temp(lat, doy, tmin, tmax)
```

## Arguments

- lat:

  Latitude (scalar or vector).

- doy:

  Day of year (scalar or vector).

- tmin:

  Minimum temperature (scalar or vector).

- tmax:

  Maximum temperature (scalar or vector).

## Value

Numeric vector of 24 hourly temperatures for single day, or numeric
vector of 24\*n hourly temperatures for multiple days.

## Author

Robert J. Hijmans with modifications by Adam H. Sparks
