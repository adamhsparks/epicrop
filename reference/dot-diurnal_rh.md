# Calculate hourly relative humidity for a given day

Calculate hourly relative humidity for a given day

## Usage

``` r
.diurnal_rh(rh, tmin, tmax, tmp, lat, doy)
```

## Arguments

- rh:

  Mean daily relative humidity.

- tmin:

  Minimum temperature.

- tmax:

  Maximum temperature.

- tmp:

  Mean daily temperature.

- lat:

  Latitude.

- doy:

  Day of year.

## Value

List of numeric vectors (one per day, 24 values each).

## Author

Adam H. Sparks, <adamhsparks@gmail.com>
