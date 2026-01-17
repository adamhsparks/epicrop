# Calculate hourly relative humidity for a given day

Uses mean rh, doy, location (lat), mean tmp, tmin and tmax to calculate
hourly rh values.

## Usage

``` r
.diurnal_rh(rh, tmin, tmax, tmp, lat, doy, yyyymmdd)
```

## Arguments

- rh:

  Mean daily relative humidity as provided by `wth` via
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md).

- tmin:

  Minimum temperature as provided by `wth` via
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md).

- tmax:

  Maximum temperature as provided by `wth` via
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md).

- tmp:

  Mean daily temperature as provided by `wth` via
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md).

- lat:

  Latitude provided by `wth` via
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md).

- doy:

  Sequential day of year as provided by `wth` via
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md).

- yyyymmdd:

  Date (YYYY-MM-DD) as provided by `wth` via
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md).

## Value

A numeric vector of relative humidity values over 24 hrs.

## Examples
