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

``` r
wth <- get_wth(
  lonlat = c(121.25562, 14.6774),
  dates = c("2000-06-30")
)

.diurnal_rh(
  rh = wth$RHUM,
  tmin = wth$TMIN,
  tmax = wth$TMAX,
  tmp = wth$TEMP,
  lat = wth$LAT,
  doy = wth$DOY,
  yyyymmdd = wth$YYYYMMDD
)
#> $`2000-06-30`
#>  [1] 100.00000 100.00000 100.00000 100.00000 100.00000 100.00000  94.58384
#>  [8]  89.21482  84.67398  81.01595  78.26627  76.43329  75.51746  75.51746
#> [15]  76.43329  78.26627  81.01595  84.67398  88.79249  92.15830  94.87917
#> [22]  97.06086  98.79907 100.00000
#> 
```
