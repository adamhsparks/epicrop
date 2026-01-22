# Calculate hourly temperature for a given day

Uses doy, location (lat) and TMIN and TMAX to calculate hourly
temperatures.

## Usage

``` r
.diurnal_temp(lat, doy, tmin, tmax)
```

## Arguments

- lat:

  Latitude as provided by `wth` via
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)

- doy:

  Sequential day of year as provided by `wth` via
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)

- tmin:

  Minimum temperature as provided by `wth` via
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)

- tmax:

  Maximum temperature as provided by `wth` via
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)

- dayl:

  Daylight hours as provided by `wth` via
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)

## Value

A numeric vector of hourly temperature values for the duration of `wth`
data

## Author

Robert J. Hijmans with modifications by Adam H. Sparks

## Examples

``` r
wth <- get_wth(
  lonlat = c(121.25562, 14.6774),
  dates = c("2000-06-30", "2000-12-31")
)

.diurnal_temp(
  lat = wth$LAT,
  doy = wth$DOY,
  tmin = wth$TMIN,
  tmax = wth$TMAX
)
#> Error in .diurnal_temp(lat = wth$LAT, doy = wth$DOY, tmin = wth$TMIN,     tmax = wth$TMAX): could not find function ".diurnal_temp"
```
