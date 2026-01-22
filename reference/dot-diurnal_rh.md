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
  [`epicrop_sim()`](https://adamhsparks.codeberg.page/epicrop/reference/epicrop_sim.md).

- tmin:

  Minimum temperature as provided by `wth` via
  [`epicrop_sim()`](https://adamhsparks.codeberg.page/epicrop/reference/epicrop_sim.md).

- tmax:

  Maximum temperature as provided by `wth` via
  [`epicrop_sim()`](https://adamhsparks.codeberg.page/epicrop/reference/epicrop_sim.md).

- tmp:

  Mean daily temperature as provided by `wth` via
  [`epicrop_sim()`](https://adamhsparks.codeberg.page/epicrop/reference/epicrop_sim.md).

- lat:

  Latitude provided by `wth` via
  [`epicrop_sim()`](https://adamhsparks.codeberg.page/epicrop/reference/epicrop_sim.md).

- doy:

  Sequential day of year as provided by `wth` via
  [`epicrop_sim()`](https://adamhsparks.codeberg.page/epicrop/reference/epicrop_sim.md).

- yyyymmdd:

  Date (YYYY-MM-DD) as provided by `wth` via
  [`epicrop_sim()`](https://adamhsparks.codeberg.page/epicrop/reference/epicrop_sim.md).

## Value

A numeric vector of relative humidity values over 24 hrs.

## Author

Adam H. Sparks, <adamhsparks@gmail.com>

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
#> $`2000-06-29`
#>  [1] 97.34653 97.78016 98.11937 98.38447 98.59149 97.04739 93.89333 91.02691
#>  [9] 88.53083 86.46838 84.88571 83.81445 83.27418 83.27418 83.81445 84.88571
#> [17] 86.46838 88.53083 90.79842 92.61096 94.05079 95.18958 96.08722 96.79290
#> 
```
