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
#> Error in nasapower::get_power(community = "AG", pars = c("T2M", "T2M_MIN",     "T2M_MAX", "RH2M", "PRECTOTCORR"), temporal_api = "DAILY",     lonlat = c(lon, lat), dates = date_vec): ℹ You have supplied more than two dates for start and end.
#> ✖ Please supply only two (2) dates for `dates` as 'YYYY-MM-DD' (ISO8601 format).

.diurnal_rh(
  rh = wth$RHUM,
  tmin = wth$TMIN,
  tmax = wth$TMAX,
  tmp = wth$TEMP,
  lat = wth$LAT,
  doy = wth$DOY,
  yyyymmdd = wth$YYYYMMDD
)
#> Error in .diurnal_rh(rh = wth$RHUM, tmin = wth$TMIN, tmax = wth$TMAX,     tmp = wth$TEMP, lat = wth$LAT, doy = wth$DOY, yyyymmdd = wth$YYYYMMDD): could not find function ".diurnal_rh"
```
