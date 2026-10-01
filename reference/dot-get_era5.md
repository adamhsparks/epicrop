# Fetch Daily Weather from ERA5-Land via Open-Meteo

Downloads hourly ERA5-Land data from the Open-Meteo Archive API and
aggregates it to daily values matching the columns of the POWER-derived
output:

- `TEMP`: mean of hourly `temperature_2m`

- `TMIN`/`TMAX`: min/max of hourly \`temperature_2m

- `RHUM`: mean of hourly relative humidity derived from `temperature_2m`
  and `dewpoint_2m` with
  [`.rh_from_dewpoint()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-rh_from_dewpoint.md).

- `RAIN`: sum of hourly `precipitation`

## Usage

``` r
.get_era5(lon, lat, start, end)
```

## Arguments

- lon, lat:

  Numeric longitude and latitude.

- start, end:

  Date start and end (inclusive).

## Value

data.table of class `epicrop.wth`.
