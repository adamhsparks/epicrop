# Fetch Daily Weather from ERA5 via Open-Meteo

Downloads hourly ERA5 data from the Open-Meteo Archive API and
aggregates it to daily values matching the columns of the POWER-derived
output:

- `TEMP`: mean of hourly `temperature_2m`

- `TMIN`/`TMAX`: min/max of hourly `temperature_2m`

- `RHUM`: mean of hourly `relative_humidity_2m`

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

## Details

Hourly data are used because the Open-Meteo Archive API does not offer a
daily mean relative humidity variable.

## Time zones

The request uses `timezone = "auto"`, so Open-Meteo resolves the
location's local time zone, interprets `start_date`/`end_date` as local
calendar dates and returns hourly timestamps in local time. Hours are
grouped by their local calendar date, so each daily value covers one
local day.
