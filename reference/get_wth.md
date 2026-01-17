# Get Weather Data for Modelling Disease

This function is a wrapper for the
[`nasapower::get_power()`](https://docs.ropensci.org/nasapower/reference/get_power.html)
with predefined parameters suitable for use in epicrop.

## Usage

``` r
get_wth(lonlat, dates, duration)
```

## Arguments

- lonlat:

  A numeric vector of geographic coordinates for a cell or region
  entered as x, y coordinates.

- dates:

  A character vector of start and end dates in that order.

- duration:

  A numeric value indicating how many days a single growing season being
  simulated is to be. If this is used, only the start date will be used
  and the end date will be ignored if supplied. This must match the
  `duration` parameter value passed along to
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
  or any of the `predict` family of functions.

## Value

A `epicrop.wth`, a data frame based on a
[`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html),
object suitable for use in epicrop containing weather data, dates and
geolocation information (LAT/LON values) with the following columns:

|                |                                                        |
|----------------|--------------------------------------------------------|
| **Field Name** | **Value**                                              |
| *YYYYMMDD*     | Date as Year Month Day (ISO8601)                       |
| *DOY*          | Consecutive day of year, commonly called "Julian date" |
| *TEMP*         | Mean daily temperature (°C)                            |
| *TMIN*         | Daily minimum temperature (°C)                         |
| *TMAX*         | Daily maximum temperature (°C)                         |
| *RHUM*         | Mean daily relative humidity (%)                       |
| *RAIN*         | Mean daily rainfall (mm)                               |
| *LAT*          | Latitude of area of interest                           |
| *LON*          | Longitude of area of interest.                         |

## Author

Adam H. Sparks, <adamhsparks@gmail.com>

## Examples

``` r

# get weather for IRRI Zeigler Experiment Station in wet season 2000 from the
# default NASA POWER data.
power <- get_wth(
  lonlat = c(121.25562, 14.6774),
  dates = c("2000-06-30", "2000-12-31")
)

# get 120 days of weather for IRRI Zeigler Experiment Station in wet season
# 2000 by specifying the duration but not the end-date and specifying to use
# POWER data.
power <- get_wth(
  lonlat = c(121.25562, 14.6774),
  dates = "2000-06-30",
  duration = 120
)
```
