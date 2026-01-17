# Check POWER data for any missing values

Check POWER data for any missing values

## Usage

``` r
.check_na(.wth)
```

## Arguments

- .wth:

  A data.frame object from
  [nasapower::get_power](https://docs.ropensci.org/nasapower/reference/get_power.html)
  with weather data for checking Checks the data returned from the POWER
  dataset and informs the user if missing values are found. It does not
  do anything other than emitting a message about missing data.

## Value

Called for its side-effects, an invisible `NULL`.

## Examples

``` r
wth <- setDT(
  nasapower::get_power(
    lonlat = c(121.25562, 14.6774),
    dates = "2000-06-30",
    community = "AG",
    pars = c(
      "T2M",
      "T2M_MIN",
      "T2M_MAX",
      "RH2M",
      "PRECTOTCORR"
    ),
    temporal_api = "DAILY"
  )
)

wth[, c("YEAR", "MM", "DD") := NULL][]
#>         LON     LAT   DOY   YYYYMMDD   T2M T2M_MIN T2M_MAX  RH2M PRECTOTCORR
#>       <num>   <num> <int>     <Date> <num>   <num>   <num> <num>       <num>
#> 1: 121.2556 14.6774   182 2000-06-30 26.02   23.34   28.76 88.53       11.01
setnames(
  wth,
  old = c(
    "T2M",
    "T2M_MIN",
    "T2M_MAX",
    "PRECTOTCORR",
    "RH2M"
  ),
  new = c(
    "TEMP",
    "TMIN",
    "TMAX",
    "RAIN",
    "RHUM"
  )
)
.check_na(wth)
```
