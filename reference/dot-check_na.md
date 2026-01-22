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
#> Error in setDT(nasapower::get_power(lonlat = c(121.25562, 14.6774), dates = "2000-06-30",     community = "AG", pars = c("T2M", "T2M_MIN", "T2M_MAX", "RH2M",         "PRECTOTCORR"), temporal_api = "DAILY")): could not find function "setDT"

wth[, c("YEAR", "MM", "DD") := NULL][]
#> Warning: A shallow copy of this data.table was taken so that := can add or remove 3 columns by reference. At an earlier point, this data.table was copied by R (or was created manually using structure() or similar). Avoid names<- and attr<- which in R currently (and oddly) may copy the whole data.table. Use set* syntax instead to avoid copying: ?set, ?setnames and ?setattr. It's also not unusual for data.table-agnostic packages to produce tables affected by this issue. If this message doesn't help, please report your use case to the data.table issue tracker so the root cause can be fixed or this message improved.
#> Warning: Tried to assign NULL to column 'YEAR', but this column does not exist to remove
#> Warning: Tried to assign NULL to column 'MM', but this column does not exist to remove
#> Warning: Tried to assign NULL to column 'DD', but this column does not exist to remove
#>        YYYYMMDD   DOY  TEMP  TMIN  TMAX  RHUM  RAIN     LAT      LON
#>          <Date> <int> <num> <num> <num> <num> <num>   <num>    <num>
#>   1: 2000-06-30   182 26.02 23.34 28.76 88.53 11.01 14.6774 121.2556
#>   2: 2000-07-01   183 25.29 23.86 27.78 92.20 23.12 14.6774 121.2556
#>   3: 2000-07-02   184 26.13 23.54 29.90 86.01 17.34 14.6774 121.2556
#>   4: 2000-07-03   185 25.50 24.28 27.23 94.16 29.08 14.6774 121.2556
#>   5: 2000-07-04   186 25.81 24.50 27.56 92.42 13.01 14.6774 121.2556
#>  ---                                                                
#> 118: 2000-10-25   299 25.82 23.44 29.54 89.76 12.04 14.6774 121.2556
#> 119: 2000-10-26   300 25.44 24.14 26.99 94.93 13.03 14.6774 121.2556
#> 120: 2000-10-27   301 25.74 24.54 27.69 91.43 11.54 14.6774 121.2556
#> 121: 2000-10-28   302 25.44 24.72 26.62 91.90 74.20 14.6774 121.2556
#> 122: 2000-10-29   303 24.97 24.15 26.34 94.15 29.11 14.6774 121.2556
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
#> Error in setnames(wth, old = c("T2M", "T2M_MIN", "T2M_MAX", "PRECTOTCORR",     "RH2M"), new = c("TEMP", "TMIN", "TMAX", "RAIN", "RHUM")): could not find function "setnames"
.check_na(wth)
#> Error in .check_na(wth): could not find function ".check_na"
```
