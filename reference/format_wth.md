# Format Weather Data for Use in epicrop_sim() Simulations

Formats an existing
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) of weather data
for use in epicrop. Only the data necessary for use in the model is
returned and the columns are renamed to values expected by the model.
The data are assumed to be on a daily time-step. For converting
sub-daily weather data to daily, see
[`cropgrowdays::daily_mean()`](https://rdrr.io/pkg/cropgrowdays/man/daily_mean.html).

## Usage

``` r
format_wth(wth, date, temp, rhum, rain, tmin, tmax, lat, lon)
```

## Arguments

- wth:

  A [`data.frame()`](https://rdrr.io/r/base/data.frame.html) object
  containing the weather data for formatting.

- date:

  The column name indicating the field containing the date values.

- temp:

  The column name indicating the field containing the average daily
  temperature values.

- rhum:

  The column name indicating the field containing the average daily
  relative humidity.

- rain:

  The column name indicating the field containing the total daily
  rainfall.

- tmin:

  The column name indicating the field containing the minimum daily
  temperature (optional).

- tmax:

  The column name indicating the field containing the maximum daily
  temperature.

- lat:

  The column name indicating the field containing the latitude of the
  observation data (optional).

- lon:

  The column name indicating the field containing the longitude of the
  observation data (optional).

## Value

An `epicrop.wth` object, a data frame compatible with a
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html),
suitable for use in epicrop.

## Details

### Required values

Weather data must be a daily time-step and contain the following values,

- full date including year, month and day,

- average daily temperature,

- average daily relative humidity,

- total daily rainfall

### Optional values

The following values are optional and the model can be run by default
without them.

- average minimum daily temperature (only used if
  `simple_wetness == FALSE`)

- average maximum daily temperature (only used if
  `simple_wetness == FALSE`)

- Latitude (only used if `simple_wetness == FALSE` and for mapping
  outputs)

- Longitude (only used for mapping outputs)

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
# Get weather data from the POWER API, using the `get_wth()` function,
# this is for example purposes only as it is NOT necessary to reformat
# the data provided by `get_wth()`.

library(nasapower)
w <- get_power(
  lonlat = c(121.25562, 14.6674),
  dates = c("2000-06-30", "2000-12-31"),
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

format_wth(
  wth = w,
  date = "YYYYMMDD",
  temp = "T2M",
  rhum = "RH2M",
  rain = "PRECTOTCORR",
  tmin = "T2M_MIN",
  tmax = "T2M_MAX",
  lat = "LAT",
  lon = "LON"
)
#>        YYYYMMDD   DOY  TEMP  TMIN  TMAX  RHUM  RAIN     LAT      LON
#>          <Date> <int> <num> <num> <num> <num> <num>   <num>    <num>
#>   1: 2000-06-30   181 26.02 23.34 28.76 88.53 11.01 14.6674 121.2556
#>   2: 2000-07-01   182 25.29 23.86 27.78 92.20 23.12 14.6674 121.2556
#>   3: 2000-07-02   183 26.13 23.54 29.90 86.01 17.34 14.6674 121.2556
#>   4: 2000-07-03   184 25.50 24.28 27.23 94.16 29.08 14.6674 121.2556
#>   5: 2000-07-04   185 25.81 24.50 27.56 92.42 13.01 14.6674 121.2556
#>  ---                                                                
#> 181: 2000-12-27   361 24.46 22.90 26.28 92.49 21.15 14.6674 121.2556
#> 182: 2000-12-28   362 24.64 23.32 27.28 91.92  6.01 14.6674 121.2556
#> 183: 2000-12-29   363 24.58 22.51 27.90 90.79  5.49 14.6674 121.2556
#> 184: 2000-12-30   364 25.31 22.84 28.84 86.25  2.07 14.6674 121.2556
#> 185: 2000-12-31   365 24.47 21.63 28.63 87.83  3.45 14.6674 121.2556
```
