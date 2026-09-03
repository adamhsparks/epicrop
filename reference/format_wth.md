# Format Weather Data for Use in seir() Simulations

Reformats an existing
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) of weather data
into the standard epicrop format for use in
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
and disease model functions. Selects and renames columns to match model
expectations, validates data quality (no gaps, no duplicates), and adds
derived fields (DOY).

## Usage

``` r
format_wth(wth, date, temp, rhum, rain, tmin, tmax, lat, lon)
```

## Arguments

- wth:

  A [`data.frame()`](https://rdrr.io/r/base/data.frame.html) or
  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
  containing raw weather observations. Must have one row per day with no
  gaps.

- date:

  Character. Name of column in `wth` containing dates. Dates should be
  in ISO format (YYYY-MM-DD), integer (YYYYMMDD), or already Date class.
  Required.

- temp:

  Character. Name of column containing mean daily temperature (degrees
  C). Required.

- rhum:

  Character. Name of column containing mean daily relative humidity (%).
  Required.

- rain:

  Character. Name of column containing total daily rainfall (mm).
  Required.

- tmin:

  Character. Name of column containing minimum daily temperature
  (degrees C). Optional; required only if using `simple_wetness = FALSE`
  in
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md).

- tmax:

  Character. Name of column containing maximum daily temperature
  (degrees C). Optional; required only if using `simple_wetness = FALSE`
  in
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md).

- lat:

  Character. Name of column containing latitude (degrees). Optional;
  improves hourly wetness calculation and enables mapping.

- lon:

  Character. Name of column containing longitude (degrees). Optional;
  enables spatial mapping of results.

## Value

An `epicrop.wth` object
([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
subclass) containing reformatted weather data. Compatible with
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
and all disease model functions
([`bacterial_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
[`leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
etc.). Automatically includes DOY (day of year) derived from date.

## Details

### Data Source Examples

Common data sources and required column names:

- **NASA POWER** (via
  [`nasapower::get_power()`](https://docs.ropensci.org/nasapower/reference/get_power.html)):
  Use `date = "YYYYMMDD"`, `temp = "T2M"`, `rhum = "RH2M"`,
  `rain = "PRECTOTCORR"`, etc.

- **CHIRPS rainfall** (via
  [`chirps::get_chirps()`](https://docs.ropensci.org/chirps/reference/get_chirps.html)):
  Combine with temperature data from another source

- **Local weather station data**: Map station file columns to parameter
  names

### Caveats

- Sub-daily weather data must be aggregated to daily first. See
  [`cropgrowdays::daily_mean()`](https://rdrr.io/pkg/cropgrowdays/man/daily_mean.html)
  for hourly data aggregation.

- Dates must be complete and continuous (no missing days). Interpolation
  is not performed.

- Temperature, RH, and rainfall must be numeric (integers or floats).
  Character/factor columns cause errors.

- For `simple_wetness = FALSE` in
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md),
  both TMIN and TMAX are required.

## Input Requirements

Weather data must contain **daily observations** (one row per day) with
these required columns (names arbitrary):

- **Date** column with full date (year, month, day)

- **Temperature** column with mean daily temperature (degrees C)

- **Relative humidity** column with mean daily RH (%)

- **Rainfall** column with total daily precipitation (mm)

Optional columns that enhance model functionality:

- **Minimum temperature** (degrees C; required if
  `simple_wetness = FALSE`)

- **Maximum temperature** (degrees C; required if
  `simple_wetness = FALSE`)

- **Latitude** (degrees; used for hourly wetness and mapping)

- **Longitude** (degrees; used for mapping and spatial analysis)

## Data Validation

This function validates that:

1.  All required columns exist in the input data.frame

2.  No duplicate dates are present

3.  Dates form a contiguous sequence with no gaps (true daily data)

4.  Date column is properly formatted as Date class

If validation fails, detailed error messages pinpoint the issue.

## Output Format

Returns a
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with class `epicrop.wth` containing the following columns (in order):

|            |                                                    |
|------------|----------------------------------------------------|
| **Column** | **Description**                                    |
| YYYYMMDD   | Date (ISO 8601 format, Date class)                 |
| DOY        | Day of year (1-365/366, integer)                   |
| TEMP       | Mean daily temperature (degrees C)                 |
| TMIN       | Minimum daily temperature (degrees C) \[optional\] |
| TMAX       | Maximum daily temperature (degrees C) \[optional\] |
| RHUM       | Mean daily relative humidity (%)                   |
| RAIN       | Total daily rainfall (mm)                          |
| LAT        | Latitude \[optional\]                              |
| LON        | Longitude \[optional\]                             |

## Author

Adam H. Sparks, <adamhsparks@gmail.com>

## Examples

``` r
# Example 1: Format weather from NASA POWER API
# (Requires nasapower package; this example shows typical usage)

# Simulated POWER data (in practice from get_power())
wth_raw <- data.frame(
  DATE = seq(as.Date("2000-06-30"), as.Date("2000-12-31"), by = "day"),
  TAVG = rnorm(185, mean = 25, sd = 3),
  TMIN = rnorm(185, mean = 20, sd = 3),
  TMAX = rnorm(185, mean = 30, sd = 3),
  RH = rnorm(185, mean = 80, sd = 10),
  PRECIP = rgamma(185, shape = 2, rate = 0.5),
  LAT = 14.6774,
  LON = 121.25562
)

# Format for epicrop
wth <- format_wth(
  wth = wth_raw,
  date = "DATE",
  temp = "TAVG",
  tmin = "TMIN",
  tmax = "TMAX",
  rhum = "RH",
  rain = "PRECIP",
  lat = "LAT",
  lon = "LON"
)

# Check class and structure
class(wth)
#> [1] "epicrop.wth" "data.table"  "data.frame" 
head(wth)
#>      YYYYMMDD   DOY     TEMP     TMIN     TMAX     RHUM     RAIN     LAT
#>        <Date> <int>    <num>    <num>    <num>    <num>    <num>   <num>
#> 1: 2000-06-30   182 20.79987 25.97933 30.53644 71.94048 1.842904 14.6774
#> 2: 2000-07-01   183 25.76595 19.53764 27.90471 81.05665 2.102449 14.6774
#> 3: 2000-07-02   184 17.68821 27.69323 27.11865 76.66400 8.544918 14.6774
#> 4: 2000-07-03   185 24.98329 23.18600 27.07373 96.41848 1.002127 14.6774
#> 5: 2000-07-04   186 26.86466 23.42808 28.98427 73.56094 5.496244 14.6774
#> 6: 2000-07-05   187 28.44523 23.37152 33.45704 85.87021 1.620152 14.6774
#>         LON
#>       <num>
#> 1: 121.2556
#> 2: 121.2556
#> 3: 121.2556
#> 4: 121.2556
#> 5: 121.2556
#> 6: 121.2556
# Ready for use in disease models
sim <- bacterial_blight(wth, emergence = "2000-07-01")

# Example 2: Minimal data (no lat/lon, no TMIN/TMAX)
# Only requires temp, rhum, rain (simple_wetness = TRUE)
wth_minimal <- data.frame(
  obs_date = seq(as.Date("2000-06-30"), as.Date("2000-08-28"), by = "day"),
  mean_temp = rnorm(60, mean = 25, sd = 2),
  mean_rh = rnorm(60, mean = 85, sd = 5),
  daily_rain = rpois(60, lambda = 3)
)

wth_fmt <- format_wth(
  wth = wth_minimal,
  date = "obs_date",
  temp = "mean_temp",
  rhum = "mean_rh",
  rain = "daily_rain"
)

# Simulate with binary wetness (no hourly calculation)
sim <- leaf_blast(wth_fmt, emergence = "2000-07-01", simple_wetness = TRUE)
#> Error in seir(wth = wth, emergence = emergence, RcA = RcA %||% params$RcA,     RcT = RcT %||% params$RcT, RRG = RRG %||% params$RRG, RRS = RRS %||%         params$RRS, onset = 15L, duration = 120L, rhlim = rhlim %||%         90L, rainlim = rainlim %||% 5L, H0 = 600L, I0 = 1L, RcOpt = 1.14,     p = 5L, i = 20L, a = 1L, Sx = 30000L, simple_wetness = TRUE,     age_driver = "day", ...): formal argument "simple_wetness" matched by multiple actual arguments

# Example 3: Integer date format (YYYYMMDD)
wth_int_date <- data.frame(
  date_int = c(20000630L, 20000701L, 20000702L),
  temp = c(25, 26, 24),
  rhum = c(85, 80, 90),
  rain = c(5, 0, 8)
)

# Convert to character first (data.table will handle conversion)
wth_int_date$date_int <- as.character(wth_int_date$date_int)

wth_formatted <- format_wth(
  wth = wth_int_date,
  date = "date_int",
  temp = "temp",
  rhum = "rhum",
  rain = "rain"
)
#> Error in charToDate(x): character string is not in a standard unambiguous format

# Example 4: Subset and format existing epicrop.wth
# (e.g., use only part of a longer dataset)
wth_full <- get_wth(
  lonlat = c(121.25562, 14.6774),
  dates = c("2000-06-30", "2000-12-31")
)

# Subset to June-July only
wth_subset <- wth_full[month(YYYYMMDD) %in% 6:7]

# Re-validate formatting
wth_check <- format_wth(
  wth = wth_subset,
  date = "YYYYMMDD",
  temp = "TEMP",
  rhum = "RHUM",
  rain = "RAIN",
  tmin = "TMIN",
  tmax = "TMAX",
  lat = "LAT",
  lon = "LON"
)
#> Warning: `wth` is already an <epicrop.wth> object.
#> ℹ No formatting is necessary; returning as-is.

# Example 5: Error handling - demonstrating validation
# This will fail (missing column)
if (FALSE) { # \dontrun{
bad_wth <- data.frame(
  date = seq(as.Date("2000-06-30"), as.Date("2000-07-05"), by = "day"),
  temp = c(25, 26, 24, 25, 27, 26),
  rhum = c(85, 80, 90, 85, 75, 80)
  # Missing 'rain' column!
)

format_wth(
  wth = bad_wth,
  date = "date",
  temp = "temp",
  rhum = "rhum",
  rain = "rain"  # This column doesn't exist
)
# Error: The following columns are not in wth: rain
} # }

# This will fail (duplicate dates)
if (FALSE) { # \dontrun{
dup_wth <- data.frame(
  date = c(as.Date("2000-06-30"), as.Date("2000-06-30"), as.Date("2000-07-01")),
  temp = c(25, 26, 24),
  rhum = c(85, 80, 90),
  rain = c(5, 0, 8)
)

format_wth(
  wth = dup_wth,
  date = "date",
  temp = "temp",
  rhum = "rhum",
  rain = "rain"
)
# Error: Weather data contains duplicate dates
} # }

# This will fail (gaps in dates)
if (FALSE) { # \dontrun{
gap_wth <- data.frame(
  date = c(
    as.Date("2000-06-30"),
    as.Date("2000-07-01"),  # Skip July 1
    as.Date("2000-07-03")   # Gap of 2 days!
  ),
  temp = c(25, 24, 26),
  rhum = c(85, 90, 80),
  rain = c(5, 8, 0)
)

format_wth(
  wth = gap_wth,
  date = "date",
  temp = "temp",
  rhum = "rhum",
  rain = "rain"
)
# Error: Weather data is not on a daily time-step or has gaps
} # }
```
