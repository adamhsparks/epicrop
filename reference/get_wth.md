# Get Weather Data for Disease Modelling

Retrieves daily weather data from the NASA POWER database and formats it
for use in
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
and epicrop disease models. This function wraps
[`nasapower::get_power()`](https://docs.ropensci.org/nasapower/reference/get_power.html)
with predefined parameters optimized for epidemiological modelling.

## Usage

``` r
get_wth(lonlat, dates, duration = NULL)
```

## Arguments

- lonlat:

  Numeric vector of coordinates as `c(longitude, latitude)`. Longitude:
  -180 to 180 degrees (West/East) Latitude: -90 to 90 degrees
  (South/North) Example: `c(121.255669, 14.167425)` for IRRI,
  Philippines

- dates:

  Character vector of start date and optional end date, or character
  scalar start date. Format: `"YYYY-MM-DD"` (ISO 8601).

  - Length 1: Use with `duration` parameter

  - Length 2+: First is start, second is end; `duration` parameter
    ignored

- duration:

  Optional integer. Number of days to retrieve (inclusive). Only used if
  `dates` is a single start date. Ignored if `dates` has length \>= 2.
  Must be positive (\> 0). If not provided and `dates` has only one
  element, an error is raised.

## Value

An `epicrop.wth` object
([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
subclass) containing daily weather data with columns:

|            |                              |                       |
|------------|------------------------------|-----------------------|
| **Column** | **Description**              | **Units**             |
| YYYYMMDD   | Date                         | ISO 8601 (YYYY-MM-DD) |
| DOY        | Day of year                  | Integer (1-365/366)   |
| TEMP       | Mean daily temperature       | Degrees C             |
| TMIN       | Minimum daily temperature    | Degrees C             |
| TMAX       | Maximum daily temperature    | Degrees C             |
| RHUM       | Mean daily relative humidity | Percent (0-100)       |
| RAIN       | Total daily rainfall         | Millimeters           |
| LAT        | Latitude of query location   | Degrees (-90 to 90)   |
| LON        | Longitude of query location  | Degrees (-180 to 180) |

Ready for direct use in
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md),
[`bacterial_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
[`leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
and other disease models.

## Data Source

Weather data are obtained from NASA's Prediction of Worldwide Energy
Resource (POWER) database, which provides global daily weather data from
1984 onwards at 0.5-degree spatial resolution. See
[`nasapower::get_power()`](https://docs.ropensci.org/nasapower/reference/get_power.html)
for details.

POWER data are derived from satellite and reanalysis sources and may
differ from local weather station observations. For critical analyses,
consider comparing with ground-truth data from your region.

## Retrieved Variables

This function automatically retrieves:

- **T2M**: Mean daily temperature (2-meter height)

- **T2M_MIN**: Daily minimum temperature

- **T2M_MAX**: Daily maximum temperature

- **RH2M**: Mean daily relative humidity (2-meter height)

- **PRECTOTCORR**: Daily precipitation (bias-corrected)

All variables are returned in standard units: temperature in degrees C,
humidity in %, rainfall in mm.

## Date Handling

Two methods for specifying the weather window:

1.  **Start and end dates** (character vector):
    `dates = c("2000-06-30", "2000-12-31")`

2.  **Start date + duration** (integer):
    `dates = "2000-06-30", duration = 120`

Duration is interpreted as inclusive days: `end = start + duration - 1`.
For example, `duration = 120` starting on June 30 ends on October 27
(120 days total).

## References

NASA POWER project (2019). Agroclimatology, Prediction Of Worldwide
Energy Resource. Funded by NASA's Applied Science Program.
https://power.larc.nasa.gov/

Sparks, A. H., P. D. Esker, M. Bates, W. Dall'Acqua, Z. Guo, V. Segovia,
S. D. Silwal, S. Tolos, and K. A. Garrett. 2008. Ecology and
Epidemiology in R: Disease Progress over Time. *The Plant Health
Instructor*.
[doi:10.1094/PHI-A-2008-0129-02](https://doi.org/10.1094/PHI-A-2008-0129-02)
.

## Author

Adam H. Sparks, <adamhsparks@gmail.com>

## Examples

``` r
# Example 1: Get weather for a date range
# IRRI Zeigler Experiment Station, wet season 2000
wth_range <- get_wth(
  lonlat = c(121.255669, 14.167425),
  dates = c("2000-06-30", "2000-12-31")
)
head(wth_range)
#> Key: <YYYYMMDD>
#>      YYYYMMDD   DOY  TEMP  TMIN  TMAX  RHUM  RAIN      LAT      LON
#>        <IDat> <int> <num> <num> <num> <num> <num>    <num>    <num>
#> 1: 2000-06-30   182 26.60 23.94 29.89 85.99 11.94 14.16742 121.2557
#> 2: 2000-07-01   183 25.50 24.26 27.39 90.80 22.45 14.16742 121.2557
#> 3: 2000-07-02   184 26.45 24.07 30.09 85.74 14.87 14.16742 121.2557
#> 4: 2000-07-03   185 25.88 24.82 27.53 93.76 19.77 14.16742 121.2557
#> 5: 2000-07-04   186 26.12 24.97 28.08 91.53 12.15 14.16742 121.2557
#> 6: 2000-07-05   187 26.28 25.51 27.70 91.45 21.90 14.16742 121.2557
nrow(wth_range)  # Number of days
#> [1] 185

# Example 2: Get weather by start date and duration
# 120-day window from June 30, 2000
wth_duration <- get_wth(
  lonlat = c(121.255669, 14.167425),
  dates = "2000-06-30",
  duration = 120L
)
nrow(wth_duration)  # Should be 120 rows
#> [1] 121

# Example 3: Verify both methods return equivalent data
# (within floating-point precision)
all.equal(wth_range[1:120], wth_duration)
#> [1] "Different number of rows"

# Example 4: Use retrieved weather in disease model
wth <- get_wth(
  lonlat = c(121.255669, 14.167425),
  dates = "2000-06-30",
  duration = 120L
)

# Run bacterial blight model
bb_sim <- bacterial_blight(wth, emergence = "2000-07-01")
#> Error in epi_get_params("rice", "bacterial_blight"): could not find function "epi_get_params"
head(bb_sim)
#>    simday      dates    sites latent infectious removed senesced rateinf  rlex
#>     <int>     <IDat>    <num>  <num>      <num>   <num>    <num>   <num> <num>
#> 1:      1 2000-07-01 100.0000      0          0       0 1.000000       0     0
#> 2:      2 2000-07-02 108.6875      0          0       0 2.086875       0     0
#> 3:      3 2000-07-03 118.1002      0          0       0 3.267877       0     0
#> 4:      4 2000-07-04 128.2934      0          0       0 4.550811       0     0
#> 5:      5 2000-07-05 139.3254      0          0       0 5.944065       0     0
#> 6:      6 2000-07-06 151.2581      0          0       0 7.456646       0     0
#>    rtransfer rremoved  rgrowth rsenesced diseased intensity     lat      lon
#>        <num>    <num>    <num>     <num>    <num>     <num>   <num>    <num>
#> 1:         0        0  9.68750  1.000000        0         0 14.6774 121.2556
#> 2:         0        0 10.49959  1.086875        0         0 14.6774 121.2556
#> 3:         0        0 11.37416  1.181002        0         0 14.6774 121.2556
#> 4:         0        0 12.31499  1.282934        0         0 14.6774 121.2556
#> 5:         0        0 13.32593  1.393254        0         0 14.6774 121.2556
#> 6:         0        0 14.41084  1.512581        0         0 14.6774 121.2556
plot(bb_sim$simday, bb_sim$intensity, type = "l",
     xlab = "Days After Emergence", ylab = "Disease Intensity")


# Example 5: Multiple locations - compare disease risk
locations <- data.frame(
  name = c("IRRI_Laguna", "BRRI_Gazipur", "CIMMYT_Mexico"),
  lon = c(121.255669, 90.2167, -99.1828),
  lat = c(14.167425, 24.0028, 19.1296)
)

# Get weather for each location
wth_list <- lapply(1:nrow(locations), function(i) {
  get_wth(
    lonlat = c(locations$lon[i], locations$lat[i]),
    dates = "2000-06-30",
    duration = 120L
  )
})
names(wth_list) <- locations$name

# Run leaf blast model at each location
lb_results <- lapply(wth_list, function(w) {
  leaf_blast(w, emergence = "2000-07-01")
})
#> Error in epi_get_params("rice", "leaf_blast"): could not find function "epi_get_params"

# Compare AUDPC (disease intensity summary)
audpc_comparison <- data.frame(
  location = names(lb_results),
  AUDPC = sapply(lb_results, function(x) attr(x, "AUDPC"))
)
#> Error: object 'lb_results' not found
print(audpc_comparison)
#> Error: object 'audpc_comparison' not found

# Example 6: Cross-year comparison (same location, different years)
years <- 2000:2002
wth_by_year <- lapply(years, function(y) {
  get_wth(
    lonlat = c(121.255669, 14.167425),
    dates = paste0(y, "-06-30"),
    duration = 120L
  )
})
names(wth_by_year) <- as.character(years)

# Run model for each year
bb_by_year <- lapply(wth_by_year, function(w) {
  bacterial_blight(w, emergence = unique(w$YYYYMMDD)[1])
})
#> Error in epi_get_params("rice", "bacterial_blight"): could not find function "epi_get_params"

# Compare disease progression across years
plot_data <- data.table::rbindlist(
  lapply(seq_along(bb_by_year), function(i) {
    data.frame(
      year = names(bb_by_year)[i],
      simday = bb_by_year[[i]]$simday,
      intensity = bb_by_year[[i]]$intensity
    )
  })
)
#> Error: object 'bb_by_year' not found
plot(plot_data$simday, plot_data$intensity,
     col = as.factor(plot_data$year), pch = 16)
#> Error: object 'plot_data' not found
legend("topleft", legend = names(bb_by_year),
       col = seq_along(bb_by_year), pch = 16)
#> Error: object 'bb_by_year' not found

# Example 7: Error handling
if (FALSE) { # \dontrun{
# This will fail (invalid coordinates)
get_wth(
  lonlat = c(200, 95),  # Out of range
  dates = "2000-06-30",
  duration = 120
)
# Error: Longitude must be in [-180, 180], got 200

# This will fail (invalid date format)
get_wth(
  lonlat = c(121.255669, 14.167425),
  dates = "30-06-2000",  # Wrong format
  duration = 120
)
# Error: dates[1] must be an ISO date ('YYYY-MM-DD')

# This will fail (missing date specification)
get_wth(
  lonlat = c(121.255669, 14.167425),
  dates = "2000-06-30"  # No end date or duration
)
# Error: Provide dates as c(start, end) or supply duration
} # }
```
