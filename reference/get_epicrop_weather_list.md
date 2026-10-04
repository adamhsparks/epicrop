# Get Weather for One or More Locations and Split by Calendar Year

Retrieves one or more continuous blocks of weather data for one or more
locations from given start dates and durations, then splits each result
into a named list of per-year tables. Each (location, start, duration)
result is an `epicrop.wth.list`. The top-level return is an
`epicrop.wth.bundle` when multiple requests are processed, otherwise a
single `epicrop.wth.list`.

## Usage

``` r
get_epicrop_weather_list(
  lonlat,
  start_date,
  duration,
  years = NULL,
  mode = "pairwise",
  location_names = NULL,
  source = c("power", "era5")
)
```

## Arguments

- lonlat:

  Numeric length-2 vector `c(lon, lat)`, OR multiple locations as:
  numeric vector of length `2n`, a 2-column matrix/data.frame, or a list
  of numeric length-2 vectors.

- start_date:

  A starting date (character `"YYYY-MM-DD"`,
  [Date](https://rdrr.io/r/base/Dates.html), or
  [data.table::IDate](https://rdrr.io/pkg/data.table/man/IDateTime.html))
  **or** a month-day string like `"06-30"` (leading `"-"` optional) used
  together with `years`.

- duration:

  Integer(s). Number of days to retrieve **from** `start_date`
  (inclusive). Single value or vector (recycled).

- years:

  Optional integer vector of years. **Required** if `start_date`
  contains any month-day values without a year.

- mode:

  Character scalar: `"pairwise"` (default) pairs locations and dates by
  position (recycling length-1), or `"cross"` forms all combinations
  (locations × dates).

- location_names:

  Optional character names for locations (length = number of unique
  locations).

- source:

  Character scalar: weather data source, `"power"` (default, NASA POWER)
  or `"era5"` (ERA5 reanalysis via Open-Meteo). See
  [`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md)
  for details on the differences between sources.

## Value

- If only one (location, start, duration) is requested: an
  `epicrop.wth.list`.

- Otherwise: an `epicrop.wth.bundle` (a named list of
  `epicrop.wth.list`).

## Flexible Input Formats

**Locations (`lonlat`):**

- Single numeric vector: `c(lon, lat)`

- Multiple locations as: numeric vector of length `2n`, a 2-column
  matrix/data.frame, or a list of `list(c(lon, lat))`

**Start dates (`start_date`):**

- Full ISO date: `"2000-06-30"` or `Date`/`IDate` object

- Month-day string: `"06-30"`, `"6-30"`, `"-06-30"` (with `years`
  parameter)

- Vector of dates (recycled in pairwise mode or crossed in cross mode)

**Duration:**

- Single integer or vector (recycled)

- Interpreted as inclusive days: `end = start + duration - 1`

**Years (optional):**

- Only used when `start_date` contains month-day values without a year

- Computes a **cover window** per distinct month-day:

  - `start = min(paste(years, md))`

  - `end = max(paste(years, md)) + (duration - 1)`.

## Author

Adam H. Sparks, <adamhsparks@gmail.com>

## Examples

``` r
# Single location, month-day start date
w1 <- get_epicrop_weather_list(
  lonlat = c(121.255669, 14.16742),
  start_date = "6/30",
  duration = 120L,
  years = 2000:2001
)

# ISO start date
w2 <- get_epicrop_weather_list(
  lonlat = c(121.255669, 14.16742),
  start_date = "2000-06-30",
  duration = 120L
)

# Multiple locations × multiple dates (cross mode)
locations <- rbind(
  "IRRI_ZES" = c(121.255669, 14.16742),
  "Metro_Manila" = c(120.985, 14.6042)
)
w3 <- get_epicrop_weather_list(
  lonlat = locations,
  start_date = c("2000-06-30", "6-30"),
  duration = 120L,
  years = 2000:2001,
  mode = "cross"
)

# Multiple start dates for same location
w4 <- get_epicrop_weather_list(
  lonlat = c(121.255669, 14.16742),
  start_date = c("-06-01", "-06-14", "-06-30"),
  duration = 90L,
  years = 2001:2003
)
# ERA5 instead of NASA POWER
w5 <- get_epicrop_weather_list(
  lonlat = c(121.255669, 14.16742),
  start_date = "2000-06-30",
  duration = 120L,
  source = "era5"
)
```
