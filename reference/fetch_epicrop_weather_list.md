# Fetch weather once (single or multiple locations) and split by calendar year

Retrieve one or more continuous blocks of weather for one or more
locations from given start dates and durations, then split each result
into a named list of per‑year tables. Each (location, start, duration)
result is an `epicrop.wth.list`. The top-level return is an
`epicrop.wth.bundle` when multiple requests are processed, otherwise a
single `epicrop.wth.list`.

## Usage

``` r
fetch_epicrop_weather_list(
  lonlat,
  start_date,
  duration,
  years = NULL,
  mode = c("pairwise", "cross"),
  location_names = NULL
)
```

## Arguments

- lonlat:

  Numeric length‑2 vector `c(lon, lat)`, OR multiple locations as:
  numeric vector of length `2n`, a 2‑column matrix/data.frame, or a list
  of numeric length‑2 vectors.

- start_date:

  A starting date (character `"YYYY-MM-DD"`, lubridate::Date, or
  [data.table::IDate](https://rdrr.io/pkg/data.table/man/IDateTime.html))
  **or** a month–day string like `"06-30"` (leading `"-"` optional) used
  together with `years`.

- duration:

  Integer(s). Number of days to retrieve **from** `start_date`
  (inclusive). Single value or vector (recycled).

- years:

  Optional integer vector of years. **Required** if `start_date`
  contains any month–day values without a year.

- mode:

  Character scalar: `"pairwise"` (default) pairs locations and dates by
  position (recycling length-1), or `"cross"` forms all combinations
  (locations × dates).

- location_names:

  Optional character names for locations (length = number of unique
  locations).

## Value

- If only one (location, start, duration) is requested: an
  `epicrop.wth.list`.

- Otherwise: an `epicrop.wth.bundle` (a named list of
  `epicrop.wth.list`).

## Details

### Flexible inputs

- `lonlat`: a single `c(lon, lat)`; or multiple locations as a numeric
  vector of length `2n`, or a 2‑column matrix/data.frame, or a list of
  `c(lon, lat)`.

- `start_date`: a single `"YYYY-MM-DD"` (or `Date`/`IDate`), or a vector
  of the same; or a month–day string like `"06-30"` or `"6-30"` (leading
  `"-"` optional) provided together with `years`.

- `duration`: single integer or a vector (recycled); interpreted as
  inclusive days to fetch (`end = start + duration - 1`).

- `years`: only used when `start_date` lacks a year. We compute a
  **cover window** per distinct month–day:
  `start = min(paste(years, md))`,
  `end = max(paste(years, md)) + (duration - 1)`.

## Examples

``` r
# Single location, month-day start date (any of "6/30", "-06-30", "06-30")
w1 <- fetch_epicrop_weather_list(
  lonlat     = c(121.255669, 14.16742),
  start_date = "6/30",
  duration   = 120L,
  years      = 2000:2001
)
#> Error in YYYYMMDD %between% c(s, e): could not find function "%between%"

# ISO start date
w2 <- fetch_epicrop_weather_list(
  lonlat     = c(121.255669, 14.16742),
  start_date = "2000-06-30",
  duration   = 120L
)
#> Error in YYYYMMDD %between% c(s, e): could not find function "%between%"

# Multiple locations × multiple dates (cross mode), mixed inputs
locations <- rbind(
  "IRRI_ZES" = c(121.255669, 14.16742),
  "Metro_Manila" = c(120.985, 14.6042)
)
w3 <- fetch_epicrop_weather_list(
  lonlat     = locations,
  start_date = c("2000-06-30", "6-30"),
  duration   = 120L,
  years      = 2000:2001,
  mode       = "cross"
)
#> Error in YYYYMMDD %between% c(s, e): could not find function "%between%"
```
