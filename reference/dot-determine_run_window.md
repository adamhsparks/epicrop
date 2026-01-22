# Determine simulation window from weather data

Extracts the subset of weather data needed for the simulation based on
the emergence date and simulation duration. Validates that sufficient
weather data is available for the requested time window.

## Usage

``` r
.determine_run_window(wth, emergence, duration)
```

## Arguments

- wth:

  A data.table containing weather data with daily time-step. Must have a
  column `YYYYMMDD` of class `Date` containing the date for each
  observation. This is typically an object of class `epicrop.wth`
  created by
  [`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md)
  or
  [`format_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/format_wth.md).

- emergence:

  Character string or Date object specifying the crop emergence date in
  `YYYY-MM-DD` format. If a vector is provided, only the first element
  is used (with a warning).

- duration:

  Integer specifying the number of days to simulate from the emergence
  date. Must be a positive integer, typically \>= 50 days for meaningful
  disease progression.

## Value

A list with two elements:

- wth_subset:

  A data.table subset of the input weather data containing exactly
  `duration` rows, starting from the emergence date. This is the weather
  data that will be used for the simulation.

- run_dates:

  A Date vector of length `duration` containing the sequence of dates
  for the simulation window, starting from the emergence date.

## Details

This function performs several operations:

1.  Converts the emergence date to Date class using
    [`.check_date()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-check_date.md)

2.  Generates a sequence of dates from emergence to emergence +
    duration - 1

3.  Ensures the weather data's YYYYMMDD column is Date class for
    matching

4.  Extracts rows from the weather data that match the simulation dates

5.  Validates that the extracted data has exactly `duration` rows

If multiple emergence dates are provided, only the first is used and a
warning is issued.

If insufficient weather data is available (the extracted subset has
fewer than `duration` rows), an informative error is thrown showing:

- The requested number of days

- The actual number of days available

- The requested date window

- The available date range in the weather data

## Side Effects

- Prints an informative message showing the emergence date

- May modify the input `wth` data.table in place to convert YYYYMMDD to
  Date class if it isn't already

## See also

- [`.check_date()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-check_date.md)
  for date parsing and validation

- [`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md)
  for fetching weather data from NASA POWER

- [`format_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/format_wth.md)
  for formatting user-supplied weather data

- [`epicrop_sim()`](https://adamhsparks.codeberg.page/epicrop/reference/epicrop_sim.md)
  which calls this function internally

## Author

Adam H. Sparks, <adamhsparks@gmail.com>

## Examples

``` r
if (FALSE) { # \dontrun{
# This is an internal function not meant to be called directly
wth <- get_wth(
  lonlat = c(121.25562, 14.6774),
  dates = c("2000-06-30", "2000-12-31")
)

# Extract 120-day window starting July 1
run_info <- epicrop:::.determine_run_window(
  wth = wth,
  emergence = "2000-07-01",
  duration = 120
)

# Check the results
nrow(run_info$wth_subset)  # 120
length(run_info$run_dates)  # 120
range(run_info$run_dates)  # 2000-07-01 to 2000-10-28
} # }
```
