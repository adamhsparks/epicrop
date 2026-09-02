# Validate Required Arguments for Weather Fetching

Checks that `lonlat`, `start_date`, and `duration` are provided and
aborts with helpful error messages if any are missing. Called early in
[`fetch_epicrop_weather_list()`](https://adamhsparks.codeberg.page/epicrop/reference/fetch_epicrop_weather_list.md)
to fail fast.

## Usage

``` r
.validate_required_args(lonlat, start_date, duration)
```

## Arguments

- lonlat:

  Numeric coordinates or matrix of coordinates (required).

- start_date:

  A date string or vector of dates (required).

- duration:

  Integer or vector of integers specifying days (required).

## Value

NULL (invisibly). Called for side effects (validation/abort).

## Examples

``` r
# This would trigger validation (not normally called directly)
.validate_required_args(
  c(121.255669, 14.16742),
  "2000-06-30",
  120L
)
#> Error in .validate_required_args(c(121.255669, 14.16742), "2000-06-30",     120L): could not find function ".validate_required_args"
```
