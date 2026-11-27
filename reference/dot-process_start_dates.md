# Process Start Dates to Character Format

Converts start dates to character format and normalises month-day
strings. Month-day dates require the `years` parameter to construct
complete date ranges.

## Usage

``` r
.process_start_dates(start_date, years)
```

## Arguments

- start_date:

  Character, `Date`, or `IDate` vector of start dates.

- years:

  Optional integer vector of years. Required if `start_date` contains
  month-day values.

## Value

Character vector of start dates in `"YYYY-MM-DD"` or `"MM-DD"` format.

## Details

Month-day strings are normalised to `"MM-DD"` format. Full dates are
converted to character strings. If month-day values are found but
`years` is NULL, an error is raised.

## Examples

``` r
.process_start_dates(c("2000-06-30", "-06-30"), c(2000, 2001))
#> Error in .process_start_dates(c("2000-06-30", "-06-30"), c(2000, 2001)): could not find function ".process_start_dates"
```
