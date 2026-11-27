# Resolve Date String and Calculate Duration

Converts date strings to a standard start date character and duration.
Handles both full ISO dates and month-day strings with year ranges.

## Usage

``` r
.resolve_date_and_duration(d_str, dur, years)
```

## Arguments

- d_str:

  Character start date (ISO format or month-day only).

- dur:

  Integer duration (days).

- years:

  Optional integer vector of years.

## Value

A list with:

- `start_chr`: character start date in `"YYYY-MM-DD"` format

- `duration`: integer duration (days)

## Details

If `d_str` is in `"MM-DD"` format, constructs a cover window using
`years`. Otherwise, parses as ISO date using
[`data.table::as.IDate()`](https://rdrr.io/pkg/data.table/man/IDateTime.html).
