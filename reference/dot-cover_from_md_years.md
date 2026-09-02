# Calculate Date Coverage Window from Month-Day and Years

Constructs a date range that covers all occurrences of a given month-day
across a range of years, plus the specified duration.

## Usage

``` r
.cover_from_md_years(md_mmdd, yrs, window_days)
```

## Arguments

- md_mmdd:

  Character month-day in `"MM-DD"` format.

- yrs:

  Integer vector of years.

- window_days:

  Integer duration (days) from each start date.

## Value

A list with:

- `start_chr`: character earliest start date in `"YYYY-MM-DD"` format

- `duration`: integer days needed to cover all years and duration

## Details

This is used to efficiently fetch weather in a single API call that
covers multiple year windows. For example, fetching "06-01 to 08-30" for
years 2000:2002 requires one call from 2000-06-01 to 2002-08-30.

## Logic

1.  Paste year with month-day string to create dates for each year

2.  Find earliest start date: `min(all_dates)`

3.  Find latest end date: `max(all_dates) + (duration - 1)`

4.  Return: `list(start_chr, duration_to_cover_window)`

## Examples

``` r
.cover_from_md_years("06-30", 2000:2001, 90L)
#> $start_chr
#> [1] "2000-06-30"
#> 
#> $duration
#> [1] 455
#> 
```
