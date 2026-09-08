# Slice Weather Data Across a Date Window

Extracts weather for a single emergence date across one or more calendar
years. Handles year boundaries (*e.g.*, Dec 15 → Jan 15).

## Usage

``` r
.slice_weather(ed, wth_list, window_days)
```

## Arguments

- ed:

  Emergence date (IDate).

- wth_list:

  Named list of weather tables (names = year strings).

- window_days:

  Integer. Days to fetch from emergence.

## Value

A
[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
with rows covering `window_days`, or NULL if weather is completely
unavailable.

## Details

Missing years or empty segments within a year produce warnings but do
not stop execution. If the final window spans fewer days than requested
(due to missing data), the shorter window is returned.

## Algorithm

1.  Compute start = `ed`, end = `ed + window_days - 1`

2.  For each year in range:

    - Fetch the year's weather table

    - Filter rows within \[max(start, year-01-01), min(end,
      year-12-31)\]

    - Warn if no data found for that year

3.  Combine segments and return contiguous window

## Examples

``` r
# Simulated weather lists (one year per element)
w_2000 <- data.table::data.table(
  YYYYMMDD = seq(
    as.Date("2000-06-01"),
    as.Date("2000-12-31"),
    by = "day"
  ),
  TEMP = 25
)
w_2001 <- data.table::data.table(
  YYYYMMDD = seq(
    as.Date("2001-01-01"),
    as.Date("2001-12-31"),
    by = "day"
  ),
  TEMP = 24
)
wth_list <- list("2000" = w_2000, "2001" = w_2001)

# Extract window spanning Dec 2000 – Jan 2001
ed <- as.Date("2000-12-15")
sliced <- .slice_weather(ed, wth_list, window_days = 35L)
nrow(sliced)  # Should be close to 35 if data available
#> [1] 35
```
