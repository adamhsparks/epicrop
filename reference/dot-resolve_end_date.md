# Resolve End Date from Duration or Explicit End Date

Determines the end date for weather retrieval using either:

1.  A `duration` (inclusive days from `start`), or

2.  An explicit end date (from the `dates` vector)

## Usage

``` r
.resolve_end_date(dates, start, duration)
```

## Arguments

- dates:

  Character vector of dates from user input.

- start:

  Date start date (already parsed as Date class).

- duration:

  Integer duration (days) or NULL.

## Value

Date object representing the end date (inclusive).

## Details

Duration is inclusive: `end = start + duration - 1`, so the window spans
exactly `duration` days.

Example: start = 2000-06-30, duration = 3 gives end = 2000-07-02 (June
30, July 1, July 2 = 3 days total)

## Logic

Priority order:

1.  If `duration` is provided (not NULL), use `start + duration - 1`

2.  Else if `dates` has length \>= 2, use `dates[2]` as end

3.  Else abort (neither duration nor end date provided)

## Examples

``` r
start <- as.Date("2000-06-30")

# Using duration = 3
end1 <- .resolve_end_date(c("2000-06-30"), start, duration = 3L)
end1 # 2000-07-02
#> [1] "2000-07-02"
# Days: 06-30, 07-01, 07-02 (3 days)

# Using explicit end date
end2 <- .resolve_end_date(c("2000-06-30", "2000-12-31"), start, NULL)
end2 # 2000-12-31
#> [1] "2000-12-31"
```
