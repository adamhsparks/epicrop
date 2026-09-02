# Resolve End Date from Duration or Explicit End Date

Determines the end date for weather retrieval using either:

1.  An explicit end date (from `dates` vector), or

2.  Duration calculation (start + duration - 1)

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

Date object representing the end date.

## Details

Duration is inclusive: `end = start + duration - 1` means the window
spans exactly `duration` days.

Example: start = 2000-06-30, duration = 120 gives end = 2000-10-27 (June
30 through October 27 = 120 days)

## Logic

Priority order:

1.  If `duration` is provided (not NULL), use start + duration - 1

2.  Else if `dates` has length \>= 2, use `dates[2]` as end

3.  Else abort (neither duration nor end date provided)

## Examples

``` r
start <- as.Date("2000-06-30")

# Using duration
end1 <- .resolve_end_date(c("2000-06-30"), start, duration = 120L)
#> Error in .resolve_end_date(c("2000-06-30"), start, duration = 120L): could not find function ".resolve_end_date"
print(end1)  # 2000-10-27
#> Error: object 'end1' not found

# Using explicit end date
end2 <- .resolve_end_date(c("2000-06-30", "2000-12-31"), start, NULL)
#> Error in .resolve_end_date(c("2000-06-30", "2000-12-31"), start, NULL): could not find function ".resolve_end_date"
print(end2)  # 2000-12-31
#> Error: object 'end2' not found
```
