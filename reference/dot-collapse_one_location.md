# Collapse Multiple Tasks for One Location

Merges multiple tasks (start_date/duration combinations) for a single
location into one consolidated task with the minimum start date and
maximum end date. This ensures efficient API calls.

## Usage

``` r
.collapse_one_location(tsks)
```

## Arguments

- tsks:

  A list of task objects for the same location.

## Value

A single task list with:

- `location_id`: unchanged

- `location_name`: unchanged

- `lonlat`: unchanged

- `start_chr`: earliest start date

- `duration`: days to cover latest end date

## Details

Used to combine multiple requested date ranges into one API fetch.
Reduces redundant requests for the same location.

## Examples

``` r
# Multiple tasks for same location across years
tasks <- list(
  list(
    location_id = 1,
    location_name = "Site1",
    lonlat = c(0, 0),
    start_chr = "2000-06-01",
    duration = 90L
  ),
  list(
    location_id = 1,
    location_name = "Site1",
    lonlat = c(0, 0),
    start_chr = "2001-06-01",
    duration = 90L
  )
)
.collapse_one_location(tasks)
#> Error in .collapse_one_location(tasks): could not find function ".collapse_one_location"
```
