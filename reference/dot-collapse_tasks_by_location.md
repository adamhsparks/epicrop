# Collapse Tasks by Location for Efficient API Calls

Groups tasks by location and collapses each group into a single task.
This minimizes the number of API calls required to fetch all data.

## Usage

``` r
.collapse_tasks_by_location(tasks)
```

## Arguments

- tasks:

  A list of task objects (from
  [`.build_tasks()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-build_tasks.md)).

## Value

A list of collapsed tasks, one per unique location.

## Details

By combining all date ranges for a single location, we can fetch the
entire date span in one API call, then extract individual results later.
This is especially efficient when fetching multiple years or multiple
start dates for the same location.
