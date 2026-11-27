# Format Results for Return

Prepares final results in the appropriate class and structure.

## Usage

``` r
.format_results(results, tasks)
```

## Arguments

- results:

  A list of `epicrop.wth.list` objects (from
  [`.get_one_weather()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-get_one_weather.md)).

- tasks:

  A list of task objects (from
  [`.collapse_tasks_by_location()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-collapse_tasks_by_location.md)).

## Value

Either an `epicrop.wth.list` or an `epicrop.wth.bundle` object.

## Output Structure

- If only one (location, start, duration) combination: returns the
  `epicrop.wth.list` directly.

- If multiple combinations: returns an `epicrop.wth.bundle` (a named
  list of `epicrop.wth.list`) with keys like
  `"location01__2000-01-01__365d"`.
