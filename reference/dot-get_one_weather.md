# Get Weather Data for One Task

Calls the API to fetch weather data for a single (location, start,
duration) task and splits the result by calendar year into a named list.

## Usage

``` r
.get_one_weather(task, source = "power")
```

## Arguments

- task:

  A task object (from
  [`.create_single_task()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-create_single_task.md))
  with:

  - `lonlat`: numeric vector c(lon, lat)

  - `start_chr`: character start date

  - `duration`: integer duration or `"era5"`.'

## Value

An `epicrop.wth.list` object: a named list of weather tables, one per
year. Names are year strings (*e.g.*, "2000", "2001").

## Details

This function calls
[`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md)
to fetch data, then splits by year. It is called internally by
[`get_epicrop_weather_list()`](https://adamhsparks.codeberg.page/epicrop/reference/get_epicrop_weather_list.md).

## Return Structure

The weather table is split by `data.table::year(YYYYMMDD)` into a list.
Each list element is a table for one calendar year, named by the year.
The result is classed as `epicrop.wth.list`.
