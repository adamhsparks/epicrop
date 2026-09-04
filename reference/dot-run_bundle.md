# Run epicrop Model Across a Weather Bundle

Internal helper that processes a weather bundle (multiple locations or
location-date combinations) by executing
[`run_epicrop_model()`](https://adamhsparks.codeberg.page/epicrop/reference/run_epicrop_model.md)
separately for each bundled weather list and combining results with
location labels.

## Usage

``` r
.run_bundle(model_fun, emergence_dates, wth_list, window_days, output, ...)
```

## Arguments

- model_fun:

  Function. An epicrop model function.

- emergence_dates:

  Vector of emergence dates (class `epicrop.emerge`).

- wth_list:

  An `epicrop.wth.bundle`; a named list of `epicrop.wth.list` objects.

- window_days:

  Integer. Days after emergence to include (default 120L).

- output:

  Character. `"audpc"` or `"full"`.

- ...:

  Additional arguments forwarded to `model_fun`.

## Value

A
[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
with results from all bundled locations combined row-wise, with an
additional `location` column identifying the source bundle member.

## Details

The function extracts location labels from the bundle's names by taking
the text before the `"__"` delimiter (used by
[`fetch_epicrop_weather_list()`](https://adamhsparks.codeberg.page/epicrop/reference/fetch_epicrop_weather_list.md)).
For unnamed or empty-named entries, generates default labels
(`location01`, `location02`, etc.).

Each bundled weather list is processed independently via
[`run_epicrop_model()`](https://adamhsparks.codeberg.page/epicrop/reference/run_epicrop_model.md),
then results are combined using
[`data.table::rbindlist()`](https://rdrr.io/pkg/data.table/man/rbindlist.html)
with automatic column alignment.

## Algorithm

1.  Extract bundle member names; generate labels for unnamed entries

2.  For each bundle member (location or location-date):

    - Call
      [`run_epicrop_model()`](https://adamhsparks.codeberg.page/epicrop/reference/run_epicrop_model.md)
      with the sub-list

    - Add a `location` column to the result

3.  Combine all results via
    [`data.table::rbindlist()`](https://rdrr.io/pkg/data.table/man/rbindlist.html)

## See also

[`run_epicrop_model()`](https://adamhsparks.codeberg.page/epicrop/reference/run_epicrop_model.md),
[`fetch_epicrop_weather_list()`](https://adamhsparks.codeberg.page/epicrop/reference/fetch_epicrop_weather_list.md)

## Examples

``` r
# Typically called indirectly via [run_epicrop_model()] when wth_list is
# an epicrop.wth.bundle, but can be called directly:

locations <- rbind(
  "Site1" = c(121.255669, 14.16742),
  "Site2" = c(121.3, 14.2)
)
emergence <- build_epicrop_emergence(2001:2003, "-06-30")
wth_bundle <- fetch_epicrop_weather_list(
  lonlat = locations,
  start_date = "-06-30",
  duration = 120L,
  years = 2001:2003,
  mode = "cross"
)

results <- .run_bundle(
  leaf_blast,
  emergence,
  wth_bundle,
  window_days = 120L,
  output = "audpc"
)
# Results now include a 'location' column
```
