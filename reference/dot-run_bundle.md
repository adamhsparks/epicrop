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

The function extracts location labels from the bundle's names (text
before `"__"` delimiter) and generates default labels (`location01`,
`location02`, etc.) for any unnamed or empty-named entries. Each bundled
weather list is processed independently via
[`run_epicrop_model()`](https://adamhsparks.codeberg.page/epicrop/reference/run_epicrop_model.md),
then results are combined using
[`data.table::rbindlist()`](https://rdrr.io/pkg/data.table/man/rbindlist.html).

## See also

[`run_epicrop_model()`](https://adamhsparks.codeberg.page/epicrop/reference/run_epicrop_model.md),
[`fetch_epicrop_weather_list()`](https://adamhsparks.codeberg.page/epicrop/reference/fetch_epicrop_weather_list.md)
