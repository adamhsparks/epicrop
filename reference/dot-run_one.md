# Run Model for a Single Emergence Date

Internal workhorse that executes a disease model for one emergence date,
slices weather data as needed, and returns results (AUDPC or full).

## Usage

``` r
.run_one(ed, model_fun, wth_list, window_days, dots, output)
```

## Arguments

- ed:

  Emergence date (IDate).

- model_fun:

  Function. Disease model to run.

- wth_list:

  Named list of weather tables.

- window_days:

  Integer. Days to fetch.

- dots:

  List of arguments to forward to `model_fun` (from `...`).

- output:

  Character. `"audpc"` or `"full"`.

## Value

A
[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html):

- If `output = "audpc"`: one row with `emergence` and `AUDPC`

- If `output = "full"`: full model output (one row per day) plus
  `emergence` and `AUDPC` columns

- NULL if weather data are insufficient (full output only)

## Logic

1.  Slice weather via
    [`.slice_weather()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-slice_weather.md)

2.  If no weather found, return NA (AUDPC) or NULL (full)

3.  Otherwise, invoke model with sliced weather + forwarded arguments

4.  Extract AUDPC attribute and return in requested format
