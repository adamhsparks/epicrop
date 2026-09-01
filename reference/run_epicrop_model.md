# Run an epicrop disease model across emergence dates and/or locations

A unified helper that executes any epicrop model (*e.g.*,
[`bacterial_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
[`brown_spot()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
[`leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
[`sheath_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
[`tungro()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
[`modified_kim_leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
[`modified_kim_sheath_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
[`leaf_rust()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
or
[`s_tritici_blotch()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md))
over a set of emergence dates, using a list of per-year weather
`data.table`s. Handles within-year and cross-year windows without
globally binding weather.

## Usage

``` r
run_epicrop_model(
  model_fun,
  emergence_dates,
  wth_list,
  window_days = 120L,
  output = "audpc",
  ...
)
```

## Arguments

- model_fun:

  Function. An epicrop model function (*e.g.*,
  [`bacterial_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)).

- emergence_dates:

  Vector of dates (character "YYYY-MM-DD", Date, or IDate).

- wth_list:

  Named list of weather
  [data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html)s
  with one per year; `names(wth_list)` must be year strings (*e.g.*,
  "2001"). Each table must have a `YYYYMMDD` column
  (Date/IDate/ISO/"YYYYMMDD").

- window_days:

  Integer. Days after emergence to include (default 120L). The slicer
  returns exactly `window_days` rows when data are available.

- output:

  Character. "audpc" (compact summary using
  [`get_audpc()`](https://adamhsparks.codeberg.page/epicrop/reference/get_audpc.md))
  or "full" (return full model outputs per date, with `emergence`
  added). Defaults to "audpc".

- ...:

  Additional arguments forwarded to `model_fun`.

## Value

A
[data.table::data.table](https://rdrr.io/pkg/data.table/man/data.table.html).
When `output = "audpc"`: columns `emergence` (IDate) and `AUDPC`
(numeric). When `output = "full"`: full model output columns plus
`emergence`, and an `AUDPC` column preserved per emergence date.

## See also

fetch_epicrop_weather_list, build_epicrop_emergence

## Author

Adam H. Sparks, <adamhsparks@gmail.com>

## Examples

``` r
years <- 2001:2003
month_day <- c("-06-01", "-06-14", "-06-30")
lonlat <- c(121.255669, 14.16742)
duration <- 120L
emergence <- build_epicrop_emergence(years, month_day)

wth_list <- fetch_epicrop_weather_list(years,
                                       lonlat,
                                       start_date = month_day,
                                       duration = duration)
#> Error in .norm_locations(lonlat): `lonlat` numeric vector must have positive even length.
#> ✖ You provided length 3.
#> ℹ Expected pairs of (longitude, latitude).
run_epicrop_model(bacterial_blight, emergence, wth_list)
#> Error: object 'wth_list' not found
```
