# Create an AUDPC data frame for locations and emergence dates

Create an AUDPC data frame for locations and emergence dates

## Usage

``` r
audpc_dt(dt)
```

## Arguments

- dt:

  A `data.table` created by
  [`run_epicrop_model()`](https://adamhsparks.codeberg.page/epicrop/reference/run_epicrop_model.md).

## Value

A
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
suitable for mapping or other analysis. If `dt` is from
[`run_epicrop_model()`](https://adamhsparks.codeberg.page/epicrop/reference/run_epicrop_model.md)
directly, the output will be a `data.table` with five columns: `lat`,
`lon`, `AUDPC`, `location`, and `emergence`. If `dt` is from any of the
helper functions or directly from
[`epicrop_sim()`](https://adamhsparks.codeberg.page/epicrop/reference/epicrop_sim.md),
the output will be a `data.table` with two (or) four columns: `lat`*,
`lon`*, `AUDPC` and `emergence`. \*`lat` and `lon` columns must be in
the original weather dataset for them to appear.
