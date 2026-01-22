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
[`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
suitable for mapping or other analysis with five columns: `lat`, `lon`,
`AUDPC`, `location`, and `emergence`.
