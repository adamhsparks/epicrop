# Extract AUDPC Summary as data.table

Extracts Area Under Disease Progress Curve (AUDPC) values along with
location and emergence date information from an epicrop.sim object.

## Usage

``` r
audpc_dt(x)
```

## Arguments

- x:

  An `epicrop.sim` object from
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
  or disease model functions.

## Value

A
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with columns:

- `lat`: Latitude (if available)

- `lon`: Longitude (if available)

- `AUDPC`: Area under disease progress curve

- `emergence`: Emergence date (as character)
