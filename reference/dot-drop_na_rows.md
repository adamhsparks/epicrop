# Remove NA Rows from Lookup Table

Filters out rows containing NA in either column. Warns if any NA rows
are found.

## Usage

``` r
.drop_na_rows(param)
```

## Arguments

- param:

  Numeric 2-column matrix.

## Value

Matrix with rows containing any NA removed.

## Details

Aborts if all rows are NA (no valid data remain).
