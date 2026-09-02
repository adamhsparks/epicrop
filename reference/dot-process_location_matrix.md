# Process Location Matrix Input

Validates a matrix of coordinate pairs. Checks for exactly 2 columns,
numeric storage, and finite coordinate values.

## Usage

``` r
.process_location_matrix(ll)
```

## Arguments

- ll:

  A numeric matrix with exactly 2 columns.

## Value

The validated matrix unchanged.

## Examples

``` r
locations <- rbind(
  c(121.255669, 14.16742),
  c(120.985, 14.6042)
)
.process_location_matrix(locations)
#> Error in .process_location_matrix(locations): could not find function ".process_location_matrix"
```
