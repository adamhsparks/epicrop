# Process Location Numeric Vector Input

Validates and reshapes a numeric vector of coordinate pairs into a
matrix. Vector must have even length (lon1, lat1, lon2, lat2, ...).

## Usage

``` r
.process_location_vector(ll)
```

## Arguments

- ll:

  A numeric vector of even length with finite values.

## Value

A 2-column matrix with one coordinate pair per row.

## Examples

``` r
# Single location
.process_location_vector(c(121.255669, 14.16742))
#>          [,1]     [,2]
#> [1,] 121.2557 14.16742

# Multiple locations
.process_location_vector(c(121.255669, 14.16742, 120.985, 14.6042))
#>          [,1]     [,2]
#> [1,] 121.2557 14.16742
#> [2,] 120.9850 14.60420
```
