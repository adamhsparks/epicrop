# Process Location List Input

Validates and converts a list of coordinate pairs to a matrix. Each
element must be a numeric vector of length 2 with finite values.

## Usage

``` r
.process_location_list(ll)
```

## Arguments

- ll:

  A list where each element is a numeric vector of length 2.

## Value

A numeric matrix with row names preserved if available.

## Details

Errors if any element is not a length-2 numeric vector with finite
values. Row names are preserved if the list is named and all names are
non-empty.

## Examples

``` r
.process_location_list(list(
  IRRI = c(121.255669, 14.16742),
  Manila = c(120.985, 14.6042)
))
#>            [,1]     [,2]
#> IRRI   121.2557 14.16742
#> Manila 120.9850 14.60420
```
