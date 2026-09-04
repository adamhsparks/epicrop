# Process Location Data Frame Input

Validates and converts a data frame to a location matrix. Ensures
exactly 2 numeric columns (longitude, latitude).

## Usage

``` r
.process_location_dataframe(ll)
```

## Arguments

- ll:

  A data frame with exactly 2 numeric columns.

## Value

A numeric matrix with row names (if any) preserved.

## Examples

``` r
locations <- data.frame(
  lon = c(121.255669, 120.985),
  lat = c(14.16742, 14.6042)
)
.process_location_dataframe(locations)
#> Error in .process_location_dataframe(locations): could not find function ".process_location_dataframe"
```
