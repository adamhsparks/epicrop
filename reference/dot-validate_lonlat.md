# Validate Longitude and Latitude Coordinates

Checks that coordinates are numeric, correct length, and within valid
ranges.

## Usage

``` r
.validate_lonlat(lonlat)
```

## Arguments

- lonlat:

  Numeric vector of c(longitude, latitude).

## Value

Numeric vector of c(longitude, latitude) as doubles.

## Details

Validates:

- Length is exactly 2

- Values are numeric (not character, factor, etc.)

- Longitude in \[-180, 180\] (East/West)

- Latitude in \[-90, 90\] (South/North)

- Neither value is NA or Inf

## Examples

``` r
# Valid coordinates
.validate_lonlat(c(121.255669, 14.167425)) # IRRI, Philippines
#> [1] 121.25567  14.16742
.validate_lonlat(c(-99.1828, 19.1296))     # CIMMYT, Mexico
#> [1] -99.1828  19.1296

# Invalid (will error)
if (FALSE) { # \dontrun{
.validate_lonlat(c(200, 14))     # Longitude out of range
.validate_lonlat(c(121.25))      # Length not 2
.validate_lonlat(c("121", "14")) # Not numeric
} # }
```
