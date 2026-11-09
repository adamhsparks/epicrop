# Normalise Location Input to Standard Matrix Format

Converts various location input formats (numeric vector, matrix, data
frame, or list of pairs) to a standard 2-column numeric matrix with
columns LON and LAT. Preserves row names where available and validates
all coordinates are finite.

## Usage

``` r
.norm_locations(ll)
```

## Arguments

- ll:

  Location coordinates in one of the following formats:

  - Numeric vector of length 2: `c(lon, lat)` for a single location

  - Numeric vector of length 2n: `c(lon1, lat1, lon2, lat2, ...)` for n
    locations

  - 2-column matrix or data frame: each row is `c(lon, lat)`

  - List of numeric vectors: each element is `c(lon, lat)`. If named,
    names are preserved as row names.

## Value

A numeric 2-column matrix with:

- Column names: `"LON"`, `"LAT"`

- Row names preserved from input if available

- Storage mode: `"double"`

- All values finite and valid coordinates

## Input Processing

1.  **List** (non-data.frame): Validate elements, extract names, convert
    to matrix

2.  **Data frame**: Validate 2 columns and numeric type, convert to
    matrix

3.  **Matrix**: Validate 2 columns and numeric type, set column names

4.  **Numeric vector**: Validate even length, reshape to 2 columns

## Validation

- All coordinates must be finite (no NA, Inf, or NaN)

- Matrix must have exactly 2 columns

- Numeric vector must have even length (length \>= 2)

- List elements must be numeric vectors of length 2 with finite values

## Examples

``` r
# Single location as vector
.norm_locations(c(121.255669, 14.16742))
#>           LON      LAT
#> [1,] 121.2557 14.16742

# Multiple locations as vector
.norm_locations(c(121.255669, 14.16742, 120.985, 14.6042))
#>           LON      LAT
#> [1,] 121.2557 14.16742
#> [2,] 120.9850 14.60420

# Named matrix
locs <- rbind(
  "IRRI_ZES" = c(121.255669, 14.16742),
  "Metro_Manila" = c(120.985, 14.6042)
)
.norm_locations(locs)
#>                   LON      LAT
#> IRRI_ZES     121.2557 14.16742
#> Metro_Manila 120.9850 14.60420

# Named list
.norm_locations(list(
  site1 = c(121.255669, 14.16742),
  site2 = c(120.985, 14.6042)
))
#>            LON      LAT
#> site1 121.2557 14.16742
#> site2 120.9850 14.60420
```
