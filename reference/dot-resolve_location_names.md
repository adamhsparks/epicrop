# Resolve Location Names from Matrix and Inputs

Determines appropriate names for locations by checking user-provided
names, rownames from the location matrix, or generating default names.

## Usage

``` r
.resolve_location_names(locations, location_names)
```

## Arguments

- locations:

  A numeric 2-column matrix (LON, LAT).

- location_names:

  Optional character vector of location names.

## Value

A character vector of location names with length equal to the number of
rows in `locations`.

## Priority

1.  User-provided `location_names` (must match number of locations)

2.  Non-empty rownames from `locations` matrix

3.  Auto-generated names: `location01`, `location02`, ...

## Examples

``` r
locations <- rbind(
  "IRRI_ZES" = c(121.255669, 14.16742),
  "Metro_Manila" = c(120.985, 14.6042)
)

# Uses rownames by default
.resolve_location_names(locations, NULL)
#> [1] "IRRI_ZES"     "Metro_Manila"

# Overrides with user names
.resolve_location_names(locations, c("Site A", "Site B"))
#> [1] "Site A" "Site B"
```
