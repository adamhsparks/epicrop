# Check and parse date

Check and parse date

## Usage

``` r
.check_date(x)
```

## Arguments

- x:

  a date input (character, Date, POSIXct).

## Value

A validated POSIXct date object.

## Examples

``` r
# passes
.check_date("2023-10-15")
#> [1] "2023-10-15 AWST"

# fails
if (FALSE) { # \dontrun{
 .check_date("120-100-2023")
} # }
```
