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
#> Error in .check_date("2023-10-15"): could not find function ".check_date"

# fails
if (FALSE) { # \dontrun{
 .check_date("120-100-2023")
} # }
```
