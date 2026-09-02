# Identify Ambiguous Numeric Date Strings

Detects character date values that can be interpreted ambiguously as
either day-month-year or month-day-year because both the first and
second numeric components are less than or equal to 12.

## Usage

``` r
.is_ambiguous_numeric_date(x)
```

## Arguments

- x:

  A character scalar containing a potential date value.

## Value

A single logical value. Returns `TRUE` if `x` is an ambiguous numeric
date of the form `dd/mm/yy`, `mm/dd/yy`, `dd-mm-yy`, or `mm-dd-yy` where
both the first and second components are less than or equal to 12.
Returns `FALSE` otherwise.

## Details

Ambiguous dates should not be parsed automatically because their
intended meaning cannot be determined reliably. For example,
`"07/01/2000"` could represent either 7 January 2000 or 1 July 2000.

## Examples

``` r
.is_ambiguous_numeric_date("07/01/2000")
#> Error in .is_ambiguous_numeric_date("07/01/2000"): could not find function ".is_ambiguous_numeric_date"
# TRUE (could be July 1 or January 7)

.is_ambiguous_numeric_date("01-07-2000")
#> Error in .is_ambiguous_numeric_date("01-07-2000"): could not find function ".is_ambiguous_numeric_date"
# TRUE

.is_ambiguous_numeric_date("31/01/2000")
#> Error in .is_ambiguous_numeric_date("31/01/2000"): could not find function ".is_ambiguous_numeric_date"
# FALSE (31 > 12, clearly day-month-year)

.is_ambiguous_numeric_date("07/31/2000")
#> Error in .is_ambiguous_numeric_date("07/31/2000"): could not find function ".is_ambiguous_numeric_date"
# FALSE (31 > 12, clearly month-day-year)

.is_ambiguous_numeric_date("2000-07-01")
#> Error in .is_ambiguous_numeric_date("2000-07-01"): could not find function ".is_ambiguous_numeric_date"
# FALSE (ISO format with year first)
```
