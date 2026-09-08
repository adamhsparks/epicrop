# Validate and Parse Emergence Date

Converts various date formats (character, Date, IDate, POSIXt) to a
validated IDate object. Supports multiple date string formats and
rejects ambiguous numeric dates (*e.g.*, "07/01/2000").

## Usage

``` r
.validate_emergence(x)
```

## Arguments

- x:

  A date input: character string,
  [Date](https://rdrr.io/r/base/Dates.html),
  [data.table::IDate](https://rdrr.io/pkg/data.table/man/IDateTime.html),
  or [POSIXt](https://rdrr.io/r/base/DateTimeClasses.html) object.

## Value

An
[data.table::IDate](https://rdrr.io/pkg/data.table/man/IDateTime.html)
object if valid. Aborts with informative error message otherwise.

## Details

**Date string formats accepted:**

- ISO standard: `"2000-07-01"`, `"2000/07/01"`

- European: `"01-07-2000"`, `"01/07/2000"`

- US: `"07-01-2000"`, `"07/01/2000"`

- Named month: `"01 Jul 2000"`, `"1 July 2000"`, `"Jul 01 2000"`

**Ambiguous dates rejected:** Dates where both first and second numeric
components are \<= 12 (*e.g.*, `"07/01/2000"`) are rejected to prevent
misinterpretation as either July 1 or January 7.
