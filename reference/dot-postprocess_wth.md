# Post-Process NASA POWER Weather Data

Renames POWER columns to epicrop standards, converts dates and finalises
the object via
[`.as_epicrop_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-as_epicrop_wth.md).

## Usage

``` r
.postprocess_wth(wth)
```

## Arguments

- wth:

  data.table from
  [`nasapower::get_power()`](https://docs.ropensci.org/nasapower/reference/get_power.html).

## Value

data.table of class `epicrop.wth`.
