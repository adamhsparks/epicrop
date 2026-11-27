# Detect Weather Bundle vs. Simple List

Determines if `wth_list` is a bundle (multiple locations/scenarios) or a
simple year-indexed list.

## Usage

``` r
.is_wth_bundle(wth_list)
```

## Arguments

- wth_list:

  A weather list (checked for class and name structure).

## Value

Logical. TRUE if bundle, FALSE if year-indexed list.

## Details

A bundle has class `epicrop.wth.bundle` OR names that contain non-year
strings. Year-indexed lists have names that are all 4-digit year
strings.
