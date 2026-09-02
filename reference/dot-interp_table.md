# Apply Matrix Lookup Table Parameter Over Age Vector

Converts data.frame to matrix, validates, cleans (removes NA rows), and
performs fast linear interpolation.

## Usage

``` r
.interp_table(param, age_vec)
```

## Arguments

- param:

  Numeric matrix or data.frame with 2 columns (age, modifier).

- age_vec:

  Numeric vector of ages to interpolate at.

## Value

Numeric vector of interpolated modifier values.

## Details

Steps:

1.  Convert data.frame to matrix if needed

2.  Validate 2 columns and numeric values

3.  Remove rows with NA values (with warning if any found)

4.  Call
    [`.memo_fast_interp_approx_digest()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-memo_fast_interp_approx_digest.md)
    for memoised interpolation
