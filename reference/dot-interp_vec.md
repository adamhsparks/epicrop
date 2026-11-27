# Apply Vector Parameter Over Age Vector

Validates that parameter vector matches age vector length exactly.

## Usage

``` r
.interp_vec(param, age_vec)
```

## Arguments

- param:

  Numeric vector of parameter values.

- age_vec:

  Numeric vector of ages.

## Value

Numeric vector (same as `param` if length matches).

## Details

Aborts if `length(param) != length(age_vec)`.
