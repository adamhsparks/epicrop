# Check Coordinate Value is Within Valid Range

Validates a single coordinate value against min/max bounds.

## Usage

``` r
.check_coord(val, name, lo, hi)
```

## Arguments

- val:

  Numeric value to check.

- name:

  Character name of coordinate (for error messages).

- lo:

  Numeric lower bound (inclusive).

- hi:

  Numeric upper bound (inclusive).

## Value

NULL (invisibly). Called for side effects (validation/abort).

## Details

Aborts if value is NA, Inf, or outside \[lo, hi\].
