# Validate Lookup Table Structure

Checks that lookup table (matrix) has exactly 2 columns and contains
numeric values.

## Usage

``` r
.validate_lookup(param)
```

## Arguments

- param:

  Numeric matrix.

## Value

NULL (invisibly). Called for side effects (validation/abort).

## Details

Validates:

- Exactly 2 columns

- Both columns contain numeric values

- At least 1 row
