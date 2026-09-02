# Prepare X-Y Coordinates for Interpolation

Extracts x-y columns, sorts by x, and removes duplicate x-values.

## Usage

``` r
.prep_xy(mat)
```

## Arguments

- mat:

  Numeric 2-column matrix (x, y).

## Value

List with elements `x` and `y` (both sorted, deduplicated).

## Deduplication

When duplicate x-values exist (user error or data source quirk):

- Sorts by x-coordinate

- Keeps first y-value for each x (others discarded)

- Warns internally via parent function
