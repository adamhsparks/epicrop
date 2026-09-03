# Build Primary Inoculum Schedule

Internal helper that creates the daily primary inoculum vector and
validates onset indexing.

## Usage

``` r
.build_primary_inoculum(onset, duration, I0)
```

## Arguments

- onset:

  1-indexed day of disease onset (integer-like).

- duration:

  Integer simulation length in days.

- I0:

  Initial inoculum amount injected at `onset`.

## Value

A named list with:

- `Iprim`: numeric vector (length `duration`) with a single non-zero
  entry at `onset`;

- `onset_idx`: validated integer onset index.

## Error Conditions

Aborts if `onset` is outside `[1, duration]`.

## See also

[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
