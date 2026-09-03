# Compute Leaf-Wetness Modifier for Infection Rate

Supports two modes:

- **Simple wetness** (`simple_wetness = TRUE`): binary 0/1 using daily
  thresholds `RHUM >= rhlim` OR `RAIN >= rainlim`.

- **Hourly wetness** (`simple_wetness = FALSE`): computes wet hours via
  [`.calculate_leaf_wetness()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-calculate_leaf_wetness.md)
  and maps to \[0, 1\] either linearly (`wet/24`) or through
  interpolation over user-supplied `RcW`.

## Usage

``` r
.compute_wetness_modifier(
  w,
  simple_wetness,
  rhlim,
  rainlim,
  weighted_wetness,
  RcW
)
```

## Arguments

- w:

  Weather data for the simulation window.

- simple_wetness:

  Logical; if `TRUE` use binary daily wetness.

- rhlim:

  Numeric RH threshold (%).

- rainlim:

  Numeric rainfall threshold (mm).

- weighted_wetness:

  Logical; passed to
  [`.calculate_leaf_wetness()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-calculate_leaf_wetness.md).

- RcW:

  `NULL` or 2-column numeric matrix giving wetness-response curve.

## Value

Numeric vector of daily wetness multipliers (typically bounded \[0,
1\]).

## Details

Internal helper that computes daily wetness effects applied to `Rc` in
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md).

## See also

[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md),
[`.calculate_leaf_wetness()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-calculate_leaf_wetness.md),
[`.interp()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-interp.md)
