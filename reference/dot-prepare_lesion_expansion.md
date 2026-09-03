# Prepare Lesion-Expansion Inputs for Core SEIR Engine

Internal helper that derives daily lesion-expansion rates and determines
whether lesion expansion is active.

## Usage

``` r
.prepare_lesion_expansion(RRLEX, w, age_values, duration)
```

## Arguments

- RRLEX:

  Lesion expansion specification (`NULL`, scalar, vector/matrix, or
  function) as accepted by
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md).

- w:

  Weather data used when function inputs depend on temperature.

- age_values:

  Daily age vector for interpolation.

- duration:

  Integer simulation length.

## Value

A named list with:

- `lex_on`: logical scalar, `TRUE` if any daily RRLEX \> 0;

- `RRLEX_vals`: numeric vector of daily rates, or `NULL` when RRLEX is
  `NULL`.

## See also

[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md),
[`.expand_daily()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-expand_daily.md)
