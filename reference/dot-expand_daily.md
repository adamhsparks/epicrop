# Expand Flexible Inputs to Daily Vectors

Accepted input forms:

- **Function**: evaluated on either daily `TEMP` or `age_values`
  (selected by `fn_input`);

- **Matrix/vector length \> 1**: interpolated against `age_values` via
  [`.interp()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-interp.md);

- **Scalar**: replicated to length `duration`.

## Usage

``` r
.expand_daily(x, w, age_values, duration, fn_input = c("temp", "age"))
```

## Arguments

- x:

  Input object to expand.

- w:

  Weather data; `w$TEMP` used when `fn_input = "temp"`.

- age_values:

  Numeric age vector used for interpolation / function input.

- duration:

  Integer simulation length.

- fn_input:

  Character scalar; either `"temp"` or `"age"` indicating what to pass
  to `x` when `x` is a function.

## Value

Numeric vector of length `duration`.

## Details

Internal helper that normalises scalar, vector/matrix, or function
inputs into daily vectors for simulation.

## See also

[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md),
[`.interp()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-interp.md)
