# Check if Model Function Has Duration Formal Parameter

Inspects a model function to determine if `duration` is an explicit
formal parameter, allowing automatic forwarding of `window_days`.

## Usage

``` r
.model_has_duration_formal(model_fun)
```

## Arguments

- model_fun:

  Function. An epicrop model function.

## Value

Logical. TRUE if `duration` is in the function's formal parameters.

## Details

Used by
[`run_epicrop_model()`](https://adamhsparks.codeberg.page/epicrop/reference/run_epicrop_model.md)
to auto-fill the `duration` argument with `window_days` when the model
exposes it. This enables flexible model invocation without requiring the
user to pass `duration` explicitly.

## Examples

``` r
.model_has_duration_formal(leaf_blast)  # TRUE
#> [1] FALSE
.model_has_duration_formal(seir)        # TRUE (base model)
#> [1] TRUE
```
