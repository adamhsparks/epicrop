# Dispatch to SEIR Core Engine Variant

Internal helper that chooses between core simulators with or without
lesion expansion.

## Usage

``` r
.run_seir_core(
  lex_on,
  duration,
  onset_idx,
  H0,
  Iprim,
  Rc,
  p_days,
  i_days,
  RRG_vals,
  RRS_vals,
  a_vals,
  Sx,
  RRLEX_vals = NULL
)
```

## Arguments

- lex_on:

  Logical scalar; if `TRUE`, use
  [`.seir_core_with_lex()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-seir_core_with_lex.md),
  otherwise
  [`.seir_core_no_lex()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-seir_core_no_lex.md).

- duration:

  Integer simulation length.

- onset_idx:

  Integer day index for primary inoculum.

- H0:

  Initial healthy sites.

- Iprim:

  Primary inoculum vector.

- Rc:

  Daily infection-rate vector.

- p_days:

  Daily latent-period vector.

- i_days:

  Daily infectious-period vector.

- RRG_vals:

  Daily growth-rate vector.

- RRS_vals:

  Daily senescence-rate vector.

- a_vals:

  Daily aggregation-coefficient vector.

- Sx:

  Carrying capacity (max sites).

- RRLEX_vals:

  Daily lesion-expansion vector; required only when `lex_on = TRUE`.

## Value

A `data.table` produced by the selected core simulator.

## See also

[`.seir_core_no_lex()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-seir_core_no_lex.md),
[`.seir_core_with_lex()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-seir_core_with_lex.md),
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
