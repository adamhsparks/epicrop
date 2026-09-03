# Resolve Thermal Time Configuration for SEIR

This function centralizes logic for:

- enforcing that `thermal_time` is supplied when `age_driver = "dvs"`;

- validating `infection_window` structure when provided; and

- constructing a fallback `thermal_time` list from `infection_window`
  (`base_temp`, `gdd_to_maturity`) when possible.

## Usage

``` r
.resolve_thermal_time(age_driver, thermal_time, infection_window)
```

## Arguments

- age_driver:

  Character scalar. Age basis used by
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md);
  expected values are `"day"` or `"dvs"`.

- thermal_time:

  `NULL` or a list of thermal accumulation parameters. Expected fields
  are:

  - `base_temp` (numeric scalar)

  - `gdd_to_maturity` (numeric scalar \> 0)

- infection_window:

  `NULL` or a list defining a DVS infection gate. Must include:

  - `dvs_start`

  - `dvs_end` Optionally may include:

  - `base_temp`

  - `gdd_to_maturity`

## Value

A `thermal_time` list (possibly unchanged input, or derived from
`infection_window`) or `NULL` when not needed.

## Details

Internal helper that validates and resolves thermal-time inputs required
by
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
when using development stage (`age_driver = "dvs"`) and/or an
infection-window constraint.

## Error Conditions

Aborts when:

- `age_driver = "dvs"` and `thermal_time` is `NULL`;

- `infection_window` is malformed (missing `dvs_start`/`dvs_end`);

- `infection_window` is used to supply thermal-time values but is
  missing `base_temp` and/or `gdd_to_maturity`.

## See also

[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md),
[`.calculate_dvs()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-calculate_dvs.md)
