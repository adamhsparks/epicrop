# Apply Development-Stage Infection Window to Rc

If `infection_window` is provided, multiplies `Rc` by 1 when
`dvs_start <= DVS <= dvs_end`, else by 0. DVS values are taken from
`age_values` when `age_driver = "dvs"`, otherwise computed from weather
and `thermal_time`.

## Usage

``` r
.apply_infection_window(
  Rc,
  infection_window,
  age_driver,
  age_values,
  w,
  thermal_time
)
```

## Arguments

- Rc:

  Numeric vector of daily infection rates.

- infection_window:

  `NULL` or list with `dvs_start`, `dvs_end`.

- age_driver:

  Character scalar: `"day"` or `"dvs"`.

- age_values:

  Numeric age vector.

- w:

  Weather data used when DVS must be computed.

- thermal_time:

  Thermal-time list for DVS calculation.

## Value

Numeric vector of gated infection rates (`Rc`).

## Details

Internal helper that gates daily infection rates by a DVS susceptibility
window.

## See also

[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md),
[`.calculate_dvs()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-calculate_dvs.md)
