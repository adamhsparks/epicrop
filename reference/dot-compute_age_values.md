# Compute Age Driver Vector for Daily Modifiers

Internal helper that generates per-day age values used to evaluate
age-dependent modifiers in
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md).

## Usage

``` r
.compute_age_values(age_driver, duration, w, thermal_time)
```

## Arguments

- age_driver:

  Character scalar: `"day"` or `"dvs"`.

- duration:

  Integer simulation length.

- w:

  Weather subset used for simulation; requires `TEMP` when
  `age_driver = "dvs"`.

- thermal_time:

  Thermal-time list used by
  [`.calculate_dvs()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-calculate_dvs.md)
  when `age_driver = "dvs"`.

## Value

Numeric vector of length `duration`:

- day index `0:(duration-1)` when `age_driver = "day"`;

- DVS values on \[0, 2\] when `age_driver = "dvs"`.

## See also

[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md),
[`.calculate_dvs()`](https://adamhsparks.codeberg.page/epicrop/reference/dot-calculate_dvs.md)
