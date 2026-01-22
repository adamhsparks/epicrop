# Prepare daily modifiers for simulation

Interpolates and calculates all daily modifier values needed for disease
progression simulation, including age, temperature, wetness, and other
crop-specific parameters.

## Usage

``` r
.prepare_modifiers(
  w,
  age_vec,
  duration,
  RcA,
  RcT,
  RcW,
  RcOpt,
  p,
  i,
  RRS,
  a,
  RRLEX,
  RRG,
  rhlim,
  rainlim,
  simple_wetness
)
```

## Arguments

- w:

  A data.tablecontaining weather data with `TEMP`, `RHUM`, and `RAIN`
  columns.

- age_vec:

  Numeric vector of crop ages (in days or physiological time).

- duration:

  Integer. Total number of simulation days.

- RcA:

  Numeric vector or data.frame defining the age correction factor.

- RcT:

  Numeric vector or data.frame defining the temperature correction
  factor.

- RcW:

  Numeric vector, data.frame, or NULL defining the wetness correction
  factor. If NULL and not using simple_wetness, linear scaling is
  applied.

- RcOpt:

  Numeric scalar. Optimal infection rate.

- p:

  Numeric vector or data.frame defining the probability of successful
  infection.

- i:

  Numeric vector or data.frame defining the infection efficiency.

- RRS:

  Numeric vector or data.frame defining the relative rate of senescence.

- a:

  Numeric vector or data.frame defining the aggregation parameter.

- RRLEX:

  Numeric vector or data.frame defining the relative rate of latent
  expansion.

- RRG:

  Numeric vector or data.frame defining the relative rate of growth.

- rhlim:

  Numeric scalar. Relative humidity threshold (%) for leaf wetness.

- rainlim:

  Numeric scalar. Rainfall threshold (mm) for leaf wetness.

- simple_wetness:

  Logical. If TRUE, use binary wetness (wet/dry). If FALSE, calculate
  hourly leaf wetness duration.

## Value

A named list containing interpolated daily values:

- `Rc` - Combined correction factor (RcOpt \* RcA \* RcT \* RcW)

- `RRLEX` - Relative rate of latent expansion

- `RRG` - Relative rate of growth

- `RRS` - Relative rate of senescence

- `p` - Probability of successful infection

- `i` - Infection efficiency

- `a` - Aggregation parameter

## Details

For simple wetness, a day is considered wet if either:

- Relative humidity \>= rhlim, OR

- Rainfall \>= rainlim

For complex wetness, hourly leaf wetness duration is calculated and
either interpolated using RcW or linearly scaled (0-24 hours -\> 0-1).
