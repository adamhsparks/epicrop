# Susceptible-Exposed-Infectious-Removed (SEIR) Model Framework with Lesion Expansion

This function is originally used by specific disease models in ‘EPIRICE’
and ‘EPIWHEAT’ to model disease intensity of several rice diseases and
two wheat diseases respectively. Given proper values it can be used with
other pathosystems as well.

## Usage

``` r
seir(
  wth,
  emergence,
  onset,
  duration,
  H0,
  I0,
  RcA,
  RcT,
  RcOpt,
  p,
  i,
  Sx,
  a = 1,
  rhlim = 90,
  rainlim = 5,
  RRS = NULL,
  RRG = NULL,
  RcW = NULL,
  RRLEX = NULL,
  simple_wetness = TRUE,
  weighted_wetness = FALSE,
  age_driver = c("day", "dvs"),
  thermal_time = NULL,
  steps_per_day = 1L,
  infection_window = NULL
)
```

## Arguments

- wth:

  a `data.frame` of class `epicrop.wth` from either
  [`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md)
  or
  [`format_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/format_wth.md)
  containing weather on a daily time-step with the following field
  names:

  |  |  |
  |----|----|
  | **Field Name** | **Value** |
  | *YYYYMMDD* | Date as Year Month Day (ISO8601) |
  | *DOY* | Consecutive day of year, commonly called "Julian date" |
  | *TEMP* | Mean daily temperature (°C) |
  | *RHUM* | Mean daily relative humidity (%) |
  | *RAIN* | Mean daily rainfall (mm) |
  | *TMIN* | **Optional** Minimum daily temperature (°C), see TMIN/TMAX Details |
  | *TMAX* | **Optional** Maximum daily temperature (°C), see TMIN/TMAX Details |
  | *LAT* | **Optional** latitude of weather observation, see LAT/LON Details |
  | *LON* | **Optional** longitude of weather observation, see LAT/LON Details |

- emergence:

  Expected date of plant emergence (or transplanting for rice) entered
  in `YYYY-MM-DD` format (character).

- onset:

  expected number of days until the onset of disease after emergence
  date (day, integer). For this modern seir() variant onset is 1-indexed
  (1 = emergence day). When comparing with legacy SEIR(), translate as
  legacy_onset = onset - 1.

- duration:

  simulation duration *i. e.*, growing season length (day, integer).
  Described in Table 1 of Savary *et al.* 2012.

- H0:

  initial number of plant's healthy sites (integer). Described in Table
  1 of Savary *et al.* 2012.

- I0:

  initial number of infective sites (scalar integer). Described in Table
  1 of Savary *et al.* 2012. I0 is injected as new infections (become
  infectious after `p` days).

- RcA:

  modifier for *Rc* (the basic infection rate corrected for removals)
  for crop age as a numeric two-column matrix with the first column
  being the crop age in days (0 to `duration-1`) or DVS (0 to 2) and the
  second being the modifier (bounded by 0 and 1). Described in Tables 1
  and 2 of Savary *et al.* 2012.

- RcT:

  modifier for *Rc* (the basic infection rate corrected for removals)
  for temperature as a numeric two-column matrix with the first column
  being temperature in °C and the second the modifier (bounded by 0 and
  1). Described in Tables 1 and 2 of Savary *et al.* 2012.

- RcOpt:

  potential basic infection rate corrected for removals (numeric).
  Described in Table 1 of Savary *et al.* 2012. This value can be
  modified to reflect crop cultivar resistance ratings as well, see Kim
  \_et al. \_ 2015 for more details.

- p:

  duration of latent period (day, integer or function of temperature).
  Described in Table 1 of Savary *et al.* 2012.

- i:

  duration of infectious period (day, integer or function of
  temperature). Described in Table 1 of Savary *et al.* 2012.

- Sx:

  maximum number of sites (integer). Described in Table 1 of Savary *et
  al.* 2012.

- a:

  aggregation coefficient, values are from 1 to \>1 (numeric or function
  of DVS). Described in Table 1 of Savary *et al.* 2012 and Table 1 of
  Savary *et al.* 2015. See further details in ***a* - Aggregation**
  section. Defaults to 1.

- rhlim:

  relative humidity value threshold to decide whether leaves are wet or
  not (numeric). Described in Table 1 of Savary *et al.* 2012. Savary
  *et al.* 2012 used `90`. Defaults to 90.

- rainlim:

  rainfall amount (mm) threshold to decide whether leaves are wet or not
  (numeric). Described in Table 1 of Savary *et al.* 2012. Savary *et
  al.* 2012 used `5`. Defaults to 5.

- RRS:

  relative rate of physiological senescence (numeric, vector, or
  function of DVS). Described in Table 1 of Savary *et al.* 2012.

- RRG:

  relative rate of growth (numeric, vector, or function of time).
  Described in Table 1 of Savary *et al.* 2012.

- RcW:

  Optional modifier for *Rc* to calculate leaf wetness effect from 1 if
  wet to 0 if dry. This is a numeric two-column matrix with the first
  column being hours (0 to 24) and the second the modifier (bounded by 0
  and 1). Used when `simple_wetness = FALSE` (hourly/derived wetness).
  If `NULL` (default), a linear relationship is used where 0 hours wet =
  0 and 24 hours wet = 1. Ignored if `simple_wetness = TRUE`.

- RRLEX:

  Rate of lesion expansion (scalar, vector, or function). Allows healthy
  sites to be directly converted to infectious sites without passing
  through latent stage. Set to 0 (default) for standard SEIR behavior.

- simple_wetness:

  Logical. If `TRUE` (default), uses binary wetness calculation (0 or 1)
  based on `rhlim` and `rainlim` thresholds. If `FALSE`, uses hourly
  wetness duration calculation (requires TMIN, TMAX, LAT, DOY in `wth`)
  and applies `RcW` curve.

- weighted_wetness:

  Logical. If `TRUE`, compute weighted hourly wetness (0 to 24); if
  `FALSE`, count hours where hourly RH \>= rhlim (0 to 24). Only used
  when `simple_wetness = FALSE`. Defaults to FALSE.

- age_driver:

  Character. Use "day" (default, 0-indexed days from emergence) or "dvs"
  (development stage 0-2) for age-based modifiers in `RcA`. When "dvs",
  `thermal_time` parameter is required. Defaults to "day".

- thermal_time:

  List with `base_temp` (°C) and `gdd_to_maturity` (°C·days). Only
  required when `age_driver = "dvs"`. Determines how growing degree days
  are accumulated and converted to development stage (DVS).

- steps_per_day:

  Integer. Number of sub-daily time steps for numerical integration.
  Default is 1 (daily steps). Values of 2, 4, or 8 can improve accuracy
  for stiff systems at cost of computation time. Should be a power of 2.
  Defaults to 1.

- infection_window:

  Optional list defining DVS-based infection window:

  - `dvs_start`: DVS when susceptibility begins

  - `dvs_end`: DVS when susceptibility ends

  - `base_temp`: Base temperature for GDD (optional, uses thermal_time
    if present)

  - `gdd_to_maturity`: GDD to maturity (optional, uses thermal_time if
    present)

## Value

A
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
object containing the following columns:

... (documentation omitted for brevity) ...

## Author

Adam H. Sparks, <adamhsparks@gmail.com> from original by Robert J.
Hijmans, Rene Pangga and Jorrel Aunario.
