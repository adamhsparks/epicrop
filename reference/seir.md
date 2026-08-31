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
  | *TEMP* | Mean daily temperature (degrees C) |
  | *RHUM* | Mean daily relative humidity (%) |
  | *RAIN* | Mean daily rainfall (mm) |
  | *TMIN* | **Optional** Minimum daily temperature (degrees C), see TMIN/TMAX |

       Details

  *TMAX* \| **Optional** Maximum daily temperature (degrees C), see
  TMIN/TMAX Details *LAT* \| **Optional** latitude of weather
  observation, see LAT/LON Details *LON* \| **Optional** longitude of
  weather observation, see LAT/LON Details

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
  being temperature in degrees C and the second the modifier (bounded by
  0 and 1). Described in Tables 1 and 2 of Savary *et al.* 2012.

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

  List with `base_temp` (degrees C) and `gdd_to_maturity` (degrees C
  days). Only required when `age_driver = "dvs"`. Determines how growing
  degree days are accumulated and converted to development stage (DVS).

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

- simday:

  Day of simulation (1-indexed, where 1 is the emergence date)

- dates:

  Date of simulation (Date class)

- sites:

  Total number of healthy sites present on day "x"

- latent:

  Number of latent sites present on day "x"

- infectious:

  Number of infectious sites present on day "x"

- removed:

  Number of removed sites present on day "x"

- senesced:

  Number of senesced sites present on day "x"

- rateinf:

  Rate of infection (new infections on day "x")

- rlex:

  Rate of lesion expansion (healthy sites converted to infectious via
  direct expansion on day "x")

- rtransfer:

  Rate of transfer from latent to infectious sites on day "x"

- rremoved:

  Rate of removal from infectious to removed sites on day "x"

- rgrowth:

  Rate of growth of healthy sites on day "x"

- rsenesced:

  Rate of senescence of healthy sites on day "x"

- diseased:

  Number of diseased (latent + infectious + removed) sites on day "x"

- intensity:

  Proportion of diseased sites (latent + infectious) per total living
  sites (excludes removed and senesced) on day "x"

- lat:

  Latitude value if provided by the `wth` object

- lon:

  Longitude value if provided by the `wth` object

The returned object also has an attribute `AUDPC` (accessed via
`attr(result, "AUDPC")`) containing the area under the disease progress
curve for the entire simulation, calculated using the trapezoidal rule.

## Output Structure

The output is organized into three categories:

**State Variables** (stocks at end of each day):

- `sites`, `latent`, `infectious`, `removed`, `senesced`

**Flow Variables** (rates/changes during each day):

- `rateinf`, `rlex`, `rtransfer`, `rremoved`, `rgrowth`, `rsenesced`

**Derived Variables** (calculated from states):

- `diseased`, `intensity`

**Mass Balance:** At any day:
`sites + latent + infectious + removed + senesced` should equal initial
sites (H0) plus cumulative growth minus cumulative senescence
(accounting for numerical precision).

## *a* – Aggregation Details

When *a* is set to `1` the assumption is that that there is no disease
aggregation with new infections occurring at random among the healthy
sites. When *a* is greater than `1` there is aggregation in the disease
occurrence, the pathogen is unable to access the entire population of
healthy sites, which results in disease aggregation. Refer to Savary *et
al.* (2012) for greater detail.

## TMIN/TMAX Details

If `simple_wetness` is set to `FALSE`, the function will use the `TMIN`
and `TMAX` columns in the `wth` object to calculate the leaf wetness
value for 24 hours of the day using the `rhlim` and `rainlim` values and
then uses `RcW` to calculate a value between 0 and 1 for the whole day.
When `simple_wetness` is set to `TRUE`, the function only sets the leaf
wetness to 0 or 1 for the day based on the `rhlim` and `rainlim` values.

## LAT/LON Details

If the `wth` object provides `LAT` and `LON` columns, these will be
included in the output for mapping purposes. Both values must be
present. These columns are provided by default when using
[`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md).

## References

Sparks, A. H., P. D. Esker, M. Bates, W. Dall' Acqua, Z. Guo, V.
Segovia, S.D. Silwal, S. Tolos, and K. A. Garrett, 2008. Ecology and
Epidemiology in R: Disease Progress over Time. *The Plant Health
Instructor*.
[doi:10.1094/PHI-A-2008-0129-02](https://doi.org/10.1094/PHI-A-2008-0129-02)
.

Madden, L. V., G. Hughes, and F. van den Bosch. 2007. *The Study of
Plant Disease Epidemics*. American Phytopathological Society, St. Paul,
MN. [doi:10.1094/9780890545058](https://doi.org/10.1094/9780890545058) .

Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario, J.
Modeling and mapping potential epidemics of rice diseases globally.
*Crop Protection*, Volume 34, 2012, Pages 6-17, ISSN 0261-2194
[doi:10.1016/j.cropro.2011.11.009](https://doi.org/10.1016/j.cropro.2011.11.009)
.

## See also

Disease-specific helper functions:

- [`bacterial_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),

- [`brown_spot()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),

- [`leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),

- [`sheath_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),

- [`tungro()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),

- [`modified_kim_leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),

- [`modified_kim_sheath_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),

- [`leaf_rust()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md),
  and

- [`s_tritici_blotch()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md).

## Author

Adam H. Sparks, <adamhsparks@gmail.com> from original by Robert J.
Hijmans, Rene Pangga and Jorrel Aunario.
