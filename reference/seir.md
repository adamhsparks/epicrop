# Susceptible-Exposed-Infectious-Removed (SEIR) Model Framework

This function is originally used by specific disease models in ‘EPIRICE’
to model disease intensity of several rice diseases. Given proper values
it can be used with other pathosystems as well.

## Usage

``` r
seir(
  wth,
  emergence,
  onset,
  duration,
  rhlim,
  rainlim,
  H0,
  I0,
  RcA,
  RcT,
  RcOpt,
  p,
  i,
  Sx,
  a,
  RRS,
  RRG,
  RcW = NULL,
  simple_wetness = TRUE
)
```

## Arguments

- wth:

  a `data.frame` of class `epicrop. wth` from either
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

  expected date of plant emergence (or transplanting for rice) entered
  in `YYYY-MM-DD` format (character). Described in Table 1 of Savary
  \_et al. \_ 2012.

- onset:

  expected number of days until the onset of disease after emergence
  date (day, integer). Described in Table 1 of Savary *et al.* 2012.

- duration:

  simulation duration *i. e.*, growing season length (day, integer).
  Described in Table 1 of Savary \_et al. \_ 2012.

- rhlim:

  relative humidity value threshold to decide whether leaves are wet or
  not (numeric). Described in Table 1 of Savary *et al.* 2012. Savary
  *et al.* 2012 used `90`.

- rainlim:

  rainfall amount (mm) threshold to decide whether leaves are wet or not
  (numeric). Described in Table 1 of Savary *et al.* 2012. Savary *et
  al.* 2012 used `5`.

- H0:

  initial number of plant's healthy sites (integer). Described in Table
  1 of Savary *et al.* 2012.

- I0:

  initial number of infective sites (integer). Described in Table 1 of
  Savary *et al.* 2012. I0 is injected as new infections on the onset
  (become infectious after `p` days).

- RcA:

  modifier for *Rc* (the basic infection rate corrected for removals)
  for crop age as a numeric two-column matrix with the first column
  being the crop age in days (1 to `duration`) and the second being the
  modifier (bounded by 0 and 1). Described in Tables 1 and 2 of Savary
  *et al.* 2012.

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

  duration of latent period (day, integer). Described in Table 1 of
  Savary *et al.* 2012.

- i:

  duration of infectious period (day, integer). Described in Table 1 of
  Savary *et al.* 2012.

- Sx:

  maximum number of sites (integer). Described in Table 1 of Savary *et
  al.* 2012.

- a:

  aggregation coefficient, values are from 1 to \>1 (numeric). Described
  in Table 1 of Savary *et al.* 2012 and Table 1 of Savary *et
  al.* 2015. See further details in ***a* - Aggregation** section.

- RRS:

  relative rate of physiological senescence (numeric). Described in
  Table 1 of Savary \_et al. \_ 2012.

- RRG:

  relative rate of growth (numeric). Described in Table 1 of Savary *et
  al.* 2012.

- RcW:

  Optional modifier for *Rc* to calculate leaf wetness effect from 1 if
  wet to 0 if dry. This is a numeric two-column matrix with the first
  column being hours (1 to 24) and the second the modifier (bounded by 0
  and 1). Used when `simple_wetness = FALSE` (hourly/derived wetness).
  If `NULL` (default), a linear relationship is used where 0 hours wet =
  0 and 24 hours wet = 1. Ignored if `simple_wetness = TRUE`.

- simple_wetness:

  Logical. If `TRUE` (default), the function uses a simple calculation
  of leaf wetness based on daily `rhlim` and `rainlim` values (wetness
  is either 0 or 24 hours). If `FALSE`, the function uses a more complex
  calculation of leaf wetness based on hourly temperature and humidity
  patterns derived from daily TMIN/TMAX values, requiring additional
  weather data columns (TMIN, TMAX, LAT, DOY), and applies `RcW` to
  determine the effect of leaf wetness duration on the infection rate.

## Value

A
[`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
object containing the following columns:

- simday:

  Day of simulation (1-indexed, where 1 is the emergence date)

- dates:

  Date of simulation

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

- rtransfer:

  Rate of transfer from latent to infectious sites

- rgrowth:

  Rate of growth of healthy sites

- rsenesced:

  Rate of senescence of healthy sites

- diseased:

  Number of diseased (latent + infectious + removed) sites on day "x"

- intensity:

  Proportion of diseased sites (latent + infectious) per total living
  sites (excludes removed and senesced) on day "x"

- AUDPC:

  Area under the disease progress curve (AUDPC) for the simulation

- lat:

  Latitude value if provided by the `wth` object

- lon:

  Longitude value if provided by the `wth` object

## *a* – Aggregation Details

When *a* is set to `1` the assumption is that that there is no disease
aggregation with new infections occurring at random among the healthy
sites. When *a* is greater than `1` there is aggregation in the disease
occurrence, the pathogen is unable to access the entire population of
healthy sites, which results in disease aggregation. Refer to Savary
\_et al. \_ (2012) for greater detail.

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

Sparks, A. H., P.D. Esker, M. Bates, W. Dall' Acqua, Z. Guo, V. Segovia,
S.D. Silwal, S. Tolos, and K. A. Garrett, 2008. Ecology and Epidemiology
in R: Disease Progress over Time. *The Plant Health Instructor*.
[doi:10.1094/PHI-A-2008-0129-02](https://doi.org/10.1094/PHI-A-2008-0129-02)
.

Madden, L. V., G. Hughes, and F. van den Bosch. 2007. The Study of Plant
Disease Epidemics. American Phytopathological Society, St. Paul, MN.
[doi:10.1094/9780890545058](https://doi.org/10.1094/9780890545058) .

Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario, J.
Modeling and mapping potential epidemics of rice diseases globally.
*Crop Protection*, Volume 34, 2012, Pages 6-17, ISSN 0261-2194
[doi:10.1016/j.cropro.2011.11.009](https://doi.org/10.1016/j.cropro.2011.11.009)
.

## See also

`seir()` is called by the following specific disease modelling
functions:

- [`bacterial_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/bacterial_blight.md),

- [`brown_spot()`](https://adamhsparks.codeberg.page/epicrop/reference/brown_spot.md),

- [`leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/leaf_blast.md),

- [`sheath_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/sheath_blight.md),

- [`tungro()`](https://adamhsparks.codeberg.page/epicrop/reference/tungro.md),

- [`modified_kim_leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/modified_kim_leaf_blast.md),
  and,

- [`modified_kim_sheath_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/modified_kim_sheath_blight.md).

## Author

Adam H. Sparks, <adamhsparks@gmail.com> from original by Robert J.
Hijmans, Rene Pangga and Jorrel Aunario.

## Examples

``` r
# get weather for IRRI Zeigler Experiment Station in wet season 2000
wth <- get_wth(
  lonlat = c(121.25562, 14.6774),
  dates = c("2000-06-30", "2000-12-31")
)

# provide suitable values for brown spot intensity
RcA <-
  cbind(
    c(0L, 20L, 40L, 60L, 80L, 100L, 120L),
    c(0.35, 0.35, 0.35, 0.47, 0.59, 0.71, 1.0)
  )
RcT <-
  cbind(
    c(15L, 20L, 25L, 30L, 35L, 40L),
    c(0, 0.06, 1.0, 0.85, 0.16, 0)
  )
emergence <- "2000-07-15"

seir(
  wth = wth,
  emergence = emergence,
  onset = 20,
  duration = 120,
  rhlim = 90,
  rainlim = 5,
  H0 = 600,
  I0 = 1,
  RcA = RcA,
  RcT = RcT,
  RcOpt = 0.61,
  p = 6,
  i = 19,
  Sx = 100000,
  a = 1,
  RRS = 0.01,
  RRG = 0.1
)
#> Error in seir(wth = wth, emergence = emergence, onset = 20, duration = 120,     rhlim = 90, rainlim = 5, H0 = 600, I0 = 1, RcA = RcA, RcT = RcT,     RcOpt = 0.61, p = 6, i = 19, Sx = 100000, a = 1, RRS = 0.01,     RRG = 0.1): `simple_wetness` must be a Boolean
#> (`TRUE/FALSE`) value.
```
