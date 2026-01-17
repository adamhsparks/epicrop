# Predict Rice Brown Spot Intensity

A helper function to provide sensible defaults for a dynamic mechanistic
simulation of rice brown spot, causal agent *Cochliobolus miyabeanus*.
The model is driven by daily weather data, which can easily be accessed
using
[`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md)
to download weather data from NASA POWER.

## Usage

``` r
brown_spot(wth, emergence, ...)

bs(wth, emergence, ...)
```

## Arguments

- wth:

  Weather data with a daily time-step, normally NASA POWER data from
  [`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md),
  but any [`base::data.frame()`](https://rdrr.io/r/base/data.frame.html)
  object that has been formatted using
  [`format_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/format_wth.md)
  will have the following properly named columns and will work.

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

  Expected date of crop emergence

- ...:

  Additional arguments passed to
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md).

## Value

A
[`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
of disease intensity and infection sites. See
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
for a full description of the column values.

## Details

The model represents site size as 10 mm² of a rice plant's leaf.

Default values for this disease model are derived from Table 2 (Savary
*et al.* 2012).

bs is a shorthand alias for brown_spot.

## Note

Adapted from cropsim package version 0.2.0-5 by Adam H. Sparks,
Department of Primary Industries and Regional Development, WA, AU.
Original model development: Serge Savary & Rene Pangga (IRRI). Original
R implementation by Robert J. Hijmans, Rene Pangga, & Jorrel Aunario
(IRRI).

If the `wth` object provides *LAT* and *LON* columns, these will be
included in the output for mapping purposes. Both values must be
present. These columns are provided by default when using
[`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md).

The optimum temperature for brown spot as presented in Table 2 of Savary
*et al.* 2012 has a typo. The optimal value should be 25 °C, not 20 °C
as shown. The correct value, 25 °C, is used in this implementation.

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

Klomp, A.O., 1977. Early senescence of rice and *Drechslera oryzae* in
the Wageningen polder, Surinam. PhD Thesis, 97p.

Levy, Y. and Cohen, Y., 1980. Sporulation of *Helminthosporium turcicum*
on sweet corn: Effects of temperature and dew period. Canadian Journal
of Plant Pathology 2:65-69.
[doi:10.1080/07060668009501440](https://doi.org/10.1080/07060668009501440)
.

Luo Wei-Hong, 1996. Simulation and measurement of leaf wetness formation
in paddy rice crops. PhD, Wageningen Agricultural University, 87 p.

Padmanabhan, S.Y. and Ganguly, D. 1954. Relation between the age of rice
plant and its susceptibility to *Helminthosporium* and blast disease.
Proceedings of the Indian Academy of Sciences B 29:44-50.

Sarkar, A.K. and Sen Gupta, P.K., 1977. Effect of temperature and
humidity on disease development and sporulation of *Helminthosporium
oryzae* on rice. Indian Phytopathology 30:258-259.

Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario, J.
Modeling and mapping potential epidemics of rice diseases globally. Crop
Protection, Volume 34, 2012, Pages 6-17, ISSN 0261-2194
[doi:10.1016/j.cropro.2011.11.009](https://doi.org/10.1016/j.cropro.2011.11.009)
.

Waggoner. P.E., Horsfall, J.G., and Lukens, R.J. 1972. EPIMAY. A
Simulator of Southern Corn Leaf Blight. Bulletin of the Connecticut
Experiment Station, New Haven, 85p.

## See also

[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)

Other helper functions:
[`bacterial_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/bacterial_blight.md),
[`leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/leaf_blast.md),
[`modified_kim_leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/modified_kim_leaf_blast.md),
[`modified_kim_sheath_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/modified_kim_sheath_blight.md),
[`sheath_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/sheath_blight.md),
[`tungro()`](https://adamhsparks.codeberg.page/epicrop/reference/tungro.md)

## Author

Serge Savary, Ireneo Pangga, Robert Hijmans, Jorrel Khalil Aunario

## Examples

``` r
# get weather for IRRI Zeigler Experiment Station in wet season 2000
wth <- get_wth(
  lonlat = c(121.25562, 14.6774),
  dates = c("2000-06-30", "2000-12-31")
)
bs_sim <- brown_spot(wth, emergence = "2000-07-01")
plot(x = bs_sim$dates, y = bs_sim$intensity, type = "l")

# use shorthand function
bs <- bs(wth, emergence = "2000-07-01")
plot(x = bs$dates, y = bs$intensity, type = "l")
```
