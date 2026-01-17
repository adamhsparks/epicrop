# Predict Rice Sheath Blight Intensity

A helper function to provide sensible defaults for a dynamic mechanistic
simulation of sheath blight disease of rice, causal agent *Rhizoctonia
solani* AG1-1A Kühn. The model is driven by daily weather data, which
can easily be accessed using
[`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md)
to download weather data from NASA POWER.

## Usage

``` r
sheath_blight(wth, emergence, ...)

sb(wth, emergence, ...)
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

The model represents site size as 1 rice plant's tiller.

Default values for this disease model are derived from Table 2 (Savary
*et al.* 2012).

`sb()` is a shorthand alias for `sheath_blight()`.

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

Castilla, N.P., Leano, R.M., Elazegui, F.A., Teng, P.S., Savary, S.,
1996. Effects of plant contacts, inoculation pattern, leaf wetness
regime, and nitrogen supply on inoculum efficiency in rice sheath
blight. Journal of Phytopathology 144:187-192.

Gross, M.K., Santini, J.B., Tikhonova, I. and Latin, R. 1998. The
influence of temperature and leaf wetness duration on infection of
perennial ryegrass by *Rhizoctonia solani*. Plant Disease 82:1012-1016.
[doi:10.1094/PDIS.1998.82.9.1012](https://doi.org/10.1094/PDIS.1998.82.9.1012)
.

Hashiba, T. and Ijiri, T., 1989. Estimation of yield loss and
computerized forecasting system (BLIGHTAS) for rice sheath blight
disease. International Symposium on Tropical Agricultural Research: Crop
losses due to disease outbreaks in the tropics and countermeasures.
Tropical Agricultural Research Series (Japan) No. 22 pp. 163-171.

Savary, S., Willocquet, L., Teng, P.S., 1997. Modelling sheath blight
epidemics on rice tillers. Agricultural Systems 55:359-384.
[doi:10.1016/S0308-521X(97)00014-0](https://doi.org/10.1016/S0308-521X%2897%2900014-0)
.

Savary, S., Castilla, N.P., Willocquet, L. 2001. Analysis of the spatio-
temporal structure of rice sheath blight epidemics in a farmer's field.
Plant Pathology 50:53-68.
[doi:10.1046/j.1365-3059.2001.00531.x](https://doi.org/10.1046/j.1365-3059.2001.00531.x)
.

Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario, J.
Modeling and mapping potential epidemics of rice diseases globally. Crop
Protection, Volume 34, 2012, Pages 6-17, ISSN 0261-2194
[doi:10.1016/j.cropro.2011.11.009](https://doi.org/10.1016/j.cropro.2011.11.009)
.

Sharma, N.R., Teng, P.S., Olivares, F.M., 1990. Effect of rice growth
stage on sheath blight (ShB) development and yield loss. International
Rice Research Newsletter 15:19-20.

Tu, C.C., Chang, Y.C., Wang, C.W., 1979. Studies on the ecology of
*Rhizoctonia solani*, the causal organism of rice sheath blight.
National Science Council Monthly, ROC 7:1208-1219.

## See also

[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)

Other helper functions:
[`bacterial_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/bacterial_blight.md),
[`brown_spot()`](https://adamhsparks.codeberg.page/epicrop/reference/brown_spot.md),
[`leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/leaf_blast.md),
[`modified_kim_leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/modified_kim_leaf_blast.md),
[`modified_kim_sheath_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/modified_kim_sheath_blight.md),
[`tungro()`](https://adamhsparks.codeberg.page/epicrop/reference/tungro.md)

## Author

Serge Savary, Ireneo Pangga, Robert Hijmans, Jorrel Khalil Aunario, and
Adam H. Sparks

## Examples

``` r
# get weather for IRRI Zeigler Experiment Station in wet season 2000
wth <- get_wth(
  lonlat = c(121.25562, 14.6774),
  dates = c("2000-06-30", "2000-12-31")
)
sb_sim <- sheath_blight(wth, emergence = "2000-07-01")
plot(x = sb_sim$dates, y = sb_sim$intensity, type = "l")

# use shorthand function
sb <- sb(wth, emergence = "2000-07-01")
plot(x = sb$dates, y = sb$intensity, type = "l")
```
