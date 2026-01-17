# Predict Rice Bacterial Blight Intensity

A dynamic mechanistic simulation of bacterial blight disease of rice,
causal agent *Xanthomonas oryzae* pv. *oryzae*. The model is driven by
daily weather data, which can easily be accessed using
[`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md)
to download weather data from NASA POWER.

## Usage

``` r
bacterial_blight(wth, emergence, ...)

bb(wth, emergence, ...)
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

The model represents site size as 1 rice plant's leaf.

Default values for this disease model are derived from Table 2 (Savary
*et al.* 2012).

bb is a shorthand alias for bacterial_blight.

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

Adhikari, T.B., 1991. Effects of rice genotype and environment on
bacterial blight progression. PhD Thesis, University of the Philippines
at Los Baños, 143 p.

Baw A. and Mew, T.W., 1988. Scoring systems for evaluating rice varietal
resistance to bacterial blight (BB): lesion size by growth stage.
International Rice Research Newsletter 13:10-11.

Horino, O., Mew, T.W., Yamada, T., 1982. The effect of temperature on
the development of bacterial leaf blight on rice. Annals of the
Phytopathological Society of Japan 48: 72-75.

Luo Wei-Hong, 1996. Simulation and measurement of leaf wetness formation
in paddy rice crops. PhD, Wageningen Agricultural University, 87 p.

Medalla, E. 1992. Characterization of resistance of IR cultivars to two
races of *Xanthomonas oryzae* pv. *oryzae*. Unpublished M.S. Thesis,
University of the Philippines at Los Baños, 81 p.

Nayak, P., Suriya Rao, A.V., Chakrabarti, N.K., 1987. Components of
resistance to bacterial blight disease of rice. Journal of
Phytopathology 119:312-318. DOI:
[doi:10.1111/j.1439-0434.1987.tb04402.x](https://doi.org/10.1111/j.1439-0434.1987.tb04402.x)
.

Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario, J.
Modeling and mapping potential epidemics of rice diseases globally. Crop
Protection, Volume 34, 2012, Pages 6-17, ISSN 0261-2194 DOI:
[doi:10.1016/j.cropro.2011.11.009](https://doi.org/10.1016/j.cropro.2011.11.009)
.

## See also

[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)

Other helper functions:
[`brown_spot()`](https://adamhsparks.codeberg.page/epicrop/reference/brown_spot.md),
[`leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/leaf_blast.md),
[`modified_kim_leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/modified_kim_leaf_blast.md),
[`modified_kim_sheath_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/modified_kim_sheath_blight.md),
[`sheath_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/sheath_blight.md),
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
bb_sim <- bacterial_blight(wth, emergence = "2000-07-01")
plot(x = bb_sim$dates, y = bb_sim$intensity, type = "l")

# use shorthand function
bb <- bb(wth, emergence = "2000-07-01")
plot(x = bb$dates, y = bb$intensity, type = "l")
```
