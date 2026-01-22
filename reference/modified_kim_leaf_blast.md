# Predict Rice Leaf Blast Intensity Using Modified Kim Model

A helper function to provide values used by Kim \_et al. 2015 for a
dynamic mechanistic simulation of leaf blast disease of rice, causal
agent *Magnaporthe oryzae*. The model is driven by daily weather data,
which can easily be accessed using
[`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md)
to download weather data from NASA POWER.

## Usage

``` r
modified_kim_leaf_blast(wth, emergence, ...)

mk_lb(wth, emergence, ...)
```

## Source

Kim, Kwang-Hyung (2015). Modified EPIRICE Ref.. figshare. Dataset.
[doi:10.6084/m9.figshare.1515884.v1](https://doi.org/10.6084/m9.figshare.1515884.v1)

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

The model represents site size as 44 mm² of a rice plant's leaf.

## Note

The original code for this function from Kim \_et al. 2015 had an
incorrect matrix for the `RcW` modifier. The original code had the
following values:

- Hours: `c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L)`

- Modifier: `c(0, 0.24, 0.41, 0.68, 0.94, 0.97, 1, 1, 1, 1 ,1)` These
  have been corrected to:

- Hours: `c(4L, 6L, 8L, 10L, 12L, 14L, 16L, 18L, 20L, 22L, 24L)`

- Modifier: `c(0, 0.24, 0.41, 0.68, 0.94, 0.97, 1, 1, 1, 1, 1)`
  providing modifiers for hours 4-24, inclusive rather than stopping at
  10 hours.

## Function Provenance and Modifications

This function is adapted from Kim *et al.* by Adam H. Sparks, Curtin
University, with assistance from Kwang-Hyung Kim, APEC Climate Center,
12 Centum7-ro, Haeundae-gu, Busan 612-020, South Korea. Original model
development: Serge Savary & Rene Pangga (IRRI). Original R
implementation by Robert J. Hijmans, Rene Pangga, & Jorrel Aunario
(IRRI).

While the paper by Kim *et al.* 2015 indicates that the leaf wetness
duration changed with time rather than being 0 or 1 as in Savary *et
al.* 2012, the original code available does not implement this. epicrop
offers the option to do this by setting `simple_wetness = FALSE`, but
the results may differ from the published values if this is used.
Further changes were made to the length of the growing season where the
example code ran for 100 days, the paper ran for 77. This function has
been written to match the paper's 77 day growing season duration and RcA
and RRG have been modified accordingly.

If the `wth` object provides *LAT* and *LON* columns, these will be
included in the output for mapping purposes. Both values must be
present. These columns are provided by default when using
[`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md).

The optimum temperature for leaf blast as presented in Table 2 of Savary
*et al.* 2012 has a typo. The optimal value should be 20 °C, not 25 °C
as shown. The correct value, 20 °C, is used in this implementation.

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

Kim, K. H., Cho, J., Lee, Y. H., & Lee, W. S. (2015). Predicting
potential epidemics of rice leaf blast and sheath blight in South Korea
under the RCP 4.5 and RCP 8.5 climate change scenarios using a rice
disease epidemiology model, EPIRICE. Agricultural and forest
meteorology, 203, 191-207.
[doi:10.1016/j.agrformet.2015.01.011](https://doi.org/10.1016/j.agrformet.2015.01.011)

El Refaei, M.I., 1977. Epidemiology of rice blast disease in the tropics
with special reference to the leaf wetness in relation to disease
development. PhD Thesis, Indian Agricultural Research Institute, New
Delhi, India. 195 p.

Hemmi, T., Abe, T., Ikaya, J., and Inoue, Y. 1936. Studies on the rice
blast disease. IV. Relation of the environment to the development of
blast disease and physiologic specialization in the rice blast fungus.
Materials for Rural Improvement, Department of Agriculture and Forestry,
Japan No. 105, 145p.

Hwang, B.K., Koh, Y.J., Chung, H.S., 1987. Effects of adult-plant
resistance on blast severity and yield of rice. Plant Disease
71:1035-1038. DOI:
[doi:10.1094/PD-71-1035](https://doi.org/10.1094/PD-71-1035) .

Kato, H. and Kozaka, T., 1974. Effect of temperature on lesion
enlargement and sporulation of *Pyricularia oryzae* in rice leaves.
Phytopathology 64:828-830. DOI:
[doi:10.1094/Phyto-64-828](https://doi.org/10.1094/Phyto-64-828) .

Wei-Hong, L. 1996. Simulation and measurement of leaf wetness formation
in paddy rice crops. PhD Thesis, Wageningen Agricultural University, 87
p.

Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario, J.
Modeling and mapping potential epidemics of rice diseases globally. Crop
Protection, Volume 34, 2012, Pages 6-17, ISSN 0261-2194 DOI:
[doi:10.1016/j.cropro.2011.11.009](https://doi.org/10.1016/j.cropro.2011.11.009)
.

Torres, C.Q., 1986. Effect of plant age on the expression of resistance
to *Pyricularia oryzae* Cav. in upland rice varieties. PhD Thesis,
University of the Philippines at Los Baños, 82 p.

## See also

[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)

Other helper functions:
[`bacterial_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/bacterial_blight.md),
[`brown_spot()`](https://adamhsparks.codeberg.page/epicrop/reference/brown_spot.md),
[`leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/leaf_blast.md),
[`modified_kim_sheath_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/modified_kim_sheath_blight.md),
[`sheath_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/sheath_blight.md),
[`tungro()`](https://adamhsparks.codeberg.page/epicrop/reference/tungro.md)

## Author

Kwang-Hyung Kim, Serge Savary, Ireneo Pangga, Robert Hijmans, Jorrel
Khalil Aunario, and Adam H. Sparks

## Examples

``` r

# get weather for IRRI Zeigler Experiment Station in wet season 2000
wth <- get_wth(
  lonlat = c(121.25562, 14.6774),
  dates = c("2000-05-01", "2000-09-01")
)

# use 25 May as transplanting date (`emergence`) as in Kim et al. 2015
lb_sim <- modified_kim_leaf_blast(wth, emergence = "2000-05-25")
plot(x = lb_sim$dates, y = lb_sim$intensity, type = "l")

# use shorthand function
mk_lb <- mk_lb(wth, emergence = "2000-07-01")
#> Error in .determine_run_window(wth, emergence, duration): Insufficient weather data for simulation window
#> ✖ Need 77 days, have 64
#> ℹ Window: 2000-06-30 to 2000-09-14
#> ℹ Available: 2000-04-30 to 2000-09-01
plot(x = mk_lb$dates, y = mk_lb$intensity, type = "l")
#> Error in mk_lb$dates: object of type 'closure' is not subsettable
```
