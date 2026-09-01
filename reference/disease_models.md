# Rice and Wheat Disease Models

Helper functions with pre-loaded parameter tables for optimal
performance. Custom parameter tables can be provided to override
defaults.

## Usage

``` r
bacterial_blight(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
)

brown_spot(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
)

leaf_blast(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
)

sheath_blight(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
)

tungro(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
)

modified_kim_leaf_blast(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
)

modified_kim_sheath_blight(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
)

leaf_rust(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
)

s_tritici_blotch(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
)

bb(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
)

bs(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
)

lb(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
)

sb(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
)

tg(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
)

mk_lb(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
)

mk_sb(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
)

lr(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
)

brown_rust(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
)

stb(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
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
  | *TMIN* | **Optional** Minimum daily temperature (degrees C), see TMIN/TMAX details for more |
  | *TMAX* | **Optional** Maximum daily temperature (degrees C), see TMIN/TMAX details for more |
  | *LAT* | **Optional** latitude of weather observation, see LAT/LON details for more |
  | *LON* | **Optional** longitude of weather observation, see LAT/LON details for more |

- emergence:

  Expected date of plant emergence (or transplanting for rice) entered
  in `YYYY-MM-DD` format (character).

- RcA:

  Optional custom age modifier table (2-column matrix).

- RcT:

  Optional custom temperature modifier table (2-column matrix).

- RRS:

  Optional custom senescence rate (scalar, vector, or function).

- RRG:

  Optional custom growth rate (scalar, vector, or table).

- rhlim:

  relative humidity value threshold to decide whether leaves are wet or
  not (numeric). Described in Table 1 of Savary *et al.* 2012. Savary
  *et al.* 2012 used `90`. Defaults to 90.

- rainlim:

  rainfall amount (mm) threshold to decide whether leaves are wet or not
  (numeric). Described in Table 1 of Savary *et al.* 2012. Savary *et
  al.* 2012 used `5`. Defaults to 5.

- ...:

  Other parameters passed to
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
  allowing the user to override the default values from published
  papers.

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

Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario, J.
(2012). Modeling and mapping potential epidemics of rice diseases
globally. *Crop Protection*, 34: 6-17.
[doi:10.1016/j.cropro.2011.11.009](https://doi.org/10.1016/j.cropro.2011.11.009)

Kim, K. H., Cho, J., Lee, Y. H., & Lee, W. S. (2015). Predicting
potential epidemics of rice leaf blast and sheath blight in South Korea.
*Agricultural and Forest Meteorology*, 203: 191-207.
[doi:10.1016/j.agrformet.2015.01.011](https://doi.org/10.1016/j.agrformet.2015.01.011)

Savary, S., Stetkiewicz, S., Brun, F., & Willocquet, L. (2015).
Modelling and mapping potential epidemics of wheat diseases. *European
Journal of Plant Pathology*, 142: 771-790.
[doi:10.1007/s10658-015-0650-7](https://doi.org/10.1007/s10658-015-0650-7)

## Author

Adam H. Sparks, <adamhsparks@gmail.com>
