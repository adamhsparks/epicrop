# Rice and Wheat Disease Models

Convenience wrappers around
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
with pre-configured parameter tables for well-characterized crop
diseases. Each function encapsulates parameters from published
epidemiological studies, allowing one-line disease simulations.

`bacterial_blight()` models rice bacterial blight (*Xanthomonas oryzae*
pv. *oryzae*). The disease requires high humidity and warm temperatures
(optimum 25–30°C) and is most severe in the Philippines and Asia-Pacific
region.

`brown_spot()` models rice brown spot (*Bipolaris oryzae*). A seed-borne
fungal disease prevalent in tropical and subtropical regions,
particularly damaging to older rice varieties.

`leaf_blast()` models rice leaf blast (*Magnaporthe oryzae*). The most
economically important rice disease worldwide, favoured by cool (25°C),
wet conditions. Rapid disease progression can cause total crop loss.

`sheath_blight()` models rice sheath blight (*Rhizoctonia solani*). A
soil-borne disease that infects the leaf sheath; can cause catastrophic
damage in high-N fertilized crops.

`tungro()` models rice tungro, a viral disease complex (Tungro
bacilliform and Tungro spherical viruses) transmitted by the insect
vector *Nephotettix* spp. Vector populations increase with temperature
and humidity; disease pressure peaks mid-season.

`modified_kim_leaf_blast()` models rice leaf blast using parameters from
Kim et al. (2015), calibrated for South Korean rice cultivars and
growing conditions. Offers a simplified (DVS-free) alternative to the
standard Savary model.

`modified_kim_sheath_blight()` models rice sheath blight using
parameters from Kim et al. (2015), calibrated for South Korean rice
cultivars. Uses slightly higher humidity threshold (95%) reflecting
Korean monsoon climate.

`leaf_rust()` models wheat leaf rust (*Puccinia triticina*). A major
wheat pathogen globally, favoured by moderate temperatures (15–25°C) and
high humidity. Uses DVS-based age driver to account for vernalization
effects.

`s_tritici_blotch()` models wheat Septoria tritici blotch (*Zymoseptoria
tritici*), a major wheat pathogen in temperate regions. Features lesion
expansion (direct H→I pathway) allowing rapid canopy senescence.
DVS-based modelling accounts for developmental susceptibility.

`bb()` is an alias for `bacterial_blight()`.

`bs()` is an alias for `brown_spot()`.

`lb()` is an alias for `leaf_blast()`.

`sb()` is an alias for `sheath_blight()`.

`tg()` is an alias for `tungro()`.

`mk_lb()` is an alias for `modified_kim_leaf_blast()`.

`mk_sb()` is an alias for `modified_kim_sheath_blight()`.

`lr()` is an alias for `leaf_rust()`.

`brown_rust()` is an alias for `leaf_rust()`.

`stb()` is an alias for `s_tritici_blotch()`.

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
  RRPSN = NULL,
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
  RRPSN = NULL,
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
  RRPSN = NULL,
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
  RRPSN = NULL,
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
  RRPSN = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
)
```

## Arguments

- wth:

  A `data.frame` of class `epicrop.wth` from
  [`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md)
  or
  [`format_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/format_wth.md)
  containing daily weather data with columns:

  **Required Columns:**

  |          |                                  |
  |----------|----------------------------------|
  | Column   | Description                      |
  | YYYYMMDD | Date (ISO 8601: YYYYMMDD)        |
  | DOY      | Day of year (1–366)              |
  | TEMP     | Mean daily temperature (°C)      |
  | RHUM     | Mean daily relative humidity (%) |
  | RAIN     | Daily rainfall (mm)              |

  **Optional Columns (for advanced features):**

  |  |  |
  |----|----|
  | Column | Purpose |
  | TMIN | Minimum daily temperature (°C); required if `simple_wetness = FALSE` |
  | TMAX | Maximum daily temperature (°C); required if `simple_wetness = FALSE` |
  | LAT | Latitude; required if `simple_wetness = FALSE` |
  | LON | Longitude; included in output if present |

- emergence:

  Plant emergence (transplanting for rice) date in `YYYY-MM-DD` format
  (character).

- RcA:

  Optional custom age modifier table (2-column numeric matrix).
  Overrides the published default for this disease.

- RcT:

  Optional custom temperature modifier table (2-column numeric matrix).
  Overrides the published default for this disease.

- RRS:

  Optional custom relative senescence rate (scalar numeric, vector,
  function, or matrix). Overrides the published default. Applies to rice
  disease models only.

- RRG:

  Optional custom relative growth rate (scalar numeric, vector, or
  function). Overrides the published default.

- rhlim:

  Optional override for relative humidity threshold (%) for leaf
  wetness. Defaults to disease-specific value (typically 90).

- rainlim:

  Optional override for rainfall threshold (mm) for leaf wetness.
  Defaults to disease-specific value (typically 5).

- ...:

  Additional parameters passed to
  [`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md),
  allowing full customization of any parameter (onset, duration, H0, I0,
  p, i, Sx, a, simple_wetness, etc.).

- RRPSN:

  Optional custom relative photoperiod-adjusted senescence rate (scalar
  numeric, vector, function, or matrix). Overrides the published
  default. Applies to wheat disease models only.

## Value

A
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
of class `epicrop.sim` with disease progression over time. See
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
for output details.

## Details

All functions support parameter override via `...` arguments, enabling
sensitivity analysis and custom cultivar/environment modelling.

## Available Models

**Rice Diseases (Savary *et al.* 2012):**

- `bacterial_blight()` – Bacterial blight (*Xanthomonas oryzae* pv.
  *oryzae*)

- `brown_spot()` – Brown spot (*Bipolaris oryzae*)

- `leaf_blast()` – Leaf blast (*Magnaporthe oryzae*)

- `sheath_blight()` – Sheath blight (*Rhizoctonia solani*)

- `tungro()` – Tungro (viral, vector-borne)

**Rice Diseases (Kim *et al.* 2015 modifications):**

- `modified_kim_leaf_blast()` – Cultivar-adjusted leaf blast

- `modified_kim_sheath_blight()` – Cultivar-adjusted sheath blight

**Wheat Diseases (Savary *et al.* 2015):**

- `leaf_rust()` – Leaf rust (*Puccinia triticina*)

- `s_tritici_blotch()` – Septoria tritici blotch (*Zymoseptoria
  tritici*)

## Parameter Sources

**Rice models** use baseline parameters from:

- Savary *et al.* (2012) – Original parameterisation from literature

- Kim *et al.* (2015) – Versions calibrated for Korean rice cultivars

**Wheat models** use:

- Savary *et al.* (2015) – European calibration

## TMIN/TMAX Details

When `simple_wetness = FALSE`, the function calculates hourly leaf
wetness for 24 hours using `TMIN`, `TMAX`, `LAT`, and `DOY` in the `wth`
object. The `rhlim` and `rainlim` thresholds determine wet hours. A
leaf-wetness modifier (`RcW`) converts wet hours to a 0–1 *Rc* scale.

When `simple_wetness = TRUE`, this calculation is skipped; leaf wetness
is binary (0 or 1) based solely on daily `RHUM` and `RAIN`.

## LAT/LON Details

If the `wth` object contains `LAT` and `LON` columns, they are appended
to each row of the output for mapping or spatial analysis. Both must be
present to be included. By default,
[`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md)
and
[`format_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/format_wth.md)
provide these.

## References

Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario, J.
2012. Modeling and mapping potential epidemics of rice diseases
globally. *Crop Protection*, 34: 6–17.
[doi:10.1016/j.cropro.2011.11.009](https://doi.org/10.1016/j.cropro.2011.11.009)

Kim, K. H., Cho, J., Lee, Y. H., and Lee, W. S. 2015. Predicting
potential epidemics of rice leaf blast and sheath blight in South Korea.
*Agricultural and Forest Meteorology*, 203: 191–207.
[doi:10.1016/j.agrformet.2015.01.011](https://doi.org/10.1016/j.agrformet.2015.01.011)

Savary, S., Stetkiewicz, S., Brun, F., and Willocquet, L. 2015.
Modelling and mapping potential epidemics of wheat diseases. *European
Journal of Plant Pathology*, 142: 771–790.
[doi:10.1007/s10658-015-0650-7](https://doi.org/10.1007/s10658-015-0650-7)

## Author

Adam H. Sparks, <adamhsparks@gmail.com>

## Examples

``` r
# Fetch weather data for a location in the Philippines
wth <- get_wth(
  lonlat = c(121.255669, 14.16742),
  dates = "2000-06-30",
  duration = 120L
)

# Run bacterial blight model with default parameters (Savary et al. 2012)
bb_sim <- bacterial_blight(
  wth = wth,
  emergence = "2000-06-30"
)

# Inspect results
head(bb_sim)
#>    simday      dates    sites latent infectious removed senesced rateinf  rlex
#>     <int>     <IDat>    <num>  <num>      <num>   <num>    <num>   <num> <num>
#> 1:      1 2000-06-30 100.0000      0          0       0 1.000000       0     0
#> 2:      2 2000-07-01 108.6875      0          0       0 2.086875       0     0
#> 3:      3 2000-07-02 118.1002      0          0       0 3.267877       0     0
#> 4:      4 2000-07-03 128.2934      0          0       0 4.550811       0     0
#> 5:      5 2000-07-04 139.3254      0          0       0 5.944065       0     0
#> 6:      6 2000-07-05 151.2581      0          0       0 7.456646       0     0
#>    rtransfer rremoved  rgrowth rsenesced diseased intensity      lat      lon
#>        <num>    <num>    <num>     <num>    <num>     <num>    <num>    <num>
#> 1:         0        0  9.68750  1.000000        0         0 14.16742 121.2557
#> 2:         0        0 10.49959  1.086875        0         0 14.16742 121.2557
#> 3:         0        0 11.37416  1.181002        0         0 14.16742 121.2557
#> 4:         0        0 12.31499  1.282934        0         0 14.16742 121.2557
#> 5:         0        0 13.32593  1.393254        0         0 14.16742 121.2557
#> 6:         0        0 14.41084  1.512581        0         0 14.16742 121.2557
plot(bb_sim$simday, bb_sim$intensity, type = "l",
     main = "Rice Bacterial Blight Progression",
     xlab = "Days", ylab = "Disease Intensity")


# Simulate brown spot for a standard rice season
wth <- get_wth(
  lonlat = c(121.255669, 14.16742),
  dates = "2000-06-30",
  duration = 120L
)

bs_sim <- brown_spot(
  wth = wth,
  emergence = "2000-06-30"
)

# Brown spot develops slowly compared to leaf blast
plot(bs_sim$simday, bs_sim$intensity, type = "l",
     main = "Rice Brown Spot Progression",
     xlab = "Days After Emergence", ylab = "Disease Intensity")

# Simulate leaf blast under favourable conditions
wth <- get_wth(
  lonlat = c(121.255669, 14.16742),
  dates = "2000-06-30",
  duration = 120L
)

lb_sim <- leaf_blast(
  wth = wth,
  emergence = "2000-06-30"
)

# Leaf blast shows rapid increase in intensity after onset
plot(lb_sim$simday, lb_sim$intensity, type = "l", ylim = c(0, 1),
     main = "Rice Leaf Blast Progression",
     xlab = "Days After Emergence", ylab = "Disease Intensity")

wth <- get_wth(
  lonlat = c(121.255669, 14.16742),
  dates = "2000-06-30",
  duration = 120L
)

sb_sim <- sheath_blight(
  wth = wth,
  emergence = "2000-06-30"
)

# Sheath blight has long infectious period and delayed onset
plot(sb_sim$simday, sb_sim$intensity, type = "l", ylim = c(0, 1),
     main = "Rice Sheath Blight Progression",
     xlab = "Days After Emergence", ylab = "Disease Intensity")

# Simulate tungro (vector-borne viral disease)
wth <- get_wth(
  lonlat = c(121.255669, 14.16742),
  dates = "2000-06-30",
  duration = 120L
)

tg_sim <- tungro(
  wth = wth,
  emergence = "2000-06-30"
)

# Tungro shows plateau phase (low Sx due to limited vector population)
plot(tg_sim$simday, tg_sim$intensity, type = "l",
     main = "Rice Tungro Progression",
     xlab = "Days After Emergence", ylab = "Disease Intensity")

# Simulate using Kim et al. (2015) cultivar-adjusted parameters
wth <- get_wth(
  lonlat = c(126.9780, 37.5665),  # Seoul, South Korea
  dates = "2000-06-01",
  duration = 77L
)

kim_lb <- modified_kim_leaf_blast(
  wth = wth,
  emergence = "2000-06-01"
)

# Kim version has shorter duration and higher Sx (larger canopy model)
plot(kim_lb$simday, kim_lb$intensity, type = "l",
     main = "Rice Leaf Blast (Kim et al. 2015)",
     xlab = "Days", ylab = "Intensity")

# Simulate sheath blight with Kim et al. (2015) parameters
wth <- get_wth(
  lonlat = c(126.9780, 37.5665),
  dates = "2000-06-01",
  duration = 88L
)

kim_sb <- modified_kim_sheath_blight(
  wth = wth,
  emergence = "2000-06-01"
)

plot(kim_sb$simday, kim_sb$intensity, type = "l",
     main = "Rice Sheath Blight (Kim et al. 2015)",
     xlab = "Days", ylab = "Intensity")

# Simulate wheat leaf rust with DVS (development stage) modelling
wth <- get_wth(
  lonlat = c(8.5, 50.1),  # Example: France
  dates = "2000-10-15",   # Winter wheat sowing
  duration = 240L         # Full growing season
)

lr_sim <- leaf_rust(
  wth = wth,
  emergence = "2000-10-15"
)

# Leaf rust shows gradual progression across growing season
plot(lr_sim$simday, lr_sim$intensity, type = "l",
     main = "Wheat Leaf Rust Progression (DVS-based)",
     xlab = "Days After Sowing", ylab = "Disease Intensity")

# Example: Septoria tritici blotch - compare lesion expansion effect

# Fetch weather for a wheat-growing region (France, Oct-May season)
wth <- get_wth(
  lonlat = c(8.5, 50.1),
  dates = "2000-10-15",
  duration = 240L
)

stb <- s_tritici_blotch(
  wth = wth,
  emergence = "2000-10-15"
)

# Create single plot with both lines for comparison
plot(stb$simday, stb$intensity,
     type = "l", col = "black", lty = 1,
     main = "Wheat Septoria tritici Blotch",
     xlab = "Days After Sowing",
     ylab = "Disease Intensity")
```
