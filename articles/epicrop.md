# epicrop

### Introduction to {epicrop}

{epicrop} provides an R package of the ‘EPIRICE’ model as described in
(Savary et al. 2012), the modified EPIRICE model as described in (Kim et
al. 2015), the ‘EPIWHEAT’ model as described in (Savary et al. 2015) and
a generic SEIR model function,
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md),
for modelling crop disease epidemics. The model uses daily weather data
to estimate disease intensity. A function,
[`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md),
is provided to simplify downloading weather data via the
[{nasapower}](https://cran.r-project.org/package=nasapower) package
(Sparks 2018) and predict disease intensity of five rice diseases using
a generic SEIR model (Zadoks 1971) function,
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md).

For ‘EPIRICE’, default values derived from the literature suitable for
modelling unmanaged disease intensity of five rice diseases, bacterial
blight
([`bacterial_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md));
brown spot
([`brown_spot()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md));
leaf blast
([`leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md));
sheath blight
([`sheath_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md))
and tungro
([`tungro()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md))
and two modified by Kim *et al.* (2015),
([`modified_kim_leaf_blast()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
and `helper_modified_kim_sheath_blight()`), are provided. The modified
Kim versions provide leaf blast and sheath blight models that include
additional weather parameters and modified equations to better reflect
disease progress under certain environmental conditions on the Korean
Peninsula. The ‘EPIWHEAT’ model includes two wheat diseases, leaf rust
([`leaf_rust()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md))
and septoria tritici blotch
([`s_tritici_blotch()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)),
with default values derived from the literature suitable for modelling
unmanaged disease intensity of these diseases.

Using the package functions is designed to be straightforward for
modelling rice disease risks, but flexible enough to accommodate other
pathosystems using the
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
function. If you are interested in modelling other pathosystems, please
refer to (2012) for the development of the parameters that were used for
the rice diseases as derived from the existing literature and are
implemented in the individual disease model functions.

#### Getting started

Load the library.

``` r

library(epicrop)
```

#### Get weather data

The most simple way to use the model is to download weather data from
NASA POWER using
[`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md),
which provides the data in a format suitable for use in the model and is
freely available. See the help file for `naspower::get_power()` for more
details of this functionality and details on the data (Sparks 2018).

``` r

# Fetch weather for year 2000 season at the IRRI Zeigler Experiment Station
wth <- get_wth(
  lonlat = c(121.25562, 14.6774),
  dates = c("2000-01-01", "2000-12-31")
)

wth
```

    ## Key: <YYYYMMDD>
    ##        YYYYMMDD   DOY  TEMP  TMIN  TMAX  RHUM  RAIN     LAT      LON
    ##          <IDat> <int> <num> <num> <num> <num> <num>   <num>    <num>
    ##   1: 2000-01-01     1 24.38 22.85 27.46 91.25 14.93 14.6774 121.2556
    ##   2: 2000-01-02     2 24.28 22.68 27.42 90.88  6.96 14.6774 121.2556
    ##   3: 2000-01-03     3 23.82 22.17 26.96 88.36  2.28 14.6774 121.2556
    ##   4: 2000-01-04     4 23.68 21.90 27.14 88.00  0.87 14.6774 121.2556
    ##   5: 2000-01-05     5 24.11 21.54 28.18 88.12  0.43 14.6774 121.2556
    ##  ---                                                                
    ## 362: 2000-12-27   362 24.46 22.90 26.28 92.49 21.15 14.6774 121.2556
    ## 363: 2000-12-28   363 24.64 23.32 27.28 91.92  6.01 14.6774 121.2556
    ## 364: 2000-12-29   364 24.58 22.51 27.90 90.79  5.49 14.6774 121.2556
    ## 365: 2000-12-30   365 25.31 22.84 28.84 86.25  2.07 14.6774 121.2556
    ## 366: 2000-12-31   366 24.47 21.63 28.63 87.83  3.45 14.6774 121.2556

#### Predict bacterial blight

All of the helper family of functions work in exactly the same manner.
You provide them with weather data and an emergence date, that falls
within the weather data provided, and they will return a data frame of
disease intensity over the season and other values associated with the
model. See the help file for
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
for more on the values returned.

``` r

# Predict bacterial blight intensity for the year 2000 wet season at IRRI
bb_wet <- bacterial_blight(wth, emergence = "2000-07-01")
```

``` r

summary(bb_wet)
```

    ##      simday           dates                sites            latent      
    ##  Min.   :  1.00   Min.   :2000-07-01   Min.   :  0.00   Min.   : 0.000  
    ##  1st Qu.: 30.75   1st Qu.:2000-07-30   1st Qu.: 16.28   1st Qu.: 0.000  
    ##  Median : 60.50   Median :2000-08-29   Median : 64.21   Median : 1.000  
    ##  Mean   : 60.50   Mean   :2000-08-29   Mean   : 57.02   Mean   : 3.744  
    ##  3rd Qu.: 90.25   3rd Qu.:2000-09-28   3rd Qu.: 98.22   3rd Qu.: 5.080  
    ##  Max.   :120.00   Max.   :2000-10-28   Max.   :100.00   Max.   :18.014  
    ##    infectious        removed         senesced        rateinf            rlex  
    ##  Min.   : 0.000   Min.   : 0.00   Min.   : 0.00   Min.   :0.0000   Min.   :0  
    ##  1st Qu.: 1.000   1st Qu.: 0.00   1st Qu.: 0.00   1st Qu.:0.0000   1st Qu.:0  
    ##  Median : 6.838   Median : 1.00   Median : 1.00   Median :0.0000   Median :0  
    ##  Mean   :17.062   Mean   :12.88   Mean   :13.39   Mean   :0.5348   Mean   :0  
    ##  3rd Qu.:34.146   3rd Qu.:19.56   3rd Qu.:21.82   3rd Qu.:0.6962   3rd Qu.:0  
    ##  Max.   :53.168   Max.   :61.49   Max.   :62.04   Max.   :4.0939   Max.   :0  
    ##    rtransfer         rremoved         rgrowth    rsenesced         diseased    
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :0   Min.   :0.0000   Min.   : 0.00  
    ##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0   1st Qu.:0.0000   1st Qu.: 1.78  
    ##  Median :0.0000   Median :0.0000   Median :0   Median :0.0000   Median :37.14  
    ##  Mean   :0.5348   Mean   :0.5125   Mean   :0   Mean   :0.5170   Mean   :33.68  
    ##  3rd Qu.:0.6962   3rd Qu.:0.6962   3rd Qu.:0   3rd Qu.:0.6962   3rd Qu.:64.18  
    ##  Max.   :4.0939   Max.   :4.0939   Max.   :0   Max.   :4.0939   Max.   :64.69  
    ##    intensity           lat             lon       
    ##  Min.   :0.0000   Min.   :14.68   Min.   :121.3  
    ##  1st Qu.:0.0178   1st Qu.:14.68   1st Qu.:121.3  
    ##  Median :0.3602   Median :14.68   Median :121.3  
    ##  Mean   :0.4158   Mean   :14.68   Mean   :121.3  
    ##  3rd Qu.:0.7351   3rd Qu.:14.68   3rd Qu.:121.3  
    ##  Max.   :1.0000   Max.   :14.68   Max.   :121.3

### Plotting using {ggplot2}

The data are in a wide format by default and need to be converted to
long format for use in {ggplot2} if you wish to plot more than one
variable at a time.

``` r

library(ggplot2)
library(tidyr)
```

#### Wet season sites

The model records the number of sites for each bin daily; this can be
graphed as follows.

``` r

dat <- pivot_longer(
  bb_wet,
  cols = c("diseased", "removed", "latent", "infectious"),
  names_to = "site",
  values_to = "value"
)

ggplot(data = dat,
       aes(
         x = dates,
         y = value,
         shape = site,
         linetype = site
       )) +
  labs(y = "Sites",
       x = "Date") +
  geom_line(aes(group = site, colour = site)) +
  geom_point(aes(colour = site)) +
  theme_classic()
```

![Site states over time for bacterial blight. Results for wet season
year 2000 at IRRI Zeigler Experiment Station shown. Weather data used to
run the model were obtained from the NASA Langley Research Center POWER
Project funded through the NASA Earth Science Directorate Applied
Science Program.](wet_plot_sites-1.png)

Site states over time for bacterial blight. Results for wet season year
2000 at IRRI Zeigler Experiment Station shown. Weather data used to run
the model were obtained from the NASA Langley Research Center POWER
Project funded through the NASA Earth Science Directorate Applied
Science Program.

#### Wet season intensity

Plotting intensity over time does not require any data manipulation.

``` r

ggplot(data = bb_wet,
       aes(x = dates,
           y = intensity * 100)) +
  labs(y = "Intensity (%)",
       x = "Date") +
  geom_line() +
  geom_point() +
  theme_classic()
```

![Wet season disease intensity over time for bacterial blight. Results
for wet season year 2000 at IRRI Zeigler Experiment Station shown.
Weather data used to run the model were obtained from the NASA Langley
Research Center POWER Project funded through the NASA Earth Science
Directorate Applied Science Program.](wet_plot_intensity-1.png)

Wet season disease intensity over time for bacterial blight. Results for
wet season year 2000 at IRRI Zeigler Experiment Station shown. Weather
data used to run the model were obtained from the NASA Langley Research
Center POWER Project funded through the NASA Earth Science Directorate
Applied Science Program.

### Comparing epidemics

The most common way to compare disease epidemics in botanical
epidemiology is to use the area under the disease progress curve (AUDPC)
(Shaner and Finney 1977). The AUDPC value for a given simulated season
is returned as a part of the output from any of the disease simulations
offered in {epicrop}. You can find the value in the `AUDPC` column. We
can compare the dry season with the wet season by looking at the AUDPC
values for both seasons.

``` r

bb_dry <- bacterial_blight(wth = wth, emergence = "2000-01-05")
summary(bb_dry)
```

    ##      simday           dates                sites            latent     
    ##  Min.   :  1.00   Min.   :2000-01-05   Min.   :  0.00   Min.   : 0.00  
    ##  1st Qu.: 30.75   1st Qu.:2000-02-03   1st Qu.: 25.50   1st Qu.: 0.00  
    ##  Median : 60.50   Median :2000-03-04   Median : 64.45   Median : 1.00  
    ##  Mean   : 60.50   Mean   :2000-03-04   Mean   : 58.57   Mean   : 3.13  
    ##  3rd Qu.: 90.25   3rd Qu.:2000-04-03   3rd Qu.: 97.56   3rd Qu.: 4.81  
    ##  Max.   :120.00   Max.   :2000-05-03   Max.   :100.00   Max.   :21.80  
    ##    infectious        removed         senesced        rateinf            rlex  
    ##  Min.   : 0.000   Min.   : 0.00   Min.   : 0.00   Min.   :0.0000   Min.   :0  
    ##  1st Qu.: 1.000   1st Qu.: 0.00   1st Qu.: 0.00   1st Qu.:0.0000   1st Qu.:0  
    ##  Median : 8.491   Median : 1.00   Median : 1.00   Median :0.0000   Median :0  
    ##  Mean   :14.181   Mean   :12.42   Mean   :12.85   Mean   :0.4471   Mean   :0  
    ##  3rd Qu.:28.857   3rd Qu.:21.71   3rd Qu.:21.71   3rd Qu.:0.3144   3rd Qu.:0  
    ##  Max.   :40.572   Max.   :51.11   Max.   :51.11   Max.   :7.0252   Max.   :0  
    ##    rtransfer         rremoved        rgrowth    rsenesced        diseased     
    ##  Min.   :0.0000   Min.   :0.000   Min.   :0   Min.   :0.000   Min.   : 0.000  
    ##  1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:0   1st Qu.:0.000   1st Qu.: 2.452  
    ##  Median :0.0000   Median :0.000   Median :0   Median :0.000   Median :34.553  
    ##  Mean   :0.4471   Mean   :0.426   Mean   :0   Mean   :0.426   Mean   :29.734  
    ##  3rd Qu.:0.3144   3rd Qu.:0.000   3rd Qu.:0   3rd Qu.:0.000   3rd Qu.:53.122  
    ##  Max.   :7.0252   Max.   :7.025   Max.   :0   Max.   :7.025   Max.   :53.933  
    ##    intensity            lat             lon       
    ##  Min.   :0.00000   Min.   :14.68   Min.   :121.3  
    ##  1st Qu.:0.02447   1st Qu.:14.68   1st Qu.:121.3  
    ##  Median :0.34238   Median :14.68   Median :121.3  
    ##  Mean   :0.36664   Mean   :14.68   Mean   :121.3  
    ##  3rd Qu.:0.55330   3rd Qu.:14.68   3rd Qu.:121.3  
    ##  Max.   :1.00000   Max.   :14.68   Max.   :121.3

#### Dry season intensity

Check the disease progress curve for the dry season.

``` r

ggplot(data = bb_dry,
       aes(x = dates,
           y = intensity * 100)) +
  labs(y = "Intensity (%)",
       x = "Date") +
  geom_line() +
  geom_point() +
  theme_classic()
```

![Dry season site states over time for bacterial blight. Results for dry
season year 2000 at IRRI Zeigler Experiment Station shown. Weather data
used to run the model were obtained from the NASA Langley Research
Center POWER Project funded through the NASA Earth Science Directorate
Applied Science Program.](dry_plot_intensity-1.png)

Dry season site states over time for bacterial blight. Results for dry
season year 2000 at IRRI Zeigler Experiment Station shown. Weather data
used to run the model were obtained from the NASA Langley Research
Center POWER Project funded through the NASA Earth Science Directorate
Applied Science Program.

The AUDPC values can be viewed directly from the attributes of the
`data.table` outputs above, but we can also create a small helper
function to extract them.

``` r

# Dry season
get_audpc(bb_dry)
```

    ## [1] 43.49663

``` r

# Wet season
get_audpc(bb_wet)
```

    ## [1] 49.39145

The AUDPC of the wet season is greater than that of the dry season.
Checking the data and referring to the curves, the wet season intensity
reaches a peak value of 100% and the dry season tops out at 100%. So,
this meets the expectations that the wet season AUDPC is higher than the
dry season, which was predicted to have less disease intensity.

## References

Kim, Kwang-Hyung, Jaepil Cho, Yong Hwan Lee, and Woo-Seop Lee. 2015.
“Predicting Potential Epidemics of Rice Leaf Blast and Sheath Blight in
South Korea Under the RCP 4.5 and RCP 8.5 Climate Change Scenarios Using
a Rice Disease Epidemiology Model, EPIRICE.” *Agricultural and Forest
Meteorology* 203 (April): 191–207.
<https://doi.org/10.1016/j.agrformet.2015.01.011>.

Savary, Serge, Andrew Nelson, Laetitia Willocquet, Ireneo Pangga, and
Jorrel Aunario. 2012. “Modeling and Mapping Potential Epidemics of Rice
Diseases Globally.” *Crop Protection* 34 (April): 6–17.
<https://doi.org/10.1016/j.cropro.2011.11.009>.

Savary, Serge, Stacia Stetkiewicz, François Brun, and Laetitia
Willocquet. 2015. “Modelling and Mapping Potential Epidemics of Wheat
Diseases—Examples on Leaf Rust and Septoria Tritici Blotch Using
EPIWHEAT.” *European Journal of Plant Pathology* 142: 771–90.
<https://api.semanticscholar.org/CorpusID:254474961>.

Shaner, Gregory, and Robert E. Finney. 1977. “The Effect of Nitrogen
Fertilization on the Expression of Slow-Mildewing Resistance in Knox
Wheat.” *Phytopathology* 67 (8): 1051–56.

Sparks, Adam. 2018. “nasapower: A NASA POWER Global Meteorology, Surface
Solar Energy and Climatology Data Client for R.” *Journal of Open Source
Software* 3 (30): 1035. <https://doi.org/10.21105/joss.01035>.

Zadoks, JC. 1971. “Systems Analysis and the Dynamics of Epidemics.”
*Phytopathology* 61 (6): 600–610.
