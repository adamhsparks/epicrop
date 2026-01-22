# epicrop

### Introduction to {epicrop}

{epicrop} provides an R package of the ‘EPIRICE’ model as described in
(Savary et al. 2012), the modified EPIRICE model as described in (Kim et
al. 2015), the ‘EPIWHEAT’ model as described in (Savary et al. 2015) and
a generic SEIR model function,
[`epicrop_sim()`](https://adamhsparks.codeberg.page/epicrop/reference/epicrop_sim.md),
for modelling crop disease epidemics. The model uses daily weather data
to estimate disease intensity. A function,
[`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md),
is provided to simplify downloading weather data via the
[{nasapower}](https://cran.r-project.org/package=nasapower) package
(Sparks 2018) and predict disease intensity of five rice diseases using
a generic SEIR model (Zadoks 1971) function,
[`epicrop_sim()`](https://adamhsparks.codeberg.page/epicrop/reference/epicrop_sim.md).

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
[`epicrop_sim()`](https://adamhsparks.codeberg.page/epicrop/reference/epicrop_sim.md)
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

    ##        YYYYMMDD   DOY  TEMP  TMIN  TMAX  RHUM  RAIN     LAT      LON
    ##          <Date> <int> <num> <num> <num> <num> <num>   <num>    <num>
    ##   1: 1999-12-31   365 24.41 23.34 26.20 93.41  4.92 14.6774 121.2556
    ##   2: 2000-01-01     1 24.38 22.85 27.46 91.25 14.93 14.6774 121.2556
    ##   3: 2000-01-02     2 24.28 22.68 27.42 90.88  6.96 14.6774 121.2556
    ##   4: 2000-01-03     3 23.82 22.17 26.96 88.36  2.28 14.6774 121.2556
    ##   5: 2000-01-04     4 23.68 21.90 27.14 88.00  0.87 14.6774 121.2556
    ##  ---                                                                
    ## 363: 2000-12-27   362 24.46 22.90 26.28 92.49 21.15 14.6774 121.2556
    ## 364: 2000-12-28   363 24.64 23.32 27.28 91.92  6.01 14.6774 121.2556
    ## 365: 2000-12-29   364 24.58 22.51 27.90 90.79  5.49 14.6774 121.2556
    ## 366: 2000-12-30   365 25.31 22.84 28.84 86.25  2.07 14.6774 121.2556
    ## 367: 2000-12-31   366 24.47 21.63 28.63 87.83  3.45 14.6774 121.2556

#### Predict bacterial blight

All of the helper family of functions work in exactly the same manner.
You provide them with weather data and an emergence date, that falls
within the weather data provided, and they will return a data frame of
disease intensity over the season and other values associated with the
model. See the help file for
[`epicrop_sim()`](https://adamhsparks.codeberg.page/epicrop/reference/epicrop_sim.md)
for more on the values returned.

``` r

# Predict bacterial blight intensity for the year 2000 wet season at IRRI
bb_wet <- bacterial_blight(wth, emergence = "2000-07-01")
```

``` r

summary(bb_wet)
```

    ##      simday           dates                sites            latent       
    ##  Min.   :  1.00   Min.   :2000-06-30   Min.   : 108.7   Min.   :  0.000  
    ##  1st Qu.: 30.75   1st Qu.:2000-07-29   1st Qu.: 989.0   1st Qu.:  0.933  
    ##  Median : 60.50   Median :2000-08-28   Median :1712.0   Median : 17.979  
    ##  Mean   : 60.50   Mean   :2000-08-28   Mean   :1590.1   Mean   : 60.023  
    ##  3rd Qu.: 90.25   3rd Qu.:2000-09-27   3rd Qu.:2278.9   3rd Qu.:108.208  
    ##  Max.   :120.00   Max.   :2000-10-27   Max.   :2586.3   Max.   :234.072  
    ##    infectious        removed          senesced         rateinf      
    ##  Min.   :  0.00   Min.   :  0.00   Min.   :   1.0   Min.   : 0.000  
    ##  1st Qu.:  1.00   1st Qu.:  0.00   1st Qu.: 122.7   1st Qu.: 0.000  
    ##  Median : 26.79   Median :  1.00   Median : 665.4   Median : 0.866  
    ##  Mean   :267.60   Mean   : 56.04   Mean   : 775.2   Mean   :12.262  
    ##  3rd Qu.:595.15   3rd Qu.: 28.87   3rd Qu.:1390.0   3rd Qu.:22.356  
    ##  Max.   :970.87   Max.   :612.71   Max.   :1895.1   Max.   :56.827  
    ##       rlex     rtransfer        rremoved         rgrowth         rsenesced     
    ##  Min.   :0   Min.   : 0.00   Min.   : 0.000   Min.   : 9.688   Min.   : 1.000  
    ##  1st Qu.:0   1st Qu.: 0.00   1st Qu.: 0.000   1st Qu.:19.569   1st Qu.: 9.323  
    ##  Median :0   Median : 0.00   Median : 0.000   Median :32.024   Median :17.120  
    ##  Mean   :0   Mean   :11.83   Mean   : 5.106   Mean   :38.908   Mean   :15.793  
    ##  3rd Qu.:0   3rd Qu.:22.36   3rd Qu.: 2.517   3rd Qu.:57.391   3rd Qu.:22.789  
    ##  Max.   :0   Max.   :56.83   Max.   :48.174   Max.   :79.585   Max.   :25.863  
    ##     diseased          intensity             lat             lon       
    ##  Min.   :   0.000   Min.   :0.000000   Min.   :14.68   Min.   :121.3  
    ##  1st Qu.:   2.429   1st Qu.:0.002382   1st Qu.:14.68   1st Qu.:121.3  
    ##  Median :  46.853   Median :0.017969   Median :14.68   Median :121.3  
    ##  Mean   : 383.668   Mean   :0.122956   Mean   :14.68   Mean   :121.3  
    ##  3rd Qu.: 846.783   3rd Qu.:0.289563   3rd Qu.:14.68   3rd Qu.:121.3  
    ##  Max.   :1471.442   Max.   :0.426437   Max.   :14.68   Max.   :121.3

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
    ##  Min.   :  1.00   Min.   :2000-01-04   Min.   : 108.7   Min.   :  0.000  
    ##  1st Qu.: 30.75   1st Qu.:2000-02-02   1st Qu.: 989.5   1st Qu.:  0.000  
    ##  Median : 60.50   Median :2000-03-03   Median :2513.1   Median :  2.444  
    ##  Mean   : 60.50   Mean   :2000-03-03   Mean   :1865.1   Mean   : 14.522  
    ##  3rd Qu.: 90.25   3rd Qu.:2000-04-02   3rd Qu.:2590.0   3rd Qu.: 19.080  
    ##  Max.   :120.00   Max.   :2000-05-02   Max.   :2714.2   Max.   :148.175  
    ##    infectious        removed          senesced         rateinf      
    ##  Min.   :  0.00   Min.   :  0.00   Min.   :   1.0   Min.   : 0.000  
    ##  1st Qu.:  1.00   1st Qu.:  0.00   1st Qu.: 122.7   1st Qu.: 0.000  
    ##  Median : 18.49   Median :  1.00   Median : 665.3   Median : 0.000  
    ##  Mean   : 74.78   Mean   : 21.38   Mean   : 824.8   Mean   : 2.904  
    ##  3rd Qu.:141.06   3rd Qu.: 19.49   3rd Qu.:1455.2   3rd Qu.: 0.562  
    ##  Max.   :285.00   Max.   :154.75   Max.   :2213.8   Max.   :43.425  
    ##       rlex     rtransfer         rremoved        rgrowth         rsenesced     
    ##  Min.   :0   Min.   : 0.000   Min.   : 0.00   Min.   : 9.688   Min.   : 1.000  
    ##  1st Qu.:0   1st Qu.: 0.000   1st Qu.: 0.00   1st Qu.:25.940   1st Qu.: 9.333  
    ##  Median :0   Median : 0.000   Median : 0.00   Median :33.960   Median :24.958  
    ##  Mean   :0   Mean   : 2.904   Mean   : 1.29   Mean   :41.597   Mean   :18.448  
    ##  3rd Qu.:0   3rd Qu.: 0.562   3rd Qu.: 0.00   3rd Qu.:57.653   3rd Qu.:25.900  
    ##  Max.   :0   Max.   :43.425   Max.   :25.25   Max.   :79.439   Max.   :27.142  
    ##     diseased         intensity             lat             lon       
    ##  Min.   :  0.000   Min.   :0.000000   Min.   :14.68   Min.   :121.3  
    ##  1st Qu.:  2.166   1st Qu.:0.002226   1st Qu.:14.68   1st Qu.:121.3  
    ##  Median : 33.297   Median :0.012592   Median :14.68   Median :121.3  
    ##  Mean   :110.689   Mean   :0.032582   Mean   :14.68   Mean   :121.3  
    ##  3rd Qu.:200.347   3rd Qu.:0.063808   3rd Qu.:14.68   3rd Qu.:121.3  
    ##  Max.   :348.522   Max.   :0.108523   Max.   :14.68   Max.   :121.3

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

    ## [1] 3.874294

``` r

# Wet season
get_audpc(bb_wet)
```

    ## [1] 14.56478

The AUDPC of the wet season is greater than that of the dry season.
Checking the data and referring to the curves, the wet season intensity
reaches a peak value of 43% and the dry season tops out at 11%. So, this
meets the expectations that the wet season AUDPC is higher than the dry
season, which was predicted to have less disease intensity.

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
