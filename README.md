
<!-- README.md is generated from README.Rmd. Please edit that file -->



# Simulation Modelling of Crop Diseases Using a Susceptible-Exposed-Infectious-Removed (SEIR) Model <img src="man/figures/logo.png" align="right"/>

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable) [![DOI](https://zenodo.org/badge/58613738.svg)](https://zenodo.org/badge/latestdoi/58613738) [![CRAN status](https://www.r-pkg.org/badges/version/epicrop)](https://CRAN.R-project.org/package=epicrop) [![Code Coverage: 70%](https://img.shields.io/badge/code_coverage-70%25-green)](#code-coverage)

<!-- badges: end -->

A fork of [{cropsim}](https://github.com/r-forge/cropsim/tree/master/pkg/cropsim) (Hijmans *et al.* 2009) designed to make using the EPIRICE model (Savary *et al.* 2012) for rice diseases easier to use and to implement the EPIWHEAT model in the same R package.
This version provides easy to use functions to fetch weather data from NASA POWER, via the [{nasapower}](https://cran.r-project.org/package=nasapower) package (Sparks 2018) and predict disease intensity of five rice diseases using a generic Susceptible-Exposed-Infectious-Removed (SEIR) model (Zadoks 1971) function, `seir()`.

The original EPIRICE manuscript, Savary *et al.* (2012), which details the model and results of its use to model global epidemics of rice diseases, was published in *Crop Protection* detailing global unmanaged disease risk of bacterial blight, brown spot, leaf blast, sheath blight and rice tungro, which are included in this package.
The original EPIWHEAT manuscript, Savary *et al.* (2015), which details the model and results of its use to model potential epidemics of wheat diseases, leaf rust and Septoria tritici blotch, is also included in this package.

Additional models included for rice are adapted from Kim et al. (2015) for rice sheath blight and leaf blast.

# Quick start

You can easily simulate any of the five diseases for rice grown anywhere in the world for years from 1983 to near current using `get_wth()` to fetch data from the [NASA POWER web API](https://power.larc.nasa.gov).
Alternatively, you can supply your own weather data for any time period as long as it fits the model’s requirements.

{epicrop} is not yet on CRAN.
You can install it this way.

```r 
install.packages("epicrop",
  repos = c("https://adamhsparks.r-universe.dev",
            "https://cloud.r-project.org"))
```

## Get weather data

First you need to provide weather data for the model; {epicrop} provides the `get_wth()` function to do this.
Using it you can fetch weather data for any place in the world from 1983 to near present by providing the and latitude and dates or length of rice growing season as shown below.


``` r
library(epicrop)

# Fetch weather for year 2000 wet season for a 120 day rice variety at the IRRI
# Zeigler Experiment Station
wth <- get_wth(
  lonlat = c(121.25562, 14.6774),
  dates = "2000-07-01",
  duration = 120
)

wth
#>        YYYYMMDD   DOY  TEMP  TMIN  TMAX  RHUM  RAIN     LAT      LON
#>          <Date> <int> <num> <num> <num> <num> <num>   <num>    <num>
#>   1: 2000-06-30   182 26.02 23.34 28.76 88.53 11.01 14.6774 121.2556
#>   2: 2000-07-01   183 25.29 23.86 27.78 92.20 23.12 14.6774 121.2556
#>   3: 2000-07-02   184 26.13 23.54 29.90 86.01 17.34 14.6774 121.2556
#>   4: 2000-07-03   185 25.50 24.28 27.23 94.16 29.08 14.6774 121.2556
#>   5: 2000-07-04   186 25.81 24.50 27.56 92.42 13.01 14.6774 121.2556
#>  ---                                                                
#> 118: 2000-10-25   299 25.82 23.44 29.54 89.76 12.04 14.6774 121.2556
#> 119: 2000-10-26   300 25.44 24.14 26.99 94.93 13.03 14.6774 121.2556
#> 120: 2000-10-27   301 25.74 24.54 27.69 91.43 11.54 14.6774 121.2556
#> 121: 2000-10-28   302 25.44 24.72 26.62 91.90 74.20 14.6774 121.2556
#> 122: 2000-10-29   303 24.97 24.15 26.34 94.15 29.11 14.6774 121.2556
```

## Modelling bacterial blight disease intensity

Once you have the weather data, run the model for any of the five rice diseases by providing the emergence or crop establishment date for transplanted rice.


``` r
bb_sim <- bacterial_blight(wth, emergence = "2000-07-01")

bb_sim
#>      simday      dates     sites   latent infectious  removed    senesced
#>       <int>     <Date>     <num>    <num>      <num>    <num>       <num>
#>   1:      1 2000-06-30  108.6875  0.00000     0.0000   0.0000    1.000000
#>   2:      2 2000-07-01  118.1002  0.00000     0.0000   0.0000    2.086875
#>   3:      3 2000-07-02  128.2934  0.00000     0.0000   0.0000    3.267877
#>   4:      4 2000-07-03  139.3254  0.00000     0.0000   0.0000    4.550811
#>   5:      5 2000-07-04  151.2581  0.00000     0.0000   0.0000    5.944065
#>  ---                                                                     
#> 116:    116 2000-10-23 1453.4937 66.62053   916.5275 435.9723 1837.520100
#> 117:    117 2000-10-24 1453.8292 43.86453   891.1091 484.1467 1852.055037
#> 118:    118 2000-10-25 1435.3902 40.40084   868.8771 528.6018 1866.593329
#> 119:    119 2000-10-26 1418.3189 36.13249   849.7002 569.4202 1880.947231
#> 120:    120 2000-10-27 1402.4149 52.32144   806.4095 612.7109 1895.130420
#>       rateinf  rlex rtransfer rremoved  rgrowth rsenesced diseased intensity
#>         <num> <num>     <num>    <num>    <num>     <num>    <num>     <num>
#>   1:  0.00000     0   0.00000  0.00000  9.68750  1.000000    0.000 0.0000000
#>   2:  0.00000     0   0.00000  0.00000 10.49959  1.086875    0.000 0.0000000
#>   3:  0.00000     0   0.00000  0.00000 11.37416  1.181002    0.000 0.0000000
#>   4:  0.00000     0   0.00000  0.00000 12.31499  1.282934    0.000 0.0000000
#>   5:  0.00000     0   0.00000  0.00000 13.32593  1.393254    0.000 0.0000000
#>  ---                                                                        
#> 116:  0.00000     0  24.08819 46.61892 14.88278 14.531424 1419.120 0.4034849
#> 117:  0.00000     0  22.75600 48.17443 14.87042 14.534937 1419.120 0.3913984
#> 118: 18.75936     0  22.22305 44.45507 14.85861 14.538292 1437.880 0.3878067
#> 119: 17.37313     0  21.64148 40.81840 14.65579 14.353902 1455.253 0.3844507
#> 120: 16.18895     0   0.00000 43.29069 14.46810 14.183189 1471.442 0.3797769
#>          lat      lon
#>        <num>    <num>
#>   1: 14.6774 121.2556
#>   2: 14.6774 121.2556
#>   3: 14.6774 121.2556
#>   4: 14.6774 121.2556
#>   5: 14.6774 121.2556
#>  ---                 
#> 116: 14.6774 121.2556
#> 117: 14.6774 121.2556
#> 118: 14.6774 121.2556
#> 119: 14.6774 121.2556
#> 120: 14.6774 121.2556
```

Lastly, you can visualise the result of the model run.


``` r
library(ggplot2)

ggplot(
  data = bb_sim,
  aes(
    x = dates,
    y = intensity
  )
) +
  labs(
    y = "Intensity",
    x = "Date"
  ) +
  geom_line() +
  geom_point() +
  theme_classic()
```

<div class="figure">
<img src="man/figures/README-plot-1.png" alt="Bacterial blight disease progress over time. Results for wet season year 2000 at IRRI Zeigler Experiment Station shown. Weather data used to run the model were obtained from the NASA Langley Research Center POWER Project funded through the NASA Earth Science Directorate Applied Science Program." width="100%" />
<p class="caption">Bacterial blight disease progress over time. Results for wet season year 2000 at IRRI Zeigler Experiment Station shown. Weather data used to run the model were obtained from the NASA Langley Research Center POWER Project funded through the NASA Earth Science Directorate Applied Science Program.</p>
</div>

# Meta

-   Please [report any issues or bugs](https://codeberg.org/adamhsparks/epicrop/issues).

-   License: GPL-3

-   To cite {epicrop}, please use the output from `citation(package = "epicrop")`.

## Code Coverage


```
#> epicrop Coverage: 70.38%
#> R/audpc_dt.R: 0.00%
#> R/build_epicrop_emergence.R: 0.00%
#> R/fetch_epicrop_weather_list.R: 0.00%
#> R/run_epicrop_model.R: 0.00%
#> R/calculate_audpc.R: 80.00%
#> R/validation.R: 83.07%
#> R/epicrop_sim.R: 85.47%
#> R/leaf_wetness.R: 90.55%
#> src/seir_core.cpp: 97.73%
#> R/disease_models.R: 100.00%
#> R/interpolation.R: 100.00%
#> R/weather.R: 100.00%
```

## Code of Conduct

Please note that the epicrop project is released with a [Contributor Code of Conduct](https://codeberg.org/adamhsparks/epicrop/src/branch/main/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## Other Implementations in R

 - The EPIRICE model was originally written in R as a part of the [{cropsim}](https://github.com/r-forge/cropsim/tree/master/pkg/cropsim) package (Hijmans _et al._ 2009).

 - The EPIRICE model is also available from CRAN in the [{ZeBook}](https://cran.r-project.org/package=ZeBook) package (Brun _et al._ 2018) to accompany, "Working with dynamic crop models: methods, tools and examples for agriculture and environment" (Wallach _et al._ 2018).

# References
Brun François, David Makowski, Daniel Wallach, and James W Jones.
(2018).
*ZeBook: Working with Dynamic Models for Agriculture and Environment*.
DOI: [10.32614/CRAN.package.ZeBook](https://doi.org/10.32614/CRAN.package.ZeBook) 
R package version 1.1, <https://CRAN.R-project.org/package=ZeBook>.

Kwang-Hyung Kim, Jaepil Cho, Yong Hwan Lee, and Woo-Seop Lee.
(2015).
Predicting potential epidemics of rice leaf blast and sheath blight in South Korea.
*Agricultural and Forest Meteorology*, 203: 191-207.
DOI: [10.1016/j.agrformet.2015.01.011](https://doi.org/10.1016/j.agrformet.2015.01.011)

Robert J. Hijmans, Serge Savary, Rene Pangga and Jorrel Aunario.
**cropsim**.
(2009).
Simulation modeling of crops and their diseases.
R package version 0.2-6.

Serge Savary, Andrew Nelson, Laetitia Willocquet, Ireneo Pangga and Jorrel Aunario.
(2012).
Modeling and mapping potential epidemics of rice diseases globally.
*Crop Protection*, Volume 34, Pages 6-17, ISSN 0261-2194 DOI: [10.1016/j.cropro.2011.11.009](https://doi.org/10.1016/j.cropro.2011.11.009).

Serge Savary, Stacia Stetkiewicz, François Brun, and Laetitia Willocquet.
Modelling and Mapping Potential Epidemics of Wheat Diseases-Examples on Leaf Rust and Septoria Tritici Blotch Using EPIWHEAT.
(2015).
*European Journal of Plant Pathology* 142, no. 4:771–90.
DOI: [10.1007/s10658-015-0650-7](https://doi.org/10.1007/s10658-015-0650-7).

Adam Sparks.
(2018).
nasapower: A NASA POWER Global Meteorology, Surface Solar Energy and Climatology Data Client for R.
*Journal of Open Source Software*, 3(30), 1035, DOI: [10.21105/joss.01035](https://doi.org/10.21105/joss.01035).

Jan C. Zadoks.
(1971).
Systems Analysis and the Dynamics of Epidemics.
*Phytopathology* 61:600.
DOI: [10.1094/Phyto-61-600](https://doi.org/10.1094/Phyto-61-600).

Wallach, Daniel, David Makowski, James W. Jones, and François Brun.
(2018)
_Working with dynamic crop models: methods, tools and examples for agriculture and environment_. Academic Press.
