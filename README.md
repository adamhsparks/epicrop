
<!-- README.md is generated from README.Rmd. Please edit that file -->

# *epicrop*: Simulation Modelling of Crop Diseases Using a Susceptible-Exposed-Infectious-Removed (SEIR) Model

<img align="right" src="man/figures/logo.png">

<!-- badges: start -->

[![tic](https://github.com/adamhsparks/epicrop/workflows/tic/badge.svg?branch=main)](https://github.com/adamhsparks/epicrop/actions)
[![codecov](https://codecov.io/gh/adamhsparks/epicrop/branch/main/graph/badge.svg?token=NWrKsX9MaP)](https://codecov.io/gh/adamhsparks/epicrop)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![DOI](https://zenodo.org/badge/58613738.svg)](https://zenodo.org/badge/latestdoi/58613738)
<!-- badges: end -->

A fork of [*cropsim*](https://r-forge.r-project.org/R/?group_id=335)
(Hijmans *et al.* 2009) designed to make using the EPIRICE model (Savary
*et al.* 2012) for rice diseases easier to use. This version provides
easy to use functions to fetch weather data from NASA POWER, via the
[*nasapower*](https://cran.r-project.org/package=nasapower) package
(Sparks 2018, Sparks 2020) or
[*chirps*](https://docs.ropensci.org/chirps/) package (de Sousa \_et
al. 2020), which provides weather data from the Client for the Climate
Hazards Center ‘CHIRPS’ and ‘CHIRTS’ and predict disease intensity of
five rice diseases using a generic SEIR model (Zadoks 1971) function,
`SEIR()`.

The original EPIRICE manuscript, Savary *et al.* (2012), which details
the model and results of its use to model global epidemics of rice
diseases, was published in *Crop Protection* detailing global unmanaged
disease risk of bacterial blight, brown spot, leaf blast, sheath blight
and rice tungro, which are included in this package.

# Quick start

You can easily simulate any of the five diseases for rice grown anywhere
in the world for years from 1983 to near current using `get_wth()` to
fetch data from the [NASA POWER web API](https://power.larc.nasa.gov) or
[CHIRPS and CHIRTS web APIs](https://chc.ucsb.edu/data). Alternatively,
you can supply your own weather data for any time period as long as it
fits the model’s requirements.

*epicrop* is not yet on CRAN. You can install it this way.

``` r
if (!require("remotes"))
  install.packages("remotes")
remotes::install_github("adamhsparks/epicrop",
                        build_vignettes = TRUE
)
```

## Get weather data

First you need to provide weather data for the model. *epicrop* provides
the `get_wth()` function to do this. Using it you can fetch weather data
for any place in the world from 1983 to near present by providing the
and latitude and dates or length of rice growing season as shown below.

``` r
library("epicrop")

# Fetch weather for year 2000 wet season for a 120 day rice variety at the IRRI
# Zeigler Experiment Station
wth <- get_wth(
  lonlat = c(121.25562, 14.6774),
  dates = "2000-07-01",
  duration = 120
)

wth
#>        YYYYMMDD DOY  TEMP  RHUM  RAIN     LAT      LON
#>   1: 2000-07-01 183 25.30 92.19 23.12 14.6774 121.2556
#>   2: 2000-07-02 184 26.13 86.00 17.34 14.6774 121.2556
#>   3: 2000-07-03 185 25.51 94.19 29.08 14.6774 121.2556
#>   4: 2000-07-04 186 25.81 92.44 13.00 14.6774 121.2556
#>   5: 2000-07-05 187 25.97 92.31 32.20 14.6774 121.2556
#>  ---                                                  
#> 117: 2000-10-25 299 25.82 89.75 12.04 14.6774 121.2556
#> 118: 2000-10-26 300 25.44 94.94 13.03 14.6774 121.2556
#> 119: 2000-10-27 301 25.74 91.44 11.54 14.6774 121.2556
#> 120: 2000-10-28 302 25.44 91.88 74.20 14.6774 121.2556
#> 121: 2000-10-29 303 24.97 94.12 29.11 14.6774 121.2556
```

## Modelling bacterial blight disease intensity

Once you have the weather data, run the model for any of the five rice
diseases by providing the emergence or crop establishment date for
transplanted rice.

``` r
bb <- predict_bacterial_blight(wth, emergence = "2000-07-01")

bb
#>      simday      dates     sites    latent infectious  removed    senesced
#>   1:      1 2000-07-01  100.0000   0.00000     0.0000   0.0000    0.000000
#>   2:      2 2000-07-02  108.6875   0.00000     0.0000   0.0000    1.000000
#>   3:      3 2000-07-03  118.1002   0.00000     0.0000   0.0000    2.086875
#>   4:      4 2000-07-04  128.2934   0.00000     0.0000   0.0000    3.267877
#>   5:      5 2000-07-05  139.3254   0.00000     0.0000   0.0000    4.550811
#>  ---                                                                      
#> 116:    116 2000-10-24 1453.0383 107.60598   953.2163 217.8993 2095.744599
#> 117:    117 2000-10-25 1429.1891  77.30207   952.9399 248.4797 2140.855357
#> 118:    118 2000-10-26 1380.4038  73.81997   947.4256 281.2134 2187.881013
#> 119:    119 2000-10-27 1332.8691  69.34615   938.4942 315.9354 2236.407049
#> 120:    120 2000-10-28 1286.3824  64.14358   926.0857 352.6359 2286.436253
#>       rateinf rtransfer  rgrowth rsenesced diseased intensity     lat      lon
#>   1:  0.00000   0.00000  9.68750  1.000000    0.000 0.0000000 14.6774 121.2556
#>   2:  0.00000   0.00000 10.49959  1.086875    0.000 0.0000000 14.6774 121.2556
#>   3:  0.00000   0.00000 11.37416  1.181002    0.000 0.0000000 14.6774 121.2556
#>   4:  0.00000   0.00000 12.31499  1.282934    0.000 0.0000000 14.6774 121.2556
#>   5:  0.00000   0.00000 13.32593  1.393254    0.000 0.0000000 14.6774 121.2556
#>  ---                                                                          
#> 116:  0.00000  30.30391 21.26159 45.110758 1278.722 0.4219893 14.6774 121.2556
#> 117: 23.73737  27.21946 21.97777 47.025655 1278.722 0.4188944 14.6774 121.2556
#> 118: 21.31678  25.79060 22.30807 48.526036 1302.459 0.4252267 14.6774 121.2556
#> 119: 19.08944  24.29201 22.63192 50.029205 1323.776 0.4305705 14.6774 121.2556
#> 120: 17.03536   0.00000 22.94393 47.492428 1342.865 0.4349575 14.6774 121.2556
```

Lastly, you can visualise the result of the model run.

``` r
library("ggplot2")

ggplot(data = bb,
       aes(x = dates,
           y = intensity)) +
  labs(y = "Intensity",
       x = "Date") +
  geom_line() +
  geom_point() +
  theme_classic()
```

<img src="man/figures/README-plot-1.png" title="Bacterial blight disease progress over time. Results for wet season year 2000 at IRRI Zeigler Experiment Station shown. Weather data used to run the model were obtained from the NASA Langley Research Center POWER Project funded through the NASA Earth Science Directorate Applied Science Program." alt="Bacterial blight disease progress over time. Results for wet season year 2000 at IRRI Zeigler Experiment Station shown. Weather data used to run the model were obtained from the NASA Langley Research Center POWER Project funded through the NASA Earth Science Directorate Applied Science Program." width="100%" />

# Meta

-   Please [report any issues or
    bugs](https://github.com/adamhsparks/epicrop/issues).

-   License: MIT

-   To cite *epicrop*, please use the output from
    `citation(package = "epicrop")`.

## Code of Conduct

Please note that the epicrop project is released with a [Contributor
Code of
Conduct](http://adamhsparks.github.io/epicrop/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

# References

Robert J. Hijmans, Serge Savary, Rene Pangga and Jorrel Aunario. (2009)
Simulation modeling of crops and their diseases. R package version
0.2-6.

Serge Savary, Andrew Nelson, Laetitia Willocquet, Ireneo Pangga and
Jorrel Aunario.(2012). Modeling and mapping potential epidemics of rice
diseases globally. *Crop Protection*, Volume 34, Pages 6-17, ISSN
0261-2194 DOI:
[10.1016/j.cropro.2011.11.009](https://doi.org/10.1016/j.cropro.2011.11.009).

Serge Savary, Stacia Stetkiewicz, François Brun, and Laetitia
Willocquet. Modelling and Mapping Potential Epidemics of Wheat
Diseases-Examples on Leaf Rust and Septoria Tritici Blotch Using
EPIWHEAT. *European Journal of Plant Pathology* 142, no. 4 (August 1,
2015): 771–90. DOI:
[10.1007/s10658-015-0650-7](https://doi.org/10.1007/s10658-015-0650-7).

Kauê de Sousa and Adam H. Sparks and William Ashmall and Jacob van Etten
and Svein Ø. Solberg (2020). chirps: API Client for the CHIRPS
Precipitation Data in R. Journal of Open Source Software, 5(51), 2419,
DOI: [10.21105/joss.02419](https://doi.org/10.21105/joss.02419)

Adam Sparks (2018). nasapower: A NASA POWER Global Meteorology, Surface
Solar Energy and Climatology Data Client for R. Journal of Open Source
Software, 3(30), 1035, DOI:
[10.21105/joss.01035](https://doi.org/10.21105/joss.01035).

Adam Sparks (2020). *nasapower: NASA-POWER Data from R*. R package
version 3.0.1, URL: <https://CRAN.R-project.org/package=nasapower>.

Jan C. Zadoks. (1971) Systems Analysis and the Dynamics of Epidemics.
*Phytopathology* 61:600. DOI:
[10.1094/Phyto-61-600](https://doi.org/10.1094/Phyto-61-600).
