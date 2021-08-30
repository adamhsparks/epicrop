
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
designed to make using the EPIRICE model (Savary *et al.* 2012) for rice
diseases easier to use. This version provides easy to use functions to
fetch weather data from NASA POWER, via the
[*nasapower*](https://cran.r-project.org/package=nasapower) package
(Sparks 2018, Sparks 2020) and predict disease intensity of five rice
diseases using a generic SEIR model (Zadoks 1971) function, `SEIR()`.

The original EPIRICE manuscript, Savary *et al.* (2012), which details
the model and results of its use to model global epidemics of rice
diseases, was published in *Crop Protection* detailing global unmanaged
disease risk of bacterial blight, brown spot, leaf blast, sheath blight
and tungro, which are included in this package.

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
#>   1: 2000-07-01 183 25.30 92.19 21.09 14.6774 121.2556
#>   2: 2000-07-02 184 26.13 86.00 15.82 14.6774 121.2556
#>   3: 2000-07-03 185 25.51 94.19 31.64 14.6774 121.2556
#>   4: 2000-07-04 186 25.81 92.44 10.55 14.6774 121.2556
#>   5: 2000-07-05 187 25.97 92.31 31.64 14.6774 121.2556
#>  ---                                                  
#> 117: 2000-10-25 299 25.82 89.75 10.55 14.6774 121.2556
#> 118: 2000-10-26 300 25.44 94.94 10.55 14.6774 121.2556
#> 119: 2000-10-27 301 25.74 91.44 10.55 14.6774 121.2556
#> 120: 2000-10-28 302 25.44 91.88 73.83 14.6774 121.2556
#> 121: 2000-10-29 303 24.97 94.12 31.64 14.6774 121.2556
```

## Modelling bacterial blight disease intensity

Once you have the weather data, run the model for any of the five rice
diseases by providing the emergence or crop establishment date for
transplanted rice.

``` r
bb <- predict_bacterial_blight(wth, emergence = "2000-07-01")

bb
#>      simday      dates    sites   latent infectious  removed    senesced
#>   1:      0 2000-07-01 100.0000  0.00000     0.0000   0.0000    0.000000
#>   2:      1 2000-07-02 108.6875  0.00000     0.0000   0.0000    1.000000
#>   3:      2 2000-07-03 118.1002  0.00000     0.0000   0.0000    2.086875
#>   4:      3 2000-07-04 128.2934  0.00000     0.0000   0.0000    3.267877
#>   5:      4 2000-07-05 139.3254  0.00000     0.0000   0.0000    4.550811
#>  ---                                                                    
#> 117:    116 2000-10-25 875.8491 30.45186   832.4015 650.1907 2348.383357
#> 118:    117 2000-10-26 837.8242 25.59042   795.4533 696.8289 2403.780012
#> 119:    118 2000-10-27 800.6865 21.17410   757.1764 743.6080 2458.937342
#> 120:    119 2000-10-28 764.5504 24.60700   710.5275 790.2569 2513.593193
#> 121:    120 2000-10-29 729.5491 20.84640   670.8576 836.5217 2567.503481
#>       rateinf rtransfer  rgrowth rsenesced diseased intensity     lat      lon
#>   1: 0.000000  0.000000  9.68750  1.000000    0.000 0.0000000 14.6774 121.2556
#>   2: 0.000000  0.000000 10.49959  1.086875    0.000 0.0000000 14.6774 121.2556
#>   3: 0.000000  0.000000 11.37416  1.181002    0.000 0.0000000 14.6774 121.2556
#>   4: 0.000000  0.000000 12.31499  1.282934    0.000 0.0000000 14.6774 121.2556
#>   5: 0.000000  0.000000 13.32593  1.393254    0.000 0.0000000 14.6774 121.2556
#>  ---                                                                          
#> 117: 4.828461  9.689900 22.20022 55.396656 1513.044 0.4962628 14.6774 121.2556
#> 118: 4.085963  8.502277 22.10555 55.157330 1517.873 0.4949422 14.6774 121.2556
#> 119: 3.432902  0.000000 21.95270 54.655851 1521.959 0.4929274 14.6774 121.2556
#> 120: 2.834348  6.594946 21.74329 53.910288 1525.391 0.4901926 14.6774 121.2556
#> 121: 2.335856  5.664732 21.48123 52.794037 1528.226 0.4866860 14.6774 121.2556
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

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms.

# References

Serge Savary, Andrew Nelson, Laetitia Willocquet, Ireneo Pangga and
Jorrel Aunario. Modeling and mapping potential epidemics of rice
diseases globally. *Crop Protection*, Volume 34, 2012, Pages 6-17, ISSN
0261-2194 DOI:
[10.1016/j.cropro.2011.11.009](https://doi.org/10.1016/j.cropro.2011.11.009).

Serge Savary, Stacia Stetkiewicz, François Brun, and Laetitia
Willocquet. Modelling and Mapping Potential Epidemics of Wheat
Diseases-Examples on Leaf Rust and Septoria Tritici Blotch Using
EPIWHEAT. *European Journal of Plant Pathology* 142, no. 4 (August 1,
2015): 771–90. DOI:
[10.1007/s10658-015-0650-7](https://doi.org/10.1007/s10658-015-0650-7).

Jan C. Zadoks. Systems Analysis and the Dynamics of Epidemics.
Laboratory of Phytopathology, Agricultural University, Wageningen, The
Netherlands; *Phytopathology* 61:600. DOI:
[10.1094/Phyto-61-600](https://doi.org/10.1094/Phyto-61-600).

Adam Sparks (2018). nasapower: A NASA POWER Global Meteorology, Surface
Solar Energy and Climatology Data Client for R. Journal of Open Source
Software, 3(30), 1035,
[10.21105/joss.01035](https://doi.org/10.21105/joss.01035).

Adam Sparks (2020). *nasapower: NASA-POWER Data from R*. R package
version 3.0.1, URL: <https://CRAN.R-project.org/package=nasapower>.
