# *epicrop*: Simulation Modelling of Crop Diseases Using a Susceptible-Exposed-Infectious-Removed (SEIR) Model

<img align="right" src="man/figures/logo.png">

<!-- badges: start -->

[![tic](https://github.com/adamhsparks/epicrop/workflows/tic/badge.svg?branch=main)](https://github.com/adamhsparks/epicrop/actions)
[![codecov](https://codecov.io/gh/adamhsparks/epicrop/branch/main/graph/badge.svg?token=NWrKsX9MaP)](https://codecov.io/gh/adamhsparks/epicrop)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

A fork of [_cropsim_](https://r-forge.r-project.org/R/?group_id=335) designed to make using the EPIRICE model (Savary et al. 2012) for rice diseases easier to use.
This version provides easy to use functions to fetch weather data from NASA POWER, via the [*nasapower*](https://cran.r-project.org/package=nasapower) package (Sparks 2018, Sparks 2020) and predict disease severity of five rice diseases using a generic SEIR model (Zadoks 1971) function, `SEIR()`.

The original EPIRICE manuscript, Savary et al. (2012), which details the model and results of its use to model global epidemics of rice diseases, was published in *Crop Protection* detailing global unmanaged disease risk of bacterial blight, brown spot, leaf blast, sheath blight and tungro, which are included in this package.

# Quick start

You can easily simulate any of the five diseases for rice grown anywhere in the world for years from 1983 to near current using `get_wth()` to fetch data from the [NASA POWER web API](https://power.larc.nasa.gov).
Alternatively, you can supply your own weather data for any time period as long as it fits the model’s requirements.

*epicrop* is not yet on CRAN. You can install it this way.

``` r
if (!require("remotes"))
  install.packages("remotes")
remotes::install_github("adamhsparks/epicrop"
)
```

## Get weather data

First you need to provide weather data for the model. *epicrop* provides the `get_wth()` function to do this. Using it you can fetch weather data for any place in the world from 1983 to near present by providing the
longitude and latitude and dates or length of rice growing season as shown below.

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
```

    ##        YYYYMMDD DOY  TEMP  RHUM  RAIN   LAT   LON
    ##   1: 2000-07-01 183 25.34 91.07 24.87 14.68 121.3
    ##   2: 2000-07-02 184 25.99 85.71 17.63 14.68 121.3
    ##   3: 2000-07-03 185 25.35 94.01 33.52 14.68 121.3
    ##   4: 2000-07-04 186 25.58 93.28 16.21 14.68 121.3
    ##   5: 2000-07-05 187 25.79 92.62 36.28 14.68 121.3
    ##  ---                                             
    ## 117: 2000-10-25 299 25.56 89.57 11.04 14.68 121.3
    ## 118: 2000-10-26 300 25.31 94.35 10.51 14.68 121.3
    ## 119: 2000-10-27 301 25.58 90.85  9.13 14.68 121.3
    ## 120: 2000-10-28 302 25.25 92.52 77.16 14.68 121.3
    ## 121: 2000-10-29 303 24.78 94.41 29.22 14.68 121.3

## Modelling bacterial blight disease severity

Once you have the weather data, run the model for any of the five rice
diseases by providing the emergence or crop establishment date for
transplanted rice.

``` r
bb <- predict_bacterial_blight(wth, emergence = "2000-07-01")

bb
```

    ##      simday      dates  sites latent infectious removed senesced rateinf
    ##   1:      0 2000-06-30  100.0   0.00        0.0     0.0    0.000    0.00
    ##   2:      1 2000-07-01  108.7   0.00        0.0     0.0    1.000    0.00
    ##   3:      2 2000-07-02  118.1   0.00        0.0     0.0    2.087    0.00
    ##   4:      3 2000-07-03  128.3   0.00        0.0     0.0    3.268    0.00
    ##   5:      4 2000-07-04  139.3   0.00        0.0     0.0    4.551    0.00
    ##  ---                                                                    
    ## 117:    116 2000-10-24 1448.6  27.12      897.7   307.6 2185.382   24.56
    ## 118:    117 2000-10-25 1390.2  24.56      882.0   350.4 2242.712   21.54
    ## 119:    118 2000-10-26 1339.1  46.10      842.4   390.0 2296.228   18.52
    ## 120:    119 2000-10-27 1294.8  64.62      805.6   426.8 2346.414   16.11
    ## 121:    120 2000-10-28 1248.7  80.73      763.9   468.5 2401.046   13.32
    ##      rtransfer rgrowth rsenesced diseased severity   lat   lon
    ##   1:      0.00   9.688     1.000        0     0.00 14.68 121.3
    ##   2:      0.00  10.500     1.087        0     0.00 14.68 121.3
    ##   3:      0.00  11.374     1.181        0     0.00 14.68 121.3
    ##   4:      0.00  12.315     1.283        0     0.00 14.68 121.3
    ##   5:      0.00  13.326     1.393        0     0.00 14.68 121.3
    ##  ---                                                          
    ## 117:     27.12  23.495    57.330     1232    38.97 14.68 121.3
    ## 118:      0.00  24.018    53.516     1257    39.47 14.68 121.3
    ## 119:      0.00  24.371    50.186     1279    39.88 14.68 121.3
    ## 120:      0.00  24.609    54.632     1297    40.19 14.68 121.3
    ## 121:      0.00  24.903    56.775     1313    40.35 14.68 121.3

Once you have the results you can visualise them.

``` r
library(ggplot2)

ggplot(data = bb,
       aes(x = dates,
           y = severity)) +
  labs(y = "Severity",
       x = "Date") +
  geom_line() +
  geom_point() +
  labs(title = "Bacterial blight disease progress over time",
       subtitle = "Results for wet season year 2000 at IRRI Zeigler Experiment
       Station shown",
       caption = "Weather data acknowledgement:\nThese data were obtained from
       the NASA Langley Research Center POWER Project\nfunded through the NASA
       Earth Science Directorate Applied Science Program.") +
  theme_light()
```

![](man/figures/fig1-1.png)<!-- -->

# Meta

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md).
By participating in this project you agree to abide by its terms.

# References

Serge Savary, Andrew Nelson, Laetitia Willocquet, Ireneo Pangga and Jorrel Aunario. Modeling and mapping potential epidemics of rice diseases globally. *Crop Protection*, Volume 34, 2012, Pages 6-17, ISSN 0261-2194 DOI: [10.1016/j.cropro.2011.11.009](https://doi.org/10.1016/j.cropro.2011.11.009).

Serge Savary, Stacia Stetkiewicz, François Brun, and Laetitia Willocquet. Modelling and Mapping Potential Epidemics of Wheat Diseases-Examples on Leaf Rust and Septoria Tritici Blotch Using EPIWHEAT. *European Journal of Plant Pathology* 142, no. 4 (August 1, 2015): 771–90. DOI: [10.1007/s10658-015-0650-7](https://doi.org/10.1007/s10658-015-0650-7).

Jan C. Zadoks. Systems Analysis and the Dynamics of Epidemics. Laboratory of Phytopathology, Agricultural University, Wageningen, The Netherlands; *Phytopathology* 61:600. DOI: [10.1094/Phyto-61-600](https://doi.org/10.1094/Phyto-61-600).

Adam Sparks (2018). nasapower: A NASA POWER Global Meteorology, Surface Solar Energy and Climatology Data Client for R. Journal of Open Source Software, 3(30), 1035, [10.21105/joss.01035](https://doi.org/10.21105/joss.01035).

Adam Sparks (2020). *nasapower: NASA-POWER Data from R*. R package version 3.0.1, URL: <https://CRAN.R-project.org/package=nasapower>.
