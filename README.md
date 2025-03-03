---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# {epicrop} Simulation Modelling of Crop Diseases Using a Susceptible-Exposed-Infectious-Removed (SEIR) Model <img src="man/figures/logo.png" align="right"/>

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable) [![DOI](https://zenodo.org/badge/58613738.svg)](https://zenodo.org/badge/latestdoi/58613738) [![CRAN status](https://www.r-pkg.org/badges/version/epicrop)](https://CRAN.R-project.org/package=epicrop)

<!-- badges: end -->

A fork of [{cropsim}](https://github.com/r-forge/cropsim/tree/master/pkg/cropsim) (Hijmans *et al.* 2009) designed to make using the EPIRICE model (Savary *et al.* 2012) for rice diseases easier to use.
This version provides easy to use functions to fetch weather data from NASA POWER, via the [{nasapower}](https://cran.r-project.org/package=nasapower) package (Sparks 2018, Sparks 2020) and predict disease intensity of five rice diseases using a generic SEIR model (Zadoks 1971) function, `SEIR()`.

The original EPIRICE manuscript, Savary *et al.* (2012), which details the model and results of its use to model global epidemics of rice diseases, was published in *Crop Protection* detailing global unmanaged disease risk of bacterial blight, brown spot, leaf blast, sheath blight and rice tungro, which are included in this package.

# Quick start

You can easily simulate any of the five diseases for rice grown anywhere in the world for years from 1983 to near current using `get_wth()` to fetch data from the [NASA POWER web API](https://power.larc.nasa.gov).
Alternatively, you can supply your own weather data for any time period as long as it fits the model’s requirements.

{epicrop} is not yet on CRAN.
You can install it this way.


``` r
if (!require("pak")) {
  install.packages("pak",
    repos = sprintf(
      "https://r-lib.github.io/p/pak/stable/%s/%s/%s",
      .Platform$pkgType, R.Version()$os, R.Version()$arch
    )
  )
}

pak::pkg_install("git::https://codeberg.org/adamhsparks/epicrop")
```

## Get weather data

First you need to provide weather data for the model; {epicrop} provides the `get_wth()` function to do this.
Using it you can fetch weather data for any place in the world from 1983 to near present by providing the and latitude and dates or length of rice growing season as shown below.


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
#>        YYYYMMDD   DOY  TEMP  RHUM  RAIN     LAT      LON
#>          <Date> <int> <num> <num> <num>   <num>    <num>
#>   1: 2000-07-01   183 25.29 92.20 23.12 14.6774 121.2556
#>   2: 2000-07-02   184 26.13 86.01 17.34 14.6774 121.2556
#>   3: 2000-07-03   185 25.50 94.16 29.08 14.6774 121.2556
#>   4: 2000-07-04   186 25.81 92.42 13.01 14.6774 121.2556
#>   5: 2000-07-05   187 25.97 92.34 32.20 14.6774 121.2556
#>  ---                                                    
#> 117: 2000-10-25   299 25.82 89.76 12.04 14.6774 121.2556
#> 118: 2000-10-26   300 25.44 94.93 13.03 14.6774 121.2556
#> 119: 2000-10-27   301 25.74 91.43 11.54 14.6774 121.2556
#> 120: 2000-10-28   302 25.44 91.90 74.20 14.6774 121.2556
#> 121: 2000-10-29   303 24.97 94.15 29.11 14.6774 121.2556
```

## Modelling bacterial blight disease intensity

Once you have the weather data, run the model for any of the five rice diseases by providing the emergence or crop establishment date for transplanted rice.


``` r
bb <- predict_bacterial_blight(wth, emergence = "2000-07-01")

bb
#>      simday      dates     sites    latent infectious  removed    senesced  rateinf rtransfer
#>       <int>     <Date>     <num>     <num>      <num>    <num>       <num>    <num>     <num>
#>   1:      1 2000-07-01  100.0000   0.00000     0.0000   0.0000    0.000000  0.00000   0.00000
#>   2:      2 2000-07-02  108.6875   0.00000     0.0000   0.0000    1.000000  0.00000   0.00000
#>   3:      3 2000-07-03  118.1002   0.00000     0.0000   0.0000    2.086875  0.00000   0.00000
#>   4:      4 2000-07-04  128.2934   0.00000     0.0000   0.0000    3.267877  0.00000   0.00000
#>   5:      5 2000-07-05  139.3254   0.00000     0.0000   0.0000    4.550811  0.00000   0.00000
#>  ---                                                                                         
#> 116:    116 2000-10-24 1442.4836 105.87923   954.9354 223.8352 2097.385647  0.00000  29.85591
#> 117:    117 2000-10-25 1418.0978  76.02332   953.5148 255.1118 2143.087067 23.19935  26.80440
#> 118:    118 2000-10-26 1369.3763  72.41827   946.9419 288.4890 2190.645270 20.80752  25.36284
#> 119:    119 2000-10-27 1321.9560  67.86296   937.0147 323.7791 2239.629140 18.61122  23.85609
#> 120:    120 2000-10-28 1275.5526  62.61810   923.6023 361.0475 2290.117140 16.58631   0.00000
#>       rgrowth rsenesced diseased intensity    AUDPC     lat      lon
#>         <num>     <num>    <num>     <num>    <num>   <num>    <num>
#>   1:  9.68750  1.000000    0.000 0.0000000 12.53946 14.6774 121.2556
#>   2: 10.49959  1.086875    0.000 0.0000000 12.53946 14.6774 121.2556
#>   3: 11.37416  1.181002    0.000 0.0000000 12.53946 14.6774 121.2556
#>   4: 12.31499  1.282934    0.000 0.0000000 12.53946 14.6774 121.2556
#>   5: 13.32593  1.393254    0.000 0.0000000 12.53946 14.6774 121.2556
#>  ---                                                                
#> 116: 21.31570 45.701419 1284.650 0.4237668 12.53946 14.6774 121.2556
#> 117: 22.03601 47.558203 1284.650 0.4206255 12.53946 14.6774 121.2556
#> 118: 22.37109 48.983870 1307.849 0.4267361 12.53946 14.6774 121.2556
#> 119: 22.69581 50.488000 1328.657 0.4318648 12.53946 14.6774 121.2556
#> 120: 23.00696 47.881728 1347.268 0.4360386 12.53946 14.6774 121.2556
```

Lastly, you can visualise the result of the model run.


``` r
library("ggplot2")

ggplot(
  data = bb,
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

## Code of Conduct

Please note that the epicrop project is released with a [Contributor Code of Conduct](https://codeberg.org/adamhsparks/epicrop/src/branch/main/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

# References

Robert J. Hijmans, Serge Savary, Rene Pangga and Jorrel Aunario.
(2009).
Simulation modeling of crops and their diseases.
R package version 0.2-6.

Serge Savary, Andrew Nelson, Laetitia Willocquet, Ireneo Pangga and Jorrel Aunario.(2012).
Modeling and mapping potential epidemics of rice diseases globally.
*Crop Protection*, Volume 34, Pages 6-17, ISSN 0261-2194 DOI: [10.1016/j.cropro.2011.11.009](https://doi.org/10.1016/j.cropro.2011.11.009).

Serge Savary, Stacia Stetkiewicz, François Brun, and Laetitia Willocquet.
Modelling and Mapping Potential Epidemics of Wheat Diseases-Examples on Leaf Rust and Septoria Tritici Blotch Using EPIWHEAT.
(2015).
*European Journal of Plant Pathology* 142, no. 4:771–90.
DOI: [10.1007/s10658-015-0650-7](https://doi.org/10.1007/s10658-015-0650-7).

Adam Sparks (2018).
nasapower: A NASA POWER Global Meteorology, Surface Solar Energy and Climatology Data Client for R.
*Journal of Open Source Software*, 3(30), 1035, DOI: [10.21105/joss.01035](https://doi.org/10.21105/joss.01035).

Adam Sparks (2021).
*nasapower: NASA-POWER Data from R*.
DOI: [10.5281/zenodo.1040727](https://doi.org/10.5281/zenodo.1040727), R package version 4.0.0, URL: <https://CRAN.R-project.org/package=nasapower>.

Jan C. Zadoks.
(1971).
Systems Analysis and the Dynamics of Epidemics.
*Phytopathology* 61:600.
DOI: [10.1094/Phyto-61-600](https://doi.org/10.1094/Phyto-61-600).
