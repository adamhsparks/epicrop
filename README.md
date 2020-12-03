_epirice_: Simulation Modelling of Rice Diseases <img align="right" src="man/figures/logo.png">
================
<!-- badges: start -->
[![tic](https://github.com/adamhsparks/epirice/workflows/tic/badge.svg?branch=main)](https://github.com/adamhsparks/epirice/actions)
[![codecov](https://codecov.io/gh/adamhsparks/epirice/branch/main/graph/badge.svg?token=NWrKsX9MaP)](https://codecov.io/gh/adamhsparks/epirice)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
<!-- badges: end -->

A fork of the R [cropsim
package](https://r-forge.r-project.org/R/?group_id=335) designed to make using the EPIRICE model for rice diseases easier to use.
This version provides easy to use functions to fetch weather data from NASA POWER, via the [*nasapower*](https://cran.r-project.org/package=nasapower) package and predict disease severity of five rice diseases using a generic SEIR model (Zadoks 1971) function, `SEIR()`.

The original manuscript, Savary et al. (2012), which details the model and results of its use to model global epidemics of rice diseases was published in Crop Protection detailing global disease risk of five major rice diseases, bacterial blight, brown spot, leaf blast, sheath blight and tungro.

# Installation

`epirice` is not yet on CRAN.
You can install it this way.

``` r
if (!require("remotes"))
  install.packages("remotes")
remotes::install_github("adamshsparks/epirice"
)
```

# Meta

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md).
By participating in this project you agree to abide by its terms.

# References

Serge Savary, Andrew Nelson, Laetitia Willocquet, Ireneo Pangga and Jorrel Aunario. Modeling and mapping potential epidemics of rice diseases globally. *Crop Protection*, Volume 34, 2012, Pages 6-17, ISSN 0261-2194 DOI: [10.1016/j.cropro.2011.11.009](https://doi.org/10.1016/j.cropro.2011.11.009).

Jan C. Zadoks. Systems Analysis and the Dynamics of Epidemics. Laboratory of Phytopathology, Agricultural University, Wageningen, The Netherlands; *Phytopathology* 61:600. DOI: [10.1094/Phyto-61-600](https://doi.org/10.1094/Phyto-61-600).
