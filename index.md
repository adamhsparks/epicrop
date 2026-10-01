# Simulation Modelling of Crop Diseases Using a Susceptible-Exposed-Infectious-Removed (SEIR) Model

A fork of
[{cropsim}](https://github.com/r-forge/cropsim/tree/master/pkg/cropsim)
(Hijmans *et al.* 2009) designed to make using the EPIRICE model (Savary
*et al.* 2012) for rice diseases easier to use and to implement the
EPIWHEAT model in the same R package. This package provides easy to use
functions to fetch weather data from NASA POWER, via the
[{nasapower}](https://cran.r-project.org/package=nasapower) package
(Sparks 2018) and ERA5-Land data (Muñoz-Sabater et al. 2021) (built in
courtesy of Emerson Del Ponte) via
[Open-Meteo](https://open-meteo.com/en/docs/historical-weather-api) and
predict disease intensity of five rice and two wheat diseases using a
generic Susceptible-Exposed-Infectious-Removed (SEIR) model (Zadoks
1971) function,
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md).

This package fully implements the EPIRICE model first published in
Savary *et al.* (2012), which introduces the model and uses it to model
global epidemics of rice diseases illustrating the risk of bacterial
blight, brown spot, leaf blast, sheath blight and rice tungro disease.
Additional models included for rice are adapted from Kim *et al.* (2015)
for rice sheath blight and leaf blast. The EPIWHEAT model first
published in Savary *et al.* (2015) that was used to model potential
epidemics of two wheat diseases, leaf rust and Septoria tritici blotch,
is also included in this package.

The modern core of
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
in {epicrop} uses a different numerical integration approach than the
legacy `SEIR()` in {cropsim}. While both implement the canonical SEIR
formula (diseased = latent + infectious + removed), the modern version
uses cumulative delay windows instead of forward Euler integration. This
results in different dynamics and typically lower AUDPC values.
Additionally, the modern
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
is 1-indexed (standard R convention) while the legacy `SEIR()` is
0-indexed (as in the original EPIRICE publication by Savary *et al.*
2012).

Scripts are available in `inst` that benchmark and validate this version
against the original EPIRICE. As the original EPIWHEAT code is not
available, no such comparisons are possible. I have recreated this as
closely as possible to the original, while fixing minor bugs and
implementing more efficient routines. In benchmarking this iteration is
up to 57% faster in seasonal disease calculations than the original
EPIRICE. This varies by OS and hardware, but the gains should be notable
across large datasets, *e.g.*, several years of global data as in Savary
*et al.* 2012.

Users may also provide their own parameters to the generic
[`seir()`](https://adamhsparks.codeberg.page/epicrop/reference/seir.md)
function to simulate other crop diseases as long as the weather data
provided meet the model’s requirements.

# Quick start

{epicrop} is not yet on CRAN. You can install it this way.

\
[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(`\
`  ``"epicrop"``,`\
`  repos ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"https://adamhsparks.r-universe.dev"``, ``"https://cloud.r-project.org"``)`\
`)`

## Get weather data

First you need to provide weather data for the model; {epicrop} provides
the
[`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md)
function to do this. Using it with the POWER source you can fetch
weather data for any place in the world from 1983 to near present by
providing the and latitude and dates or length of rice growing season as
shown below.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`epicrop`](https://codeberg.org/adamhsparks/epicrop)`)`\
\
`# Fetch weather for year 2000 wet season for a 120 day rice variety at the IRRI`\
`# Zeigler Experiment Station for both weather sources`\
\
`sources`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"power"``, ``"era5"``)`\
\
`wth`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(`\
`  X ``=`` ``sources``,`\
`  FUN ``=`` ``get_wth``,`\
`  lonlat ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``121.25562``, ``14.6774``)``,`\
`  dates ``=`` ``"2000-07-01"``,`\
`  duration ``=`` ``120`\
`  ``)`\
\
`wth`` ``<-`` `[`get_wth`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md)`(`\
`  ``vals``,`\
`  sources ``=`` ``"power"`\
`  ``)`\
`` #> Error in `get_wth()`: ``\
`#> ! unused argument (sources = "power")`\
\
[`names`](https://rdrr.io/r/base/names.html)`(``wth``)`` ``<-`` ``sources`\
\
`wth`\
`#> $power`\
`#> Key: <YYYYMMDD>`\
`#>        YYYYMMDD   DOY  TEMP  TMIN  TMAX  RHUM`\
`#>          <IDat> <int> <num> <num> <num> <num>`\
`#>   1: 2000-07-01   183 25.29 23.86 27.78 92.20`\
`#>   2: 2000-07-02   184 26.13 23.54 29.90 86.01`\
`#>   3: 2000-07-03   185 25.50 24.28 27.23 94.16`\
`#>   4: 2000-07-04   186 25.81 24.50 27.56 92.42`\
`#>   5: 2000-07-05   187 25.97 25.13 27.40 92.34`\
`#>  ---                                         `\
`#> 116: 2000-10-24   298 25.81 23.54 29.72 87.65`\
`#> 117: 2000-10-25   299 25.82 23.44 29.54 89.76`\
`#> 118: 2000-10-26   300 25.44 24.14 26.99 94.93`\
`#> 119: 2000-10-27   301 25.74 24.54 27.69 91.43`\
`#> 120: 2000-10-28   302 25.44 24.72 26.62 91.90`\
`#>       RAIN     LAT      LON`\
`#>      <num>   <num>    <num>`\
`#>   1: 23.12 14.6774 121.2556`\
`#>   2: 17.34 14.6774 121.2556`\
`#>   3: 29.08 14.6774 121.2556`\
`#>   4: 13.01 14.6774 121.2556`\
`#>   5: 32.20 14.6774 121.2556`\
`#>  ---                       `\
`#> 116:  2.76 14.6774 121.2556`\
`#> 117: 12.04 14.6774 121.2556`\
`#> 118: 13.03 14.6774 121.2556`\
`#> 119: 11.54 14.6774 121.2556`\
`#> 120: 74.20 14.6774 121.2556`\
`#> `\
`#> $era5`\
`#> Key: <YYYYMMDD>`\
`#>        YYYYMMDD   DOY     TEMP  TMIN  TMAX`\
`#>          <IDat> <int>    <num> <num> <num>`\
`#>   1: 2000-07-01   183 24.70417  23.2  27.9`\
`#>   2: 2000-07-02   184 24.18333  23.2  26.5`\
`#>   3: 2000-07-03   185 23.72917  22.9  25.8`\
`#>   4: 2000-07-04   186 23.19167  22.8  23.5`\
`#>   5: 2000-07-05   187 23.65833  23.1  24.6`\
`#>  ---                                      `\
`#> 116: 2000-10-24   298 25.76250  23.2  29.2`\
`#> 117: 2000-10-25   299 25.58333  22.9  29.2`\
`#> 118: 2000-10-26   300 25.20000  23.8  27.9`\
`#> 119: 2000-10-27   301 25.27083  23.9  27.2`\
`#> 120: 2000-10-28   302 23.87917  23.2  24.4`\
`#>          RHUM  RAIN      LAT      LON`\
`#>         <num> <num>    <num>    <num>`\
`#>   1: 89.29167  28.8 14.65729 121.2825`\
`#>   2: 91.04167  27.9 14.65729 121.2825`\
`#>   3: 93.25000  43.3 14.65729 121.2825`\
`#>   4: 96.25000  96.0 14.65729 121.2825`\
`#>   5: 95.75000  86.9 14.65729 121.2825`\
`#>  ---                                 `\
`#> 116: 83.50000   0.1 14.65729 121.2825`\
`#> 117: 82.54167   1.0 14.65729 121.2825`\
`#> 118: 85.58333   5.7 14.65729 121.2825`\
`#> 119: 85.08333   2.8 14.65729 121.2825`\
`#> 120: 91.58333 162.6 14.65729 121.2825`

## Modelling bacterial blight disease intensity

Once you have the weather data, run the model for any of the five rice
diseases by providing the emergence or crop establishment date for
transplanted rice.

\
`bb_sim`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``X ``=`` ``wth``, FUN ``=`` ``bacterial_blight``, emergence ``=`` ``"2000-07-01"``)`\
\
`bb_sim`\
`#> $power`\
`#>      simday      dates    sites latent`\
`#>       <int>     <IDat>    <num>  <num>`\
`#>   1:      1 2000-07-01 100.0000      0`\
`#>   2:      2 2000-07-02 108.6875      0`\
`#>   3:      3 2000-07-03 118.1002      0`\
`#>   4:      4 2000-07-04 128.2934      0`\
`#>   5:      5 2000-07-05 139.3254      0`\
`#>  ---                                  `\
`#> 116:    116 2000-10-24   0.0000      0`\
`#> 117:    117 2000-10-25   0.0000      0`\
`#> 118:    118 2000-10-26   0.0000      0`\
`#> 119:    119 2000-10-27   0.0000      0`\
`#> 120:    120 2000-10-28   0.0000      0`\
`#>      infectious  removed    senesced rateinf`\
`#>           <num>    <num>       <num>   <num>`\
`#>   1:     0.0000    0.000    1.000000       0`\
`#>   2:     0.0000    0.000    2.086875       0`\
`#>   3:     0.0000    0.000    3.267877       0`\
`#>   4:     0.0000    0.000    4.550811       0`\
`#>   5:     0.0000    0.000    5.944065       0`\
`#>  ---                                        `\
`#> 116:   396.0528 1404.565 2665.037553       0`\
`#> 117:   356.3610 1444.257 2712.120592       0`\
`#> 118:   309.2779 1491.340 2755.012575       0`\
`#> 119:   266.3860 1534.232 2791.854071       0`\
`#> 120:   229.5445 1571.073 2824.358958       0`\
`#>       rlex rtransfer rremoved  rgrowth rsenesced`\
`#>      <num>     <num>    <num>    <num>     <num>`\
`#>   1:     0         0  0.00000  9.68750  1.000000`\
`#>   2:     0         0  0.00000 10.49959  1.086875`\
`#>   3:     0         0  0.00000 11.37416  1.181002`\
`#>   4:     0         0  0.00000 12.31499  1.282934`\
`#>   5:     0         0  0.00000 13.32593  1.393254`\
`#>  ---                                            `\
`#> 116:     0         0 43.67864  0.00000 39.691810`\
`#> 117:     0         0 39.69181  0.00000 47.083039`\
`#> 118:     0         0 47.08304  0.00000 42.891982`\
`#> 119:     0         0 42.89198  0.00000 36.841497`\
`#> 120:     0         0 36.84150  0.00000 32.504887`\
`#>      diseased intensity     lat      lon`\
`#>         <num>     <num>   <num>    <num>`\
`#>   1:    0.000         0 14.6774 121.2556`\
`#>   2:    0.000         0 14.6774 121.2556`\
`#>   3:    0.000         0 14.6774 121.2556`\
`#>   4:    0.000         0 14.6774 121.2556`\
`#>   5:    0.000         0 14.6774 121.2556`\
`#>  ---                                    `\
`#> 116: 1800.618         1 14.6774 121.2556`\
`#> 117: 1800.618         1 14.6774 121.2556`\
`#> 118: 1800.618         1 14.6774 121.2556`\
`#> 119: 1800.618         1 14.6774 121.2556`\
`#> 120: 1800.618         1 14.6774 121.2556`\
`#> `\
`#> $era5`\
`#>      simday      dates    sites latent`\
`#>       <int>     <IDat>    <num>  <num>`\
`#>   1:      1 2000-07-01 100.0000      0`\
`#>   2:      2 2000-07-02 108.6875      0`\
`#>   3:      3 2000-07-03 118.1002      0`\
`#>   4:      4 2000-07-04 128.2934      0`\
`#>   5:      5 2000-07-05 139.3254      0`\
`#>  ---                                  `\
`#> 116:    116 2000-10-24   0.0000      0`\
`#> 117:    117 2000-10-25   0.0000      0`\
`#> 118:    118 2000-10-26   0.0000      0`\
`#> 119:    119 2000-10-27   0.0000      0`\
`#> 120:    120 2000-10-28   0.0000      0`\
`#>      infectious  removed    senesced rateinf`\
`#>           <num>    <num>       <num>   <num>`\
`#>   1:    0.00000    0.000    1.000000       0`\
`#>   2:    0.00000    0.000    2.086875       0`\
`#>   3:    0.00000    0.000    3.267877       0`\
`#>   4:    0.00000    0.000    4.550811       0`\
`#>   5:    0.00000    0.000    5.944065       0`\
`#>  ---                                        `\
`#> 116:  101.72614 1627.765 2639.222092       0`\
`#> 117:   76.27393 1653.217 2639.222092       0`\
`#> 118:   76.27393 1653.217 2639.222092       0`\
`#> 119:   76.27393 1653.217 2639.222092       0`\
`#> 120:   76.27393 1653.217 2639.222092       0`\
`#>       rlex rtransfer rremoved  rgrowth rsenesced`\
`#>      <num>     <num>    <num>    <num>     <num>`\
`#>   1:     0         0  0.00000  9.68750  1.000000`\
`#>   2:     0         0  0.00000 10.49959  1.086875`\
`#>   3:     0         0  0.00000 11.37416  1.181002`\
`#>   4:     0         0  0.00000 12.31499  1.282934`\
`#>   5:     0         0  0.00000 13.32593  1.393254`\
`#>  ---                                            `\
`#> 116:     0         0 26.44418  0.00000 25.452212`\
`#> 117:     0         0 25.45221  0.00000  0.000000`\
`#> 118:     0         0  0.00000  0.00000  0.000000`\
`#> 119:     0         0  0.00000  0.00000  0.000000`\
`#> 120:     0         0  0.00000  0.00000  0.000000`\
`#>      diseased intensity      lat      lon`\
`#>         <num>     <num>    <num>    <num>`\
`#>   1:    0.000         0 14.65729 121.2825`\
`#>   2:    0.000         0 14.65729 121.2825`\
`#>   3:    0.000         0 14.65729 121.2825`\
`#>   4:    0.000         0 14.65729 121.2825`\
`#>   5:    0.000         0 14.65729 121.2825`\
`#>  ---                                     `\
`#> 116: 1729.491         1 14.65729 121.2825`\
`#> 117: 1729.491         1 14.65729 121.2825`\
`#> 118: 1729.491         1 14.65729 121.2825`\
`#> 119: 1729.491         1 14.65729 121.2825`\
`#> 120: 1729.491         1 14.65729 121.2825`

Lastly, you can visualise the result of the model run.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`ggplot2`](https://ggplot2.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`data.table`](https://r-datatable.com)`)`\
\
`# use rbindlist from {data.table} to create a single df for plotting`\
`bb_sim`` ``<-`` `[`rbindlist`](https://rdrr.io/pkg/data.table/man/rbindlist.html)`(``bb_sim``, idcol ``=`` ``"source"``)`\
\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(`\
`  data ``=`` ``bb_sim``,`\
`  `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(`\
`    x ``=`` ``dates``,`\
`    y ``=`` ``intensity``,`\
`    colour ``=`` ``source`\
`  ``)`\
`)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(`\
`    y ``=`` ``"Intensity"``,`\
`    x ``=`` ``"Date"`\
`  ``)`` ``+`\
`  `[`geom_line`](https://ggplot2.tidyverse.org/reference/geom_path.html)`(``)`` ``+`\
`  `[`geom_point`](https://ggplot2.tidyverse.org/reference/geom_point.html)`(``)`` ``+`\
`  `[`theme_classic`](https://ggplot2.tidyverse.org/reference/ggtheme.html)`(``)`

![Bacterial blight disease progress over time. Results for wet season
year 2000 at IRRI Zeigler Experiment Station shown. Weather data used to
run the model were obtained from the NASA Langley Research Center POWER
Project funded through the NASA Earth Science Directorate Applied
Science Program.](reference/figures/README-plot-1.png)

Bacterial blight disease progress over time. Results for wet season year
2000 at IRRI Zeigler Experiment Station shown. Weather data used to run
the model were obtained from the NASA Langley Research Center POWER
Project funded through the NASA Earth Science Directorate Applied
Science Program.

# Meta

- Please [report any issues or
  bugs](https://codeberg.org/adamhsparks/epicrop/issues).

- License: GPL-3

- To cite {epicrop}, please use the output from
  `citation(package = "epicrop")`.

## Code Coverage

    #> epicrop Coverage: 95.74%
    #> R/interpolation.R: 80.34%
    #> R/get_wth.R: 83.85%
    #> R/validation.R: 84.62%
    #> R/format_wth.R: 91.18%
    #> R/leaf_wetness.R: 97.54%
    #> R/audpc.R: 97.83%
    #> R/run_epicrop_model.R: 99.26%
    #> R/seir.R: 99.32%
    #> R/build_epicrop_emergence.R: 100.00%
    #> R/disease_models.R: 100.00%
    #> R/epi_get_params.R: 100.00%
    #> R/fetch_epicrop_weather_list.R: 100.00%

## Code of Conduct

Please note that the epicrop project is released with a [Contributor
Code of
Conduct](https://codeberg.org/adamhsparks/epicrop/src/branch/main/CODE_OF_CONDUCT.md).
By contributing to this project, you agree to abide by its terms.

## Other Implementations in R

- The EPIRICE model was originally written in R as a part of the
  [{cropsim}](https://github.com/r-forge/cropsim/tree/master/pkg/cropsim)
  package (Hijmans *et al.* 2009).

- The EPIRICE model is also available from CRAN in the
  [{ZeBook}](https://cran.r-project.org/package=ZeBook) package (Brun
  *et al.* 2018) to accompany, “Working with dynamic crop models:
  methods, tools and examples for agriculture and environment” (Wallach
  *et al.* 2018).

# References

François Brun, David Makowski, Daniel Wallach, and James W Jones.
(2018). *ZeBook: Working with Dynamic Models for Agriculture and
Environment*. DOI:
[10.32614/CRAN.package.ZeBook](https://doi.org/10.32614/CRAN.package.ZeBook)
R package version 1.1, <https://CRAN.R-project.org/package=ZeBook>.

Kwang-Hyung Kim, Jaepil Cho, Yong Hwan Lee, and Woo-Seop Lee. (2015).
Predicting potential epidemics of rice leaf blast and sheath blight in
South Korea. *Agricultural and Forest Meteorology*, 203: 191-207. DOI:
[10.1016/j.agrformet.2015.01.011](https://doi.org/10.1016/j.agrformet.2015.01.011)

Robert J Hijmans, Serge Savary, Rene Pangga and Jorrel Aunario.
*cropsim*. (2009). Simulation modeling of crops and their diseases. R
package version 0.2-6.

Joaquín Muñoz-Sabater, Emanuel Dutra, Anna Agustí-Panareda, Clément
Albergel, Gabriele Arduini, Gianpaolo Balsamo, Souhail Boussetta,
Margarita Choulga, Shaun Harrigan, Hans Hersbach, Brecht Martens, Diego
G. Miralles, María Piles, Nemesio J. Rodríguez-Fernández, Ervin Zsoter,
Carlo Buontempo, and Jean-Noël Thépaut (2021). ERA5-Land: a
state-of-the-art global reanalysis dataset for land applications. Earth
System Science Data, 13, 4349-4383. .

Serge Savary, Andrew Nelson, Laetitia Willocquet, Ireneo Pangga and
Jorrel Aunario. (2012). Modeling and mapping potential epidemics of rice
diseases globally. *Crop Protection*, Volume 34, Pages 6-17, ISSN
0261-2194 DOI:
[10.1016/j.cropro.2011.11.009](https://doi.org/10.1016/j.cropro.2011.11.009).

Serge Savary, Stacia Stetkiewicz, François Brun, and Laetitia
Willocquet. Modelling and Mapping Potential Epidemics of Wheat
Diseases-Examples on Leaf Rust and Septoria Tritici Blotch Using
EPIWHEAT. (2015). *European Journal of Plant Pathology* 142, no.
4:771–90. DOI:
[10.1007/s10658-015-0650-7](https://doi.org/10.1007/s10658-015-0650-7).

Adam Sparks. (2018). nasapower: A NASA POWER Global Meteorology, Surface
Solar Energy and Climatology Data Client for R. *Journal of Open Source
Software*, 3(30), 1035, DOI:
[10.21105/joss.01035](https://doi.org/10.21105/joss.01035).

Jan C Zadoks. (1971). Systems Analysis and the Dynamics of Epidemics.
*Phytopathology* 61:600. DOI:
[10.1094/Phyto-61-600](https://doi.org/10.1094/Phyto-61-600).

Wallach, Daniel, David Makowski, James W Jones, and François Brun.
(2018) *Working with dynamic crop models: methods, tools and examples
for agriculture and environment*. Academic Press. DOI:
[10.1016/C2016-0-01552-8](https://doi.org/10.1016/C2016-0-01552-8).
