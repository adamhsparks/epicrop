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
`#> # Data table (class data.table) 9 x 120:`\
`#> # (Showing rows 1 - 20 out of 120)`\
`#>   │YYYYMMDD  │DOY  │TEMP │TMIN │TMAX │RHUM `\
`#>   │<oth>     │<int>│<dbl>│<dbl>│<dbl>│<dbl>`\
`#>  1│2000-07-01│  183│   25│   24│   28│   92`\
`#>  2│2000-07-02│  184│   26│   24│   30│   86`\
`#>  3│2000-07-03│  185│   26│   24│   27│   94`\
`#>  4│2000-07-04│  186│   26│   24│   28│   92`\
`#>  5│2000-07-05│  187│   26│   25│   27│   92`\
`#>  6│2000-07-06│  188│   26│   25│   28│   89`\
`#>  7│2000-07-07│  189│   26│   25│   27│   91`\
`#>  8│2000-07-08│  190│   26│   25│   28│   91`\
`#>  9│2000-07-09│  191│   26│   25│   27│   93`\
`#> 10│2000-07-10│  192│   26│   24│   27│   94`\
`#> 11│2000-07-11│  193│   26│   24│   29│   86`\
`#> 12│2000-07-12│  194│   25│   23│   27│   90`\
`#> 13│2000-07-13│  195│   25│   23│   26│   95`\
`#> 14│2000-07-14│  196│   25│   24│   27│   94`\
`#> 15│2000-07-15│  197│   25│   24│   26│   93`\
`#> 16│2000-07-16│  198│   26│   24│   29│   86`\
`#> 17│2000-07-17│  199│   26│   23│   28│   88`\
`#> 18│2000-07-18│  200│   25│   23│   28│   86`\
`#> 19│2000-07-19│  201│   25│   23│   29│   83`\
`#> 20│2000-07-20│  202│   26│   23│   29│   78`\
`#> # 3 more columns: RAIN (<dbl>), LAT (<dbl>), LON (<dbl>)`\
`#> `\
`#> $era5`\
`#> # Data table (class data.table) 9 x 120:`\
`#> # (Showing rows 1 - 20 out of 120)`\
`#>   │YYYYMMDD  │DOY  │TEMP │TMIN │TMAX │RHUM `\
`#>   │<oth>     │<int>│<dbl>│<dbl>│<dbl>│<dbl>`\
`#>  1│2000-07-01│  183│   25│   23│   28│   89`\
`#>  2│2000-07-02│  184│   24│   23│   26│   91`\
`#>  3│2000-07-03│  185│   24│   23│   26│   93`\
`#>  4│2000-07-04│  186│   23│   23│   24│   96`\
`#>  5│2000-07-05│  187│   24│   23│   25│   96`\
`#>  6│2000-07-06│  188│   24│   23│   26│   94`\
`#>  7│2000-07-07│  189│   24│   23│   26│   93`\
`#>  8│2000-07-08│  190│   24│   23│   25│   93`\
`#>  9│2000-07-09│  191│   24│   23│   26│   93`\
`#> 10│2000-07-10│  192│   24│   23│   27│   92`\
`#> 11│2000-07-11│  193│   25│   23│   29│   85`\
`#> 12│2000-07-12│  194│   24│   23│   27│   90`\
`#> 13│2000-07-13│  195│   24│   22│   25│   95`\
`#> 14│2000-07-14│  196│   23│   23│   24│   95`\
`#> 15│2000-07-15│  197│   24│   22│   26│   92`\
`#> 16│2000-07-16│  198│   26│   23│   29│   83`\
`#> 17│2000-07-17│  199│   26│   23│   29│   80`\
`#> 18│2000-07-18│  200│   25│   23│   28│   84`\
`#> 19│2000-07-19│  201│   24│   23│   27│   90`\
`#> 20│2000-07-20│  202│   24│   22│   27│   89`\
`#> # 3 more columns: RAIN (<dbl>), LAT (<dbl>), LON (<dbl>)`

## Modelling bacterial blight disease intensity

Once you have the weather data, run the model for any of the five rice
diseases by providing the emergence or crop establishment date for
transplanted rice.

\
`bb_sim`` ``<-`` `[`lapply`](https://rdrr.io/r/base/lapply.html)`(``X ``=`` ``wth``, FUN ``=`` ``bacterial_blight``, emergence ``=`` ``"2000-07-01"``)`\
\
`bb_sim`\
`#> $power`\
`#> # Data table (class data.table) 17 x 120:`\
`#> # (Showing rows 1 - 20 out of 120)`\
`#>   │simday│dates     │sites│latent│infectious`\
`#>   │<int> │<oth>     │<dbl>│<dbl> │<dbl>     `\
`#>  1│     1│2000-07-01│  100│     0│         0`\
`#>  2│     2│2000-07-02│  109│     0│         0`\
`#>  3│     3│2000-07-03│  118│     0│         0`\
`#>  4│     4│2000-07-04│  128│     0│         0`\
`#>  5│     5│2000-07-05│  139│     0│         0`\
`#>  6│     6│2000-07-06│  151│     0│         0`\
`#>  7│     7│2000-07-07│  164│     0│         0`\
`#>  8│     8│2000-07-08│  178│     0│         0`\
`#>  9│     9│2000-07-09│  193│     0│         0`\
`#> 10│    10│2000-07-10│  209│     0│         0`\
`#> 11│    11│2000-07-11│  227│     0│         0`\
`#> 12│    12│2000-07-12│  246│     0│         0`\
`#> 13│    13│2000-07-13│  266│     0│         0`\
`#> 14│    14│2000-07-14│  288│     0│         0`\
`#> 15│    15│2000-07-15│  311│     0│         0`\
`#> 16│    16│2000-07-16│  336│     0│         0`\
`#> 17│    17│2000-07-17│  363│     0│         0`\
`#> 18│    18│2000-07-18│  391│     0│         0`\
`#> 19│    19│2000-07-19│  421│     0│         0`\
`#> 20│    20│2000-07-20│  454│     0│         0`\
`#> # 12 more columns: removed (<dbl>), senesced (<dbl>), rateinf (<dbl>), rlex (<dbl>), rtransfer (<dbl>), rremoved (<dbl>), rgrowth (<dbl>), rsenesced (<dbl>), diseased (<dbl>), intensity (<dbl>), lat (<dbl>), lon (<dbl>)`\
`#> `\
`#> $era5`\
`#> # Data table (class data.table) 17 x 120:`\
`#> # (Showing rows 1 - 20 out of 120)`\
`#>   │simday│dates     │sites│latent│infectious`\
`#>   │<int> │<oth>     │<dbl>│<dbl> │<dbl>     `\
`#>  1│     1│2000-07-01│  100│     0│         0`\
`#>  2│     2│2000-07-02│  109│     0│         0`\
`#>  3│     3│2000-07-03│  118│     0│         0`\
`#>  4│     4│2000-07-04│  128│     0│         0`\
`#>  5│     5│2000-07-05│  139│     0│         0`\
`#>  6│     6│2000-07-06│  151│     0│         0`\
`#>  7│     7│2000-07-07│  164│     0│         0`\
`#>  8│     8│2000-07-08│  178│     0│         0`\
`#>  9│     9│2000-07-09│  193│     0│         0`\
`#> 10│    10│2000-07-10│  209│     0│         0`\
`#> 11│    11│2000-07-11│  227│     0│         0`\
`#> 12│    12│2000-07-12│  246│     0│         0`\
`#> 13│    13│2000-07-13│  266│     0│         0`\
`#> 14│    14│2000-07-14│  288│     0│         0`\
`#> 15│    15│2000-07-15│  311│     0│         0`\
`#> 16│    16│2000-07-16│  336│     0│         0`\
`#> 17│    17│2000-07-17│  363│     0│         0`\
`#> 18│    18│2000-07-18│  391│     0│         0`\
`#> 19│    19│2000-07-19│  421│     0│         0`\
`#> 20│    20│2000-07-20│  454│     0│         0`\
`#> # 12 more columns: removed (<dbl>), senesced (<dbl>), rateinf (<dbl>), rlex (<dbl>), rtransfer (<dbl>), rremoved (<dbl>), rgrowth (<dbl>), rsenesced (<dbl>), diseased (<dbl>), intensity (<dbl>), lat (<dbl>), lon (<dbl>)`

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
Science Program, and the Copernicus Climate Change Service (2022):
ERA5-Land hourly data from 1950 to present. Copernicus Climate Change
Service (C3S) Climate Data Store (CDS), DOI: 10.24381/cds.e2161bac
(Accessed on 2026-10-02).](reference/figures/README-plot-1.png)

Bacterial blight disease progress over time. Results for wet season year
2000 at IRRI Zeigler Experiment Station shown. Weather data used to run
the model were obtained from the NASA Langley Research Center POWER
Project funded through the NASA Earth Science Directorate Applied
Science Program, and the Copernicus Climate Change Service (2022):
ERA5-Land hourly data from 1950 to present. Copernicus Climate Change
Service (C3S) Climate Data Store (CDS), DOI: 10.24381/cds.e2161bac
(Accessed on 2026-10-02).

# Meta

- Please [report any issues or
  bugs](https://codeberg.org/adamhsparks/epicrop/issues).

- License: GPL-3

- To cite {epicrop}, please use the output from
  `citation(package = "epicrop")`.

## Code Coverage

    #> epicrop Coverage: 96.04%
    #> R/interpolation.R: 80.34%
    #> R/validation.R: 84.62%
    #> R/get_wth.R: 86.96%
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
