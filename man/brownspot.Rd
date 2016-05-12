\name{brownSpot}
\alias{brownSpot}

\title{Rice brown spot disease}

\description{
A dynamic mechanistic simulation of rice brown spot disease. The model is driven by daily weather data. 
}

\usage{
brownSpot(wth, emergence='2000-05-15', ...) 
}

\arguments{
  \item{wth}{daily weather data frame containing relative humidity (relh), minimum temeperature (tmin), and maximum temperature (tmax)}
  \item{emergence}{expected date of plant emergence}
  \item{...}{additional arguments. See under }
}

\value{
 An object of class SEIR
}

\references{ 
Klomp, A.O., 1977. Early senescence of rice and Dreschslera oryzae in the Wageningen polder, Surinam. PhD thesis, 97p.

Levy, Y. and Cohen, Y., 1980. Sporulation of Helminthosporium turcicum on sweet corn: Effects of temperature and dew period. Canadian Journal of Plant Pathology 2:65-69.

Padmanabhan, S.Y. and Ganguly, D. 1954. Relation between the age of rice plant and its susceptibility to Helminthosporium and blast disease. Proceedings of the Indian Academy of Sciences B 29:44-50.

Sarkar, A.K. and Sen Gupta, P.K., 1977. Effect of temperature and humidity on disease development and sporulation of Helminthosporium oryzae on rice. Indian Phytopathology 30:258-259.

Luo Wei-Hong, 1996. Simulation  and measurement of leaf wetness formation in paddy rice crops. PhD, Wageningen Agricultural University, 87 p.

Waggoner. P.E., Horsfall, J.G., and Lukens, R.J. 1972. EPIMAY. A Simulator pf Southern Corn Leaf Blight. Bulletin of the Connecticut Experiment Station, New Haven, 85 p.
}

\author{Serge Savary, Ireneo Pangga, Robert Hijmans, Jorrel Khalil Aunario }


\examples{
wth <- getWthFile(system.file("weather/daily_weather_28368.nasa", package = "cropsim"))
res <- brownSpot(wth, onset=20, duration=120)
plot(res, type=2)
}

\keyword{ spatial }
