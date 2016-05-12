\name{sheathBlight}

\alias{sheathBlight}

\title{Rice sheath blight disease }

\description{
 A dynamic mechanistic simulation of rice sheath blight disease. The model is driven by daily weather data. 
}

\usage{
sheathBlight(wth, emergence='2000-05-15', ...) 
}

\arguments{
  \item{wth}{daily weather data frame containing relative humidity (relh), minimum temeperature (tmin), and maximum temperature (tmax)}
  \item{emergence}{expected date of plant emergence}
  \item{...}{additional arguments. See \link[cropsim]{SEIR} }
}


\value{
  An object of class SEIR
}


\references{ 
Savary, S., Castilla, N.P., Willocquet, L. 2001. Analysis of the spatio-temporal structure of rice sheath blight epidemics in a farmer's field. Plant Pathology 50:53-68.

Savary, S., Willocquet, L., Teng, P.S., 1997. Modelling sheath blight epidemics on rice tillers. Agricultural Systems 55:359-384.

Castilla, N.P., Leano, R.M., Elazegui, F.A., Teng, P.S., Savary, S., 1996. Effects of plant contacts, inoculation pattern, leaf wetness regime, and nitrogen supply on inoculum efficiency in rice sheath blight. Journal of Phytopathology 144:187-192.

Sharma, N.R., Teng, P.S., Olivares, F.M., 1990. Effect of rice growth stage on sheath blight (ShB) development and yield loss. International Rice Research Newsletter 15:19-20.

Hashiba, T. and Ijiri, T., 1989. Estimation of yield loss and computerized forecasting system (BLIGHTAS) for rice sheath blight disease. International Symposium on Tropical Agricultural Research: Crop losses due to disease outbreaks in the tropics and countermeasures. Tropical Agricultural Research Series (Japan) No. 22 pp. 163-171.

Tu, C.C., Chang, Y.C., Wang, C.W., 1979. Studies on the ecology of Rhizoctonia solani, the causal organism of rice sheath blight. Natl. Sci. Counc. Monthly, ROC 7:1208-1219.

Gross, M.K., Santini, J.B., Tikhonova, I. and Latin, R. 1998. The influence of temperature and leaf wetness duration on infection of perennial ryegrass by Rhizoctonia solani. Plant Disease 82:1012-1016.
}

\author{ Serge Savary, Robert J. Hijmans, Ireneo Pangga, Jorrel K. Aunario }


\seealso{ \code{\link[cropsim]{brownspot}}, \link[cropsim]{leafblast}  }

\examples{
wth <- getWthFile(system.file("weather/daily_weather_28368.nasa", package = "cropsim"))
res <- sheathBlight(wth, duration=120, onset=30)
plot(res)
}

