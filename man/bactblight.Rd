\name{bactBlight}

\alias{bactBlight}

\title{Rice bacterial blight disease }

\description{
  A dynamic mechanistic simulation of rice bacterial blight disease. The model is driven by daily weather data. 
}

\usage{
bactBlight(wth, emergence='2000-05-15', ...) 
 
}
\arguments{
  \item{wth}{An object of class 'weather', containing daily weather data}
  \item{emergence}{expected date of plant emergence}
  \item{...}{additional arguments. See \link[cropsim]{SEIR} }
}


\value{
 An object of class SEIR
}

\references{ 
Adhikari, T.B., 1991. Effects of rice genotype and environment on bacterial blight progression. PhD thesis, University of the Philippines at Los Banos, 143 p. 

Nayak, P., Suriya Rao, A.V., Chakrabarti, N.K., 1987. Components of resistance to bacterial blight disease of rice. Journal of Phytopathology 119:312-318.

Baw A. and Mew, T.W., 1988. Scoring systems for evaluating rice varietal resistance to bacterial blight (BB): lesion size by growth stage. International Rice Research Newsletter 13:10-11.

Luo Wei-Hong, 1996. Simulation  and measurement of leaf wetness formation in paddy rice crops. PhD, Wageningen Agricultural University, 87 p.

Horino, O., Mew, T.W., Yamada, T., 1982. The effect of temperature on the development of bacterial leaf blight on rice. Annals of the Phytopathological Society of Japan 48: 72-75

Medalla, E. 1992. Characterization of resistance of IR cultivars to tow races of Xanthomonas oryzae pv. oryzae. Unpublished M.S. Thesis, University of the Philippines at Los Banos, 81 p.
}


\author{Serge Savary, Robert J. Hijmans, Ireneo Pangga, Jorrel K. Aunario }


\examples{
wth <- getWthFile(system.file("weather/daily_weather_28368.nasa", package = "cropsim"))
res <- bactBlight(wth, onset=30, duration=120)
plot(res)
}

