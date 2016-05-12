\name{SEIR}
\alias{SEIR}


\title{Rice leaf blast disease}

\description{
  A dynamic mechanistic simulation of rice leaf blast. The model is driven by daily weather data. 
}

\usage{
SEIR(wth, emergence, onset=15, duration=120, rhlim=90, rainlim=5, wetness=0, initSites,
			initInfection=1, ageRc, tmpRc, rhRc, baseRc, latrans, inftrans, siteMax, AGGR, 
			RRPhysiolSenesc, RRG, SenescType=1)
}

\arguments{
  \item{wth}{daily weather data frame containing relative humidity (relh), minimum temeperature (tmin), and maximum temperature (tmax)}
  \item{emergence}{expected date of plant emergence}
  \item{onset}{expected number of days until the onset of disease after emergence date}
  \item{duration}{simulation duration}
  \item{rhlim}{threshold to decide whether leaves are wet or not}
  \item{rainlim}{threshold to decide whether leaves are wet or not}
  \item{wetness}{simulate RHmax or rain threshold (0) or leaf wetness duration (1)}
  \item{initSites}{}
  \item{initInfection}{}
  \item{ageRc}{}
  \item{tmpRc}{}
  \item{rhRc}{}
  \item{baseRc}{ corrected basic infection rate }
  \item{latrans}{ latent period }
  \item{inftrans}{ infectious period }
  \item{siteMax}{}
  \item{AGGR}{}
  \item{RRPhysiolSenesc}{}
  \item{RRG}{}
  \item{SenescType}{}  
}

\value{
  An object of class 'SEIR'
}

\references{ 
}

\details{
SEIR is called by the following specific disease models: 
\code{\link[cropsim]{leafBlast}},  \code{\link[cropsim]{bactBlight}}, \code{\link[cropsim]{brownSpot}}, \link[cropsim]{sheathBlight},  \code{\link[cropsim]{tungro}}  
}


\author{Serge Savary, Robert J. Hijmans, Ireno Pangga, Jorrel K. Aunario }


\keyword{ spatial }
