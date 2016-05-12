\name{simriw}

\alias{simriw}
\alias{simriwCultivar}


\title{ Rice simulation model}

\description{
  Get weather data from a NASA website
}

\usage{
simriw(wth, cultivar, startday, transplant=FALSE, CO2=350)
simriwCultivar(name)
}

\arguments{
  \item{wth}{ object of class 'weather' }  
  \item{cultivar}{ object of class 'SIMRIWcultivar' }    
  \item{startday}{ planting or transplanting date }    
  \item{transplant}{Logical }  
  \item{CO2}{ CO2 concentration }    
  \item{name}{ cultivar name (or missing to see a list of available cultivars) }    
}

\references{
Horie, T., Nakagawa, H.N., Centeno, H.G.S., Kropff, M.J. (1995). The rice simulation model SIMRIW and its testing. In: R.B. Matthews, M.J. Kropff, D. Bachelet & H.H. van Laar (Eds), Modeling the impact of climate change on rice production in Asia. CABI, UK, IRRI, Philippines, pp. 95-139.
}

\value{
simriw: An object of class 'SIMRIW'

simriwCultivar: An object of class 'SIMRIWcultivar'
}

\author{ Robert J. Hijmans }

\examples{
wth <- getWthFile(system.file("weather/daily_weather_28368.nasa", package = "cropsim"))
cv <- simriwCultivar('Nipponbare')
sim <- simriw(wth, cv, startday='2000-05-15')

sim
plot(sim)
}
