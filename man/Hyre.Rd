\name{Hyre}

\alias{Hyre}

\title{Hyre}

\description{
  Hyre late blight forecast model
}

\usage{
Hyre(wth, emergence='2000-05-15', duration=120) 
}

\arguments{
  \item{wth}{object of class 'weather'}
  \item{emergence}{expected date of plant emergence}
  \item{duration}{modeling duration}  
}

\details{
This model is based on daily rainfall and maximum and minimum temperatures. It forecasts the outbreak of potato late blight 7-14 days after the occurrence of ten consecutive blight favorable days. Days are considered blight favorable when the 5-day average temperature is below 25.5 C and the total rainfall for the last 10 days is 30 mm or greater. Days with minimum temperatures below 7.2 C are considered unfavorable.

\url{http://www.ipm.ucdavis.edu/DISEASE/DATABASE/potatolateblight.html#Wallin}

}


\value{
A data frame with the date and a logical value indicating whether the day is blight favorable or not
}


\references{ 
Hyre, R. A. 1954. Progress in forecasting late blight of potato and tomato. Plant Disease Reporter. 38: 245-253.
}

\author{Robert J. Hijmans}


\seealso{ \code{\link[cropsim]{Wallin}}, \code{\link[cropsim]{blitecast}} }

\examples{
wth <- getWthFile(system.file("weather/daily_weather_28368.nasa", package = "cropsim"))
res <- Hyre(wth)

}

