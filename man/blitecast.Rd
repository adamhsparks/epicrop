\name{blitecast}

\alias{blitecast}

\title{Blitecast}

\description{
  blitecast late blight forecast model
}

\usage{
blitecast(wth, emergence='2000-05-15', duration=120) 
}

\arguments{
  \item{wth}{object of class 'weather'}
  \item{emergence}{expected date of plant emergence}
  \item{duration}{modeling duration}  
}

\details{
BLITECAST is an integrated computerized version of both the Hyre and the Wallin model. The first part of the program forecasts the initial occurrence of late blight 7-14 days after the first accumulation of 10 rain-favorable days according to Hyre's criteria, or the accumulation of 18 severity values according to Wallin's model. The second part of the program recommends fungicide sprays based on the number of rain-favorable days and severity values accumulated during the previous seven days. Accumulation of rain-favorable days and severity values begins when distinct green rows can be seen in the potato field, and ends at vine kill. The first spray is recommended when the first late blight forecast is given. Subsequent sprays are recommended according to an adjustable matrix which correlates rain-favorable days with severity values.
\url{http://www.ipm.ucdavis.edu/DISEASE/DATABASE/potatolateblight.html#blitecast}

}


\value{
A data frame with the date and a logical value indicating whether the crop should be sprayed that day.
}


\references{ 
Krause, R. A., Massie, L. B., and Hyre, R. A. 1975. BLITECAST, a computerized forecast of potato late blight. Plant Disease Reporter 59: 95-98.

MacKenzie, D. R. 1981. Scheduling fungicide applications for potato late blight. Plant Disease 65: 394-399.

MacKenzie, D. R. 1984. Blitecast in retrospect. A look at what we learned. FAO Plant Protection Bulletin 32:45-49.
}

\author{Robert J. Hijmans}


\seealso{ \code{\link[cropsim]{Wallin}}, \code{\link[cropsim]{Hyre}} }

\examples{
wth <- getWthFile(system.file("weather/daily_weather_28368.nasa", package = "cropsim"))
#res <- blitecast(wth)

}

