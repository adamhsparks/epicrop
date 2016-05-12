\name{Wallin}

\alias{Wallin}

\title{Wallin}

\description{
  Wallin late blight forecast model
}

\usage{
Wallin(wth, emergence='2000-05-15', duration=120) 
}

\arguments{
  \item{wth}{object of class 'weather'}
  \item{emergence}{expected date of plant emergence}
  \item{duration}{modeling duration}  
}

\details{
This model predicts the first occurrence of potato late blight and its subsequent spread based on season accumulation of severity values. These severity values are based on various combinations of the hours with a relative humidity of 90% or greater and the average temperature during those periods. Accumulation of severity values starts at plant emergence. 

For more details see: \url{http://www.ipm.ucdavis.edu/DISEASE/DATABASE/potatolateblight.html#wallin}
}

\value{
A data frame with the date, the severity and cumulative severity for that day, and a logical variable 'action' indicating whether the crop should be sprayed that day.
}


\references{ 
Wallin, J. R. 1951. Forecasting tomato and potato late blight in the northcentral region (Abstr) Phytopathology 41: 37.

Wallin, J. R. 1962. Summary of recent progress in predicting the late blight epidemics in United States and Canada. American Potato Journal 39: 306-312.

Wallin, J. R., and P. E. Waggoner. 1950. The influence of climate on the development and spread of Phytopthora infestans in artificially inoculated potato plots. Plant Disease Reporter Suppl. 190: 19-33.
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[cropsim]{Hyre}}, \code{\link[cropsim]{blitecast}} }


\examples{
wth <- getWthFile(system.file("weather/daily_weather_28368.nasa", package = "cropsim"))
res <- Wallin(wth)

}

