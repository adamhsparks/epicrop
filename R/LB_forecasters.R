# cropsim

# Potato late blight forecast models
# Model name: Hyre
# Hyre, R. A. 1954. Progress in forecasting late blight of potato and tomato. Plant Disease Reporter. 38: 245-253.

# R implementation by Robert Hijmans, r.hijmans@gmail.com
# October 2009

# Days are considered blight favorable when the 5-day average temperature 
# is below 25.5 C and the total rainfall for the last 10 days is 3 cm or greater. 
# Days with minimum temperatures below 7.2 C are considered unfavorable.

# Source: http://www.ipm.ucdavis.edu/DISEASE/DATABASE/potatolateblight.html#hyre	

.subsetwth <- function(wth, emergence='2000-05-15', duration=120) {
	emergence <- as.Date(emergence)
	wth@w <- subset(wth@w, wth@w$date >= emergence)
	if (dim(wth@w)[1] < duration) {
		stop("Incomplete weather data")
	}
	wth@w <- wth@w[1:duration,]
	wth
}


.cumulate <- function(x) {
	xx <- vector(length=length(x))
	xx[1] <- x[1]
	for (i in 2:length(x)) {
		xx[i] <- xx[i-1] + x[i]
	}
	xx
}


Hyre <- function(wth, emergence='2000-05-15', duration=120) {
	w <- .subsetwth(wth, emergence, duration)@w
	t5 <- raster::movingFun(w$tavg, n=5, type='to', na.rm=TRUE) / 5
	p10 <- raster::movingFun(w$prec, n=10, type='to', na.rm=TRUE)
	blightFavorable <- t5 < 25.5 & w$tavg > 7.2 & p10 > 30
	cbind(as.data.frame(w$date), blightFavorable)
}


Wallin <- function(wth, emergence='2000-05-15', duration=120) {

	wth <- .subsetwth(wth, emergence, duration)
	leafwet <- leafWet(wth)
	w <- wth@w
	t1 <- w$tavg > 7.2 & w$tavg <= 11.6
	t2 <- w$tavg > 11.6 & w$tavg <= 15.0
	t3 <- w$tavg > 15.0 & w$tavg <= 26.6
	lw16 <- 
	severity <- vector(length=duration)
	severity[ leafwet < 16 & t1 ]  <- 0
	severity[ leafwet < 13 & t2 ]  <- 0
	severity[ leafwet < 10 & t3 ]  <- 0
	severity[ leafwet < 19 & t1 ]  <- 1
	severity[ leafwet < 16 & t2 ]  <- 1
	severity[ leafwet < 13 & t3 ]  <- 1
	severity[ leafwet < 22 & t1 ]  <- 2
	severity[ leafwet < 19 & t2 ]  <- 2
	severity[ leafwet < 16 & t3 ]  <- 2
	severity[ leafwet <= 24 & t1 ] <- 3
	severity[ leafwet < 22 & t2 ]  <- 3
	severity[ leafwet < 19 & t3 ]  <- 3
	severity[ leafwet >= 22 & t2 ] <- 4
	severity[ leafwet >= 19 & t3 ] <- 4
	cumSeverity <- .cumulate(severity)
	x <- which(cumSeverity > 19) + 10
	x <- subset(x, x <= duration)
	action <- vector(length=length(severity))
	action[] <- FALSE
	action[x] <- TRUE
	cbind(as.data.frame(w$date), severity, cumSeverity, action )
}


blitecast <- function(wth, emergence='2000-05-15', duration=120) {
	wth <- .subsetwth(wth, emergence, duration)
	hyre <- Hyre(wth, emergence, duration)
	wallin <- Wallin(wth, emergence, duration)	
	first <- hyre$blightFavorable | wallin$action
	spray <- first
	spray[] <- FALSE
	if (sum(first) > 0) {
		first <- min(which(first))
	} else {
		return( cbind(as.data.frame(wth@w$date), spray) )
	}
	
	rainfav7 <- raster::movingFun(wth@w$prec > 3, n=7, type='to', na.rm=TRUE)
	severity7 <- raster::movingFun(wallin$severity, n=7, type='to', na.rm=TRUE)

	second <- first
	second[] <- -1

	second[ rainfav7 < 5 & severity7 == 4 ] <- 0
	second[ rainfav7 < 5 & (severity7 == 5 | severity7 == 6) ] <- 1
	second[ rainfav7 < 5 & severity7 > 5 ] <- 2
	second[ rainfav7 > 4 & severity7 == 3 ] <- 0
	second[ rainfav7 > 4 & severity7 == 4 ] <- 1
	second[ rainfav7 > 4 & severity7 > 4 ] <- 2

	spray[first] <- TRUE
	sprayed <- 0
	for (i in first:length(second)) {
		if ((second[i] == 1 & sprayed == 7) |  (second[i] == 2 & sprayed == 5)) 	{
			spray[i] <- TRUE
			sprayed <- 0
		}
		sprayed <- sprayed + 1
	}
	return( cbind(as.data.frame(wth@w$date), spray) )
}	

