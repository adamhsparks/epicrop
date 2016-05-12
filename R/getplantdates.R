
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# License GPL3
# Version 0.1  January 2009


getplantdates <- function(yields, fact=2, yp=10000, yt=0.5) {
	yields[is.na(yields)] <- 0
	if (sum(yields) == 0) {
		return(yields)
	} else {
		maxyld <- max(yields)
		if ( maxyld > yp ) { 
			acceptyield <- yt * yp
		} else { 
			acceptyield <- yt * maxyld
		}
		yields[yields < acceptyield ] <- 0
		relyields <- (yields / maxyld)^fact
		relyields[relyields<0.01] <- 0
		sumyld <- sum(relyields)
		plantdates <- relyields / sumyld
		return(plantdates)
	}
}

