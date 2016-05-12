# Model development: Serge Savary & Rene Pangga. 
# R implementation by Robert J. Hijmans, Rene Pangga, & Jorrel Aunario  
# International Rice Research Institute
# r.hijmans@gmail.com 
# Date :  10 November 2009
# Version 0.3
# Licence GPL v3



brownSpot <- function(wth, emergence='2000-05-15', ...) {
	AgeCoefRc <- cbind(0:6 * 20, c(0.35, 0.35, 0.35, 0.47, 0.59, 0.71, 1.0))
	TempCoefRc <- cbind(15+(0:5) * 5, c(0, 0.06, 1.0, 0.85, 0.16, 0))
	RHCoefRc <- cbind(0:8 * 3, c(0, 0.12, 0.20, 0.38, 0.46, 0.60, 0.73, 0.87, 1.0))
	return(SEIR(wth=wth, emergence=emergence, ageRc=AgeCoefRc, tmpRc=TempCoefRc, 
        rhRc=RHCoefRc, baseRc=0.61, latrans=6, inftrans=19, initSites=600, AGGR=1, siteMax=100000, 
        RRPhysiolSenesc=0.01, RRG=0.1, ...))
}

leafBlast <- function(wth, emergence='2000-05-15', ...) {
	AgeCoefRc <- cbind(0:24 * 5, c(1, 1, 1, 0.9, 0.8, 0.7, 0.64, 0.59, 0.53, 0.43, 0.32, 0.22, 0.16, 0.09, 0.03, 0.02, 0.02, 0.02, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01))
	TempCoefRc <- cbind(2:9 * 5, c(0, 0.5, 1, 0.6, 0.2, 0.05, 0.01, 0))
	RHCoefRc <- cbind (4 + (0:10) * 2, c(0, 0.02, 0.09, 0.19, 0.29, 0.43, 0.54, 0.63, 0.77, 0.88, 1.0))	
	return(SEIR(wth=wth, emergence=emergence,  
		ageRc=AgeCoefRc, tmpRc=TempCoefRc, rhRc=RHCoefRc, baseRc=1.14, latrans=5, inftrans=20,
		initSites=600, AGGR=1, siteMax=30000, RRPhysiolSenesc=0.01, RRG=0.1, ...))
}

bactBlight <- function(wth, emergence='2000-05-15', ...) {
    AgeCoefRc <- cbind(0:12 * 10, c( 1, 1, 1, 0.9, 0.62, 0.43, 0.41, 0.42, 0.41, 0.41, 0.41, 0.41, 0.41))
 	TempCoefRc <- cbind(16 + (0:8 * 3), c(0, 0.29, 0.44, 0.90, 0.90, 1.0, 0.88, 0.01, 0))
	RHCoefRc <- cbind(c(2,1:8 * 3), c(0, 0.67, 0.81, 0.84, 0.87, 0.91, 0.94, 0.97, 1.0))
	return(SEIR(wth=wth, emergence=emergence,  
		ageRc=AgeCoefRc, tmpRc=TempCoefRc, rhRc=RHCoefRc, baseRc=0.87, latrans=5, inftrans=30, 
		siteMax=3200, AGGR=4, initSites=100, RRPhysiolSenesc=0.01, RRG=0.1, ...))
}	


sheathBlight <- function(wth, emergence='2000-05-15', ...) {
	AgeCoefRc <- cbind(0:12 * 10, c(0.84, 0.84, 0.84, 0.84, 0.84, 0.84, 0.83, 0.88, 0.88, 1.0, 1.0, 1.0, 1.0))
	RHCoefRc <- cbind(c(8,3:8 * 3), c(0, 0.24, 0.41, 0.68, 0.94, 0.97, 1.0))
	TempCoefRc <- cbind(3:10 * 4, c(0, 0.42, 0.94, 0.94, 1.0, 0.85, 0.64, 0))
	return(SEIR(wth=wth, emergence=emergence,  
		ageRc=AgeCoefRc, tmpRc=TempCoefRc, rhRc=RHCoefRc, baseRc=0.46, latrans=3, inftrans=120, 
		siteMax=800, AGGR=2.8, initSites=25, RRPhysiolSenesc=0.005, RRG=0.2, ...))
}


tungro <- function(wth, emergence='2000-05-15', ...) {
    AgeCoefRc <- cbind (0:8 * 15, c(1.0, 1.0, 0.98, 0.73, 0.51, 0.34, 0, 0, 0))
	TempCoefRc <- cbind (c(9,10 + (0:9 * 3.1111),40), c(0,0.13, 0.65, 0.75, 0.83, 0.89, 0.93, 0.97, 1.0, 0.96, 0.93,0))
	RHCoefRc <- 1
	return(SEIR(wth=wth, emergence=emergence,  
		ageRc=AgeCoefRc, tmpRc=TempCoefRc, rhRc=RHCoefRc, baseRc=0.18, latrans=6, inftrans=120, 
		siteMax=100, AGGR=1, initSites=100, RRPhysiolSenesc=0.01, RRG=0.1, ...))
}	



SEIR <- function(wth, emergence, onset=15, duration=120, rhlim=90, rainlim=5, wetness=0, initSites, 
					initInfection=1, ageRc, tmpRc, rhRc, baseRc, latrans, inftrans, siteMax, AGGR, 
					RRPhysiolSenesc, RRG, SenescType=1)	
{

	emergence <- as.Date(emergence)
	wth@w <- subset(wth@w, wth@w$date >= emergence-1)
	if (dim(wth@w)[1] < duration) {	stop("Incomplete weather data") }
	wth@w <- wth@w[1:(duration+1),]
	
	if (wetness == 1) {
		W <- leafWet(wth, simple=TRUE)
	}

	# outputvars
	COFR <- Rc <- RHCoef <- latency <- infectious <- Severity <- RSenesced <- RGrowth <- Rtransfer <- Rinfection <- Diseased <- Senesced <- Removed <- now_infectious <- now_latent <- Sites <- TotalSites <- rep(0, times=duration+1)
	
	for (day in 0:duration) {
    # State calculations
		if (day==0) {
		# start crop growth 
			Sites[day+1] <- initSites
			RSenesced[day+1] <- RRPhysiolSenesc * Sites[day+1]
		} else {
			if (day > inftrans) {
				removedToday <- infectious[infday+2]
			} else {
				removedToday <- 0
			}

			Sites[day+1] <- Sites[day] + RGrowth[day] - Rinfection[day] - RSenesced[day]
			RSenesced[day+1] <- removedToday * SenescType + RRPhysiolSenesc * Sites[day+1]
			Senesced[day+1] <- Senesced[day] + RSenesced[day]

			latency[day+1] <- Rinfection[day]
			latday <- day - latrans+1
			latday <- max(0, latday)
			now_latent[day+1] <- sum(latency[latday:day+1])

			infectious[day+1] <- Rtransfer[day]		
			infday <- day - inftrans+1
			infday <- max(0, infday)
			now_infectious[day+1] <- sum(infectious[infday:day+1])
		}

		if (Sites[day+1] < 0 ) { 
			Sites[day+1] <- 0
			break 
		}
		
		if (wetness==0){
			if (wth@w$rhmax[day+1] >= rhlim | wth@w$prec[day+1] >= rainlim) {
				RHCoef[day+1] <- 1
			}
		} else {
			RHCoef[day+1]<- AFGen (rhRc, W[day+1])
		}		

		Rc[day+1] <- baseRc * AFGen(ageRc, day) * AFGen(tmpRc, wth@w$tavg[day+1]) * RHCoef[day+1]
		Diseased[day+1] <- sum(infectious) + now_latent[day+1] + Removed[day+1]
		Removed[day+1] <- sum(infectious) - now_infectious[day+1]

		COFR[day+1] <- 1-(Diseased[day+1]/(Sites[day+1]+Diseased[day+1]))

		if (day==onset) {
		# initialization of the disease
			Rinfection[day+1] <- initInfection
		} else if (day > onset) {
			Rinfection[day+1] <- now_infectious[day+1] * Rc[day+1] * (COFR[day+1]^AGGR)
		} else {
			Rinfection[day+1] <- 0
		}
		
		if (day >= latrans) {	
			Rtransfer[day+1] <- latency[latday+1]
		} else {
			Rtransfer[day+1] <- 0
		}

		TotalSites[day+1] <- Diseased[day+1] + Sites[day+1]
		RGrowth[day+1] <- RRG * Sites[day+1] * (1-(TotalSites[day+1]/siteMax))
		Severity[day+1] <- (Diseased[day+1]-Removed[day+1])/(TotalSites[day+1] - Removed[day+1])*100
	}

	res <- cbind(0:duration, Sites, now_latent, now_infectious, Removed, Senesced, Rinfection, Rtransfer, RGrowth, RSenesced, Diseased, Severity)
	res <- as.data.frame(res[1:(day+1),])
	
	dates <- seq(emergence-1, emergence+duration, 1)
	res <- cbind(dates[1:(day+1)], res)
	colnames(res) <- c("date", "simday", "sites", "latent", "infectious", "removed", "senesced", "rateinf", "rtransfer", "rgrowth", "rsenesced", "diseased", "severity")
	
	result <- new('SEIR')
	result@d <- res
	return(result)
}


#res1 <- leafBlast(wth, onset=15, duration=120)
#res2 <- brownSpot(wth, onset=15, duration=120)
#res3 <- bactBlight(wth, onset=15, duration=120)
#res4 <- sheatBlight(wth, onset=15, duration=120)
#res5 <- tungro(wth, onset=15, duration=120)
