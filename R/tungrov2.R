# Author: Serge Savary & Rene Pangga. 
# R translation: Robert J. Hijmans , Rene Pangga & Jorrel Aunario, r.hijmans@gmail.com (translated from STELLA TungroMod v6PRI)
# International Rice Research Institute
# Date :  23 August 2010
# Version 0.2
# Licence GPL v3
#switch :  wetness (1) uses RHCoefRc=1 vs wetness (0)  uses RH + rain threshold corresponding to  STELLA  TungroModv5T 

# onset=25 deleted as component of tungro function, as onset is not a constant
#component on nb of rice crop per year should be added in the function
tungro2 <- function(wth, emergence='2000-05-15', duration=120, rhlim=0, rainlim=0, wetness=1, PCISrice=1) {
	emergence <- as.Date(emergence)
	
    wth@w <- subset(wth@w, wth@w$date >= emergence-1)
	if (dim(wth@w)[1] < duration) {	stop("Incomplete weather data") }
	wth@w <- wth@w[1:(duration+1),]
	
    # previous crop
    # here should be a link to the GIS database with nb of rice crops per year
    # PCISrice <- 1
    # wetness
	# W <- leafwet(wth)

	RRG <- 0.1
	#RRPhysiolSenesc <- 0
	SenescType <- 1	
	AGGR <- 1
	baseRc <- 0.18
	SiteMax <- 100
	initInfection <- 1
	initSites <- 100
	infectious_transit_time <- 120
	latency_transit_time <- 6
	extinctPRI <- -0.025
	InitPrevRainfallIndex <- 100
	EARLY <- 25
	LATE <- 40
	
    if (PCISrice==1) {
		PC <- 1
	} else {
		PC <- 150
	}
	
	# Output vars
	RainOut <- RainIn <- PrevRainfallIndex <- COFR <- Rc <- RHCoef <- latency <- infectious <- Incidence <- RSenesced <- RGrowth <- Rtransfer <- Rinfection <- Diseased <- Senesced <- Removed <- now_infectious <- now_latent <- Sites <- TotalSites <- rep(0, times=duration+1)

	#parameters
    ageRc <- cbind (0:8 * 15, c(1.0, 1.0, 0.98, 0.73, 0.51, 0.34, 0, 0, 0))
	tmpRc <- cbind (c(9,10 + (0:9 * 3.1111),40), c(0,0.13, 0.65, 0.75, 0.83, 0.89, 0.93, 0.97, 1.0, 0.96, 0.93,0))
	RHCoefRc <- 1
	
	#MatPer <- 20
	
	for (day in 0:duration) {
			
	# State calculations
		if (day==0) {
		# start crop growth and Previous Rainfall Index
			Sites[day+1] <- 100
			#RSenesced[day] <- RRPhysiolSenesc * Sites[day]
			#Senesced[day] <- RRPhysiolSenesc * initSites
			PrevRainfallIndex[day+1] <- InitPrevRainfallIndex
		} else {
			if (day > infectious_transit_time) {
				removedToday <- infectious[infday+1]
			} else {
				removedToday <- 0
			}
			
			Sites[day+1] <- Sites[day] + RGrowth[day] - Rinfection[day] - RSenesced[day]
			#Sites[day] <- Sites[day-1] + RGrowth[day-1] - Rinfection[day-1]
			#RSenesced[day] <- removedToday * SenescType + RRPhysiolSenesc * Sites[day]
			#Senesced[day] <- Senesced[day-1] + RSenesced[day-1]
			
			latency[day+1] <- Rinfection[day]
			latday <- day - latency_transit_time + 1
			latday <- max(0, latday)
			now_latent[day+1] <- sum(latency[latday:day+1])

			infectious[day+1] <- Rtransfer[day]		
			infday <- day - infectious_transit_time + 1
			infday <- max(0, infday)
			now_infectious[day+1] <- sum(infectious[infday:day+1])

			PrevRainfallIndex[day+1] <- PrevRainfallIndex[day] + RainIn[day] - RainOut[day]
		
		}
		
		if (Sites[day+1] < 0 ) { 
			Sites[day+1] <- 0
			break 
		}
		
        if (day<21) {
			RainIn[day+1] <- wth@w$prec[day+1]*exp(extinctPRI)
			RainOut[day+1] <- PrevRainfallIndex[day+1]*(1-exp(extinctPRI))
		} else {
			RainIn[day+1] <- 0
			RainOut[day+1] <- 0
		}
			
        if (PrevRainfallIndex[day+1]>100) {
			EpidOnset_Endemic <- EARLY
		} else {
			EpidOnset_Endemic <-LATE
		}
    	onset <- PC*EpidOnset_Endemic 
			
		if (wetness==0){
			if (rhx[day+1] >= rhlim | rain[day+1] >= rainlim) {
				RHCoef[day] <- 1
			}
		} else {
		  RHCoef[day+1]<- RHCoefRc
		}		
		
		Rc[day+1] <- baseRc * AFGen(ageRc, day) * AFGen(tmpRc, wth@w$tavg[day+1]) * RHCoef[day+1]

		Diseased[day+1] <- sum(infectious) + now_latent[day+1] + Removed[day+1]
		Removed[day+1] <- sum(infectious) - now_infectious[day+1]
		
		COFR[day+1] <- 1-(Diseased[day+1]/(Sites[day+1]+Diseased[day+1]))
		
		if (day==onset) {
			Rinfection[day+1] <- initInfection
		} else if (day > onset) {
			Rinfection[day+1] <- now_infectious[day+1] * Rc[day+1] * (COFR[day+1]^AGGR)
		} else {
			Rinfection[day+1] <- 0
		}
		
		# Boxcar transger to other staet 
		if (day >= latency_transit_time ) {	
			Rtransfer[day+1] <- latency[latday+1]
		} else { 
			Rtransfer[day+1] <- 0	
		}
		
	
		TotalSites[day+1] <- Diseased[day+1] + Sites[day+1]
		RGrowth[day+1] <- RRG * Sites[day+1] * (1-(TotalSites[day+1]/SiteMax))
		#Severity[day+1] <- (Diseased[day+1]-Removed[day+1])/(TotalSites[day+1] - Removed[day+1])*100



# consider natural senescence...		
#		MatScen <- -1*(day*Sitemax/MatPer) + (Sitemax * duration/MatPer)
		Incidence[day+1] <- (Diseased[day+1]-Removed[day+1])/(TotalSites[day+1] - Removed[day+1])*100		

	}
	
	res <- cbind(0:duration, Sites, now_latent, now_infectious, Removed, Senesced, Rinfection, Rtransfer, RGrowth, RSenesced, Diseased, Incidence, PrevRainfallIndex)
	res <- as.data.frame(res[1:(day+1),])
	
	dates <- seq(emergence-1, emergence+duration, 1)
	res <- cbind(dates[1:(day+1)], res)
    #res <- Diseased / AllSites 
	colnames(res) <- c("date", "simday", "sites", "latent", "infectious", "removed", "senesced", "rateinf", "rtransfer", "rgrowth", "rsenesced", "diseased", "incidence", "PrevRainfallIndex")

	result <- new('SEIR')
	result@d <- res
	return(result)

}
