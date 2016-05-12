#     SIMRIW.FOR
#     SImulation Model for RIce-Weather Relationships
#     by Takeshi Horie, Lab. Crop Science, Kyoto Univ., Kyoto,Japan 

# implemented in R by Robert Hijmans, r.hijmans@gmail.com
# September 2009
# Version 0.1

# CROPARAM
# This file gives values of crop parameters used in SIMRIW
# Parameters in the line 1 to 6 (GO to DVSAS) are related to crop phenology up to heading,
# and VERY CULTIVAR - SPECif IC!
# Those in the line 7 to 8 (GR to KCR) are for crop phenology during ripening phase.
# Those in the line 10 to 11 (EXTC and COVES) are  related to dry matter production.
# HIMX (line 12) is the maximum harvest index.
# CTR (line 13) is the critical temp. for autumn coolness.
# Those in the line 14 to 19 (A to BETA) are related to leaf  area growth.
# Those in the line 20 to 23 are related to cooling-degree-days for spikelet sterility.

	

setClass('SIMRIWcultivar',
	representation (
		name  = 'character',
		GV    = 'numeric', #unit=days; related to crop phenology up to heading
		ALF   = 'numeric', #no unit; related to crop phenology up to heading
		TH    = 'numeric', #unit=C; related to crop phenology up to heading
		BDL   = 'numeric', #no unit; related to crop phenology up to heading
		LC    = 'numeric', #unit=hours; related to crop phenology up to heading
		DVIA  = 'numeric', #no unit; related to crop phenology up to heading
		TCR   = 'numeric', #unit=C
		GR    = 'numeric', #unit=days; for crop phenology during ripening phase
		KCR   = 'numeric', #no unit; for crop phenology during ripening phase
		EXTC  = 'numeric', #no unit; related to dry matter production
		COVES = 'numeric', #unit=G/MJ; related to dry matter production
		HIMX  = 'numeric', #no unit; maximum harvest index
		CTR   = 'numeric', #unit=C; critical temp. for autumn coolness.
		A     = 'numeric', #unit=/day; related to leaf  area growth
		KF    = 'numeric', #no unit; related to leaf  area growth
		ETA   = 'numeric', #no unit; related to leaf  area growth
		FAS   = 'numeric', #no unit; related to leaf  area growth
		TCF   = 'numeric', #unit=C; related to leaf  area growth
		BETA  = 'numeric', #no unit; related to leaf  area growth
		THOT  = 'numeric', #unit=C; related to cooling-degree-days for spikelet sterility
		STO   = 'numeric', #unit=%; related to cooling-degree-days for spikelet sterility
		BST   = 'numeric', #no unit; related to cooling-degree-days for spikelet sterility
		PST   = 'numeric'  #no unit; related to cooling-degree-days for spikelet sterility
 )
)


setMethod ('show' , 'SIMRIWcultivar', 
	function(object) {
	   str(object)
	}
)	


setClass('SIMRIW',
	representation (
		cultivar = 'SIMRIWcultivar',
		PYBROD = 'numeric', 
		PYBRON = 'numeric',
		PYPADY = 'numeric',
		PANDW  = 'numeric',
		DWT    = 'numeric',
		d      = 'data.frame'
	),
	prototype (	
		PYBROD = 0, 
		PYBRON = 0, 
		PYPADY = 0, 
		PANDW  = 0, 
		DWT    = 0, 
		d = data.frame()
	),	
	validity = function(object)
	{
		return(TRUE)
	}
)
	


setMethod ('show' , 'SIMRIW', 
	function(object) {
		cat('class   :' , class(object), '\n')
		cat('\n')	
		cat('Cultivar:' , object@cultivar@name, '\n')
		cat('PYBROD  :' , object@PYBROD, '\n')
		cat('PYBRON  :' , object@PYBRON, '\n')
		cat('PYPADY  :' , object@PYPADY, '\n')
		cat('PANDW   :' , object@PANDW, '\n')
		cat('DWT     :' , object@DWT, '\n')
		
		cat('\n')
		l <- dim(object@d)[1]
		if (l == 0) {
			cat('no daily data\n')
		} else {
			cat(l ,'rows: \n')
			if (l < 25) {
				print(object@d)
			} else {
				print(object@d[1:5,])
				cat('\n')
				cat('  (... ...  ...)\n')
				cat('\n')
				print(object@d[(l-5):l,])
			}
		}
	}
)	



setMethod ('plot', signature(x='SIMRIW', y='missing'),
	function(x, ...) {
		plot(x@d$date, x@d$DW,  ylab="Yield (g/m2)", ...)
		points(x@d$date, x@d$GY, col='blue')
		points(x@d$date, x@d$PY, col='red')
		legend(x@d$date[1], x@d$DW[dim(x@d)[1]], c("DW", "GY", "PY"), col=c("black", "blue", "red"), pch=21)
	}
)








.showSIMRIWcultivars <- function() {
	tab <-  read.table(system.file("simriw/cultivars.txt", package="cropsim"), sep=',', header=T)
	return(paste(colnames(tab)[-2:-1], collapse=', '))
}

simriwCultivar <- function(name) {
	if (missing(name)) {
		.showSIMRIWcultivars()
	}
	tab <-  read.table(system.file("simriw/cultivars.txt", package="cropsim"), sep=',', header=T, row.names=1)
	if (! (name %in% colnames(tab)[-2:-1]) ) {
		stop('Unknown cultivar. Choose from: ', .showSIMRIWcultivars())
	} 
	cv <- new('SIMRIWcultivar')
	cv@name <- colnames(tab[name])
	cv@GV <- tab['GV', name]
	cv@ALF <- tab['ALF', name]
	cv@TH <- tab['TH', name]
	cv@BDL <- tab['BDL', name]
	cv@LC <- tab['LC', name]
	cv@DVIA <- tab['DVIA', name]
	cv@TCR <- tab['TCR', name]
	cv@GR <- tab['GR', name]
	cv@KCR <- tab['KCR', name]
	cv@EXTC <- tab['EXTC', name]
	cv@COVES <- tab['COVES', name]
	cv@HIMX <- tab['HIMX', name]
	cv@CTR <- tab['CTR', name]
	cv@A <- tab['A', name]
	cv@KF <- tab['KF', name]
	cv@ETA <- tab['ETA', name]
	cv@FAS <- tab['FAS', name]
	cv@TCF <- tab['TCF', name]
	cv@BETA <- tab['BETA', name]
	cv@THOT <- tab['THOT', name]
	cv@STO <- tab['STO', name]
	cv@BST <- tab['BST', name]
	cv@PST <- tab['PST', name]
	return(cv)	
}


simriw <- function(wth, cultivar, startday, transplant=FALSE, CO2=350) {
#     Constants and parameters which may not be cutivar specific
#     Constants related to optcal properties of canopy
    SCAT = 0.25
	RSOL  = 0.1
	RCAN  = 0.22
	KREF = 0.5
#     Parameters related changes with DVI in radiation conversion efficiency
    CL = 0.001
	TAUC = 0.1
#     Conversion factors from rough to brown rice, and panicle to rough rice
    CVBP = 0.76
	CVPP = 0.9
 
#     Initial conditions for simulation

	if (transplant) {
		DVII = 0.15  #transplant	
		LAII = 0.05
		DWI = 15
	} else {
		DVII = 0 # emergence
		LAII = 0.0025
		DWI = 4
	}

    IFLUG1=0
    IFLUG2=0
    CDED=0.
    TCHECK=0.
    STERP=0.
    HTSUM=0.
    HTDAY=0.
    STHT=0.0
    ATHHT=cultivar@HIMX
    ATHLT=cultivar@HIMX
    JJ=1
    DWGRAIN=0.0
    DVI=DVII
    LAI=LAII
    DW=DWI
    DWGRAIN=0.0
    DWPAN=0.0
	STHT=0
	STLT=0

# weather data	
    AVT <- wth@w$tavg
	RAD <- wth@w$srad
	TMX <- wth@w$tmax
    startday <- as.Date(startday)
	endday <- startday + 200
	days <- seq(startday, endday, 1)
	DL <- daylength(wth@lat, wth@w$doy)
	
	startindex <- which(wth@w[,'date'] == startday)
	#endindex <- which(wth@w[,'date'] == endday)
	
	day <- startindex-1
	growing <- TRUE
	simday <- 0

	res <- as.data.frame(matrix(ncol=9, nrow=length(days)))
	colnames(res) <- c('date','TMP', 'RAD','DL','DVI','LAI', 'DW', 'GY', 'PY')
	class(res[,'date']) <- 'Date'
	
	#     Dynamic Section of The Model  ************************************************************
	while (growing) {
		day <- day + 1
		simday <- simday + 1
		if (day >= nrow(wth@w)) {
			warning('reached end of weather records')
			growing = FALSE
		}
		
		res[simday,'date'] <- wth@w$date[day]
		res[simday,'TMP'] <- AVT[day]
		res[simday,'RAD'] <- RAD[day]
		res[simday,'DL'] <- DL[day]
		res[simday,'DVI'] <- DVI
		res[simday,'LAI'] <- LAI
		res[simday,'DW'] <- DW
		res[simday,'GY'] <- DWGRAIN
		res[simday,'PY'] <- DWPAN
			
#     Culculation of Developmental Index DVI
		if (DVI < cultivar@DVIA) {
			EFT <- AVT[day]-cultivar@TH
			DVR <- 1.0/(cultivar@GV*(1.0+exp(-cultivar@ALF*EFT)))
		} else if (DVI <= 1.0) {
			EFT <- AVT[day]-cultivar@TH
			EFL <- min(DL[day]-cultivar@LC,0.)
			DVR <- (1.0-exp(cultivar@BDL*EFL))/(cultivar@GV*(1.0+exp(-cultivar@ALF*EFT)))
		} else {
			EFT <- max(AVT[day]-cultivar@TCR,0.)
			DVR <- (1.0-exp(-cultivar@KCR*EFT))/cultivar@GR
		}
		DVI <- DVI+DVR
#
#    Culculation of LAI
#
		if (DVI < 0.95) {
			EFFTL <- max(AVT[day]-cultivar@TCF,0.)
			GRLAI <- LAI*cultivar@A*(1.0-exp(-cultivar@KF*EFFTL))*(1.0-(LAI/cultivar@FAS)**cultivar@ETA)
			GRL95 <- GRLAI
			DVI95 <- DVI
		} else if (GRLAI > 0.0  |  DVI <= 1.0) {
			GRLAI <- GRL95*(1.0-(DVI-DVI95)/(1-DVI95))
			LAIMX <- LAI
			DVI1 <- DVI
		} else if (DVI < 1.1) {
			GRLAI <- -(LAIMX*(1.0-cultivar@BETA)*(DVI-DVI1)/(1.1-DVI1))*DVR
		} else {
			GRLAI <- -LAIMX*(1.0-cultivar@BETA)*DVR
		} 
		LAI <- LAI+GRLAI
#
#    Culuculation of Crop Dry Weight
#
		TAU <- exp(-(1.0-SCAT)*cultivar@EXTC*LAI)
		REF <- RCAN-(RCAN-RSOL)*exp(-KREF*LAI)
		ABSOP <- 1.0-REF-(1.0-RSOL)*TAU
		ABSRAD <- RAD[day]*ABSOP
		COVCO2 <- cultivar@COVES*(1.54*(CO2-330.0)/(1787.0+(CO2-  330.0))+1.0)
		if (DVI < 1.0) {
			CONEF <- COVCO2
		} else {
			CONEF <- COVCO2*(1.0+CL)/(1.0+CL*exp((DVI-1.0)/TAUC))
		}
		DW <- DW+CONEF*ABSRAD
#
#    Culuculation of Spikelet Sterility Percentage due to Cool Temerature
#
		if (DVI > 0.75  &  DVI < 1.2) {
			CDEG <- max(cultivar@THOT-AVT[day],0.)
			CDED <- CDED+CDEG
			SSTR <- cultivar@STO+cultivar@BST*CDED**cultivar@PST
			STLT <- min(100.0,SSTR)
			RIPEP <- 1.0-STLT/100.0
			ATHLT <- cultivar@HIMX*RIPEP
		}
#
#    Culculation of Spikelet Sterility Percentage due to Heat Stress
#
		if (DVI > 0.96  &  DVI < 1.20) {
			HTSUM <- HTSUM+TMX[day]
			HTDAY <- HTDAY+1
		} 
		if (DVI >= 1.20  &  IFLUG1 == 0) {
			AVTMX <- HTSUM/HTDAY
			STHT <- 100.0/(1.0+exp(-0.853*(AVTMX-36.6)))
			ATHHT <- (1.0-STHT/100.0)*cultivar@HIMX
			IFLUG1 <- 1
		} 
#
#    Culculation of Grain Yield
#
		ATHI <- min(ATHLT,ATHHT)
		STERP <- max(STHT,STLT)
		EFDVI <- max(DVI-1.22, 0.0)
		HI <- ATHI*(1.0-exp(-5.57*EFDVI))
		DWGRAIN <- DW*HI
		DWPAN <- DWGRAIN/CVBP/CVPP
#
#    Time Control and Terminal Condition of Simulation
#
		if (DVI > 1.0  &  AVT[day] < cultivar@CTR) {
			TCHECK <- TCHECK+1
		} 
		if (DVI > 2.0) {
			growing <- FALSE
		}
	}

	simday <- simday + 1
	res[simday,'date'] <- wth@w$date[day]
	res[simday,'TMP'] <- AVT[day]
	res[simday,'RAD'] <- RAD[day]
	res[simday,'DL'] <- DL[day]
	res[simday,'DVI'] <- DVI
	res[simday,'LAI'] <- LAI
	res[simday,'DW'] <- DW
	res[simday,'GY'] <- DWGRAIN
	res[simday,'PY'] <- DWPAN
	
#    Terminal Section of  Simulation
    PYBROD <- DWGRAIN/100.0
    PYBRON <- PYBROD/0.86
    PYPADY <- PYBRON/CVBP
    PANDW <- PYBROD/CVBP/CVPP
    DWT <- DW/100.0
	
	r <- new('SIMRIW')
	r@cultivar <- cultivar
	r@PYBROD <- PYBROD
	r@PYBRON <- PYBRON
	r@PYPADY <- PYPADY
	r@PANDW <- PANDW
	r@DWT <- DWT
	r@d <- res[1:simday,]
	
	return(r)
} 

