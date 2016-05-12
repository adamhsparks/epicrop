# Author: Robert J. Hijmans, r.hijmans@gmail.com; Jorrel Khalil S. Aunario; and 
# Adam H. Sparks, adamhsparks@gmail.com
# Date: 18 May 2012
# Version 0.2
# License GPL3

getLandCells <- function(tablename, odbcname){ # tablename is database table name, usually nasa_1d, odbcname is usually geoclimate
    cnt <- 0
    repeat {
		cnt<-cnt+1
		con <- odbcConnect(odbcname)	
		if (con != -1){
			break
		}
		else if (cnt > 4) {
			cat("Unable to connect to server \n")
			stop();
		}
		rm(con)
		cat("Retrying to connect. \n")
		flush.console()
	}
  maskset <- sqlQuery(con, paste(
                          "SELECT maskset_id FROM datasets WHERE table_name ='", 
                          tablename, "'", sep = ""))$maskset_id
	lc <- sqlQuery(con, paste("SELECT cell FROM maskcells WHERE maskset_id =", 
                            maskset, " AND land = TRUE", sep = ""))$cell
	odbcClose(con)
	return(lc)	
}

spatSim <- function(raster, model, starts, verbose = FALSE, ...)  {
	res(raster) <- 1

	nruns <- length(start)
	onedegworld <- raster()
	cells <- cellsFromExtent(onedegworld, raster)
	if (ncell(raster) != length(cells)) { stop("not good") }
	
	result <- matrix(NA, nrow = length(cells), ncol = length(starts))
	
	land <- getLandCells()
	cnt <- 0
	for (cell in cells) {
		cnt <- cnt + 1			
		if (verbose) {
			# for debugging or progress tracking
			cat("\r", rep.int(" ", getOption("width")), sep="")
			cat("\r", "cell: ", cell)
			flush.console() 
		}
		if ((cell-1) %in% land) {
#			if (wtness == 0) {
			wth <- DBgetWthCell(tablename, 'daily', cell-1)			
#			}
#			else{
#				xy <- xyFromCell(onedegworld, cell)
#				wth <- DBgetWthLWCell(tablename, 'daily', cell-1, xy[2])
#			}
			wth$year <- yearFromDate(wth$day)
			wth$prec[is.na(wth$prec)] <- 0
			
			for (d in 1:length(starts)) {
				result[cnt, d] <- model(wth, emergence = starts[d])
			}
		}
		else {
			result[cnt] <- NA
		}
	}

    rStack <- new('RasterStack')
	for (d in 1:length(starts)) {
		r <- setValues(raster, result[,d])
		rStack <- addLayer(rStack, r)    
	}
	return(rStack)
}

spatSimFlex <- function(region, model, outcolnames, years, pdateraster, 
                        tablename, croppingraster = NULL, 
                        nosinglecrop = FALSE, mcount = 4, period = 14, 
                        periodpt = 7, skipzero = TRUE, verbose = FALSE, 
                        out, ...){
    onedegworld <- raster()
    if (!file.exists(out)) dir.create(out, recursive = TRUE)
	  res(BaseRaster) <- 1
	  cells <- cellsFromExtent(onedegworld, BaseRaster)
    
	#nruns <- length(start)
	pcells <- cellsFromExtent(raster(), pdateraster)
	cwpd <- pcells[which(pdateraster[]>0)]
	
	if (!is.null(croppingraster)& class(croppingraster) == "RasterLayer"){
        ccells <- cellsFromExtent(onedegworld, croppingraster)
        checkcropping <- TRUE
    } else {
        checkcropping <- FALSE
    }
	
    inc <- which(cells %in% cwpd)
    cells <- cells[inc] 
	#if (ncell(raster) != length(cells)) { stop("not good") }
	
	result <- matrix(NA, nrow = length(cells), ncol = length(years)*mcount)
	
	land <- getLandCells(tablename)
	cnt <- 0
	for (cell in cells) {
		cnt <- cnt + 1			
		if (verbose) {
			# for debugging or progress tracking
			cat("\r", rep.int(" ", getOption("width")), sep = "")
			cat("\r", "cell: " , cell)
			flush.console()
		}
		
		#if (pdateraster[pcells == cell] == 0 & skipzero) next
				
		if ((cell-1) %in% land) {
#			if (wtness == 0) {
            xy <- xyFromCell(onedegworld, cell)
			wth <- DBgetWthXY(con, tablename, xy[1], xy[2])			
#			}
#			else{
#				xy <- xyFromCell(onedegworld, cell)
#				wth <- DBgetWthLWCell(tablename, 'daily', cell-1, xy[2])
#			}
			wth@w$prec[is.na(wth@w$prec)] <- 0
			wth@w$rhmin[is.na(wth@w$rhmin)] <- 0
			wth@w$tavg[is.na(wth@w$tavg)] <- (wth@w$tmin[is.na(wth@w$tavg)] + 
      wth@w$tmax[is.na(wth@w$tavg)])/2
			wth@w$rhmax[is.na(wth@w$rhmax)] <- wth@w$rhmin[is.na(wth@w$rhmax)]
			cellresults <- numeric(0)
			
		  if (checkcropping){
              if(nosinglecrop & croppingraster[ccells == cell]>1) PCISrice <- 1 
              else PCISrice <- 0     
      }

			for (d in 1:length(years)) {
				if (pdateraster[pcells == cell]>0){
					pdate <- dateFromDoy(
            (pdateraster[pcells == cell]-1)*period+periodpt,years[d])
			} else {
					pdate <- paste(years[d], "5-15", sep="-") 
			}                             
			if (checkcropping) cellresults <- c(cellresults, model(wth, 
                                          emergence=pdate, PCISrice = PCISrice)) 
        else cellresults <- c(cellresults,model(wth, emergence=pdate))
			}
      result[cnt, ] <- cellresults
		}
		else {
			result[cnt,] <- NA
		}
	}
    #models <- c("leafblast", "brownspot", "bactblight", "sheathblight")
    cnames <- character(0)	
    for (y in years){
        cnames <- c(cnames,paste(outcolnames, y, sep = ""))
    }
    colnames(result) <- cnames
	for (i in 1:ncol(result)) {
	    r <- raster(BaseRaster)
		r[inc] <- result[,i]
		writeRaster(r, paste(out, paste(colnames(result)[i],".tif", sep = ""), 
                  sep = "/"), ...)
        rm(r)		    
	}
    return(stack
           (paste(out, paste(colnames(result),".tif", sep = ""), sep = "/")))
}

simulate.Region <- function(model, plantingdate, wthsource, years=1998,region=extent(115,128,4,21), ncroppings = NULL, nosinglecrop = FALSE, outpath=NA, verbose = TRUE, ...){
    
	if (!is.na(outpath) & !file.exists(outpath)) dir.create(outpath, recursive = TRUE)
    
    if (class(plantingdate)!="RasterLayer") {
        stop("plantingdate should be RasterLayer")
    }
	
    aoiraster <- crop(plantingdate, region)    
	xy <- xyFromCell(aoiraster,1:ncell(aoiraster)) # recent change
    
    pdcells <- cellFromXY(plantingdate, xy) 
    
	if (!is.null(ncroppings) & class(ncroppings)=="RasterLayer"){
        nccells <- cellFromXY(ncroppings, xy)
    }
	
    results <- vector()
	for (i in 1:ncell(aoiraster)) {
        #cell <- cells[i]
        if (is.na(plantingdate[pdcells[i]]) | plantingdate[pdcells[i]]<1) next
        
        if (!is.null(ncroppings)) {
          if (nosinglecrop & ncroppings[nccells[i]]<1) next  
        } 
		
		if (verbose) {
			# for debugging or progress tracking
			cat("\r", rep.int(" ", getOption("width")), sep = "")
			cat("\r", "cell: " , i)
			flush.console()
		}
		
		#wth <- DBgetWthCell(con, wthdataset, wthcells[i])
		wth <- wthsource(as.matrix(t(xy[i,])))			
		
       	if (nrow(wth@w)>0){
            tavgcol <- which(colnames(wth@w) %in% c("t2m","tavg"))
    	    if(length(tavgcol)==0) wth@w$tavg <- (wth@w$tmin+wth@w$tmax)/2 else colnames(wth@w)[tavgcol] <- "tavg"
           	rhnx <- rhMinMax(wth@w$rh2m, wth@w$tmin, wth@w$tmax, wth@w$tavg)
            wth@w$rhmin <- rhnx[,1]
        	wth@w$rhmax <- rhnx[,2]
			wth@w$prec[is.na(wth@w$prec)] <- 0
			wth@w$rhmin[is.na(wth@w$rhmin)] <- 0
			wth@w$tavg[is.na(wth@w$tavg)] <- (wth@w$tmin[is.na(wth@w$tavg)] + wth@w$tmax[is.na(wth@w$tavg)])/2
			wth@w$rhmax[is.na(wth@w$rhmax)] <- wth@w$rhmin[is.na(wth@w$rhmax)]
        } else {
            if (verbose) {
    			# for debugging or progress tracking
    			message(rep("", getOption("width")), "\r", appendLF=TRUE)
    			message("cell: " , i, " - No Data.\n")
    			flush.console()
    		}
    		next
        }

		for (d in 1:length(years)) {
            pdate <- dateFromDoy(plantingdate[pdcells[i]],years[d])
			results <- rbind(results, c(i, years[d], model(wth, emergence=pdate)))
		}    
	}
    
	regpre <- paste(xmin(region), xmax(region), ymin(region), ymax(region), sep="_")
    for (y in years){
        thisyear <- results[results[,2]==y,]
        for (cc in 3:ncol(results)){
            r <- raster(aoiraster)
            r[thisyear[,1]] <- thisyear[,cc]
		    writeRaster(r, paste(outpath, paste(y, "_", regpre, "_", colnames(results)[cc],".tif", sep = ""),sep = "/"),format="GTiff", options=c("COMPRESS=LZW", "TFW=YES"), overwrite=T)
            rm(r)
            gc(verbose=FALSE)    
        }
    }
    
    return(dir(outpath, pattern=paste(regpre,".*.tif", sep=""), full.names=TRUE))
}
