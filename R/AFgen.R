
AFGen <- function(xy, x) {
	# it would be safe to first sort xy by x
	d <- dim(xy)
	if (x <= xy[1,1]) {res <- xy[1,2] 
	} else if (x >= xy[d[1] ,1]) {res <- xy[d[1],2] 
	} else {
		a <- xy[xy[,1] <= x,]
		b <- xy[xy[,1] >= x,]
		if (length(a)==2) {
			int <- rbind(a, b[1,])
		} else if (length(b)==2) {
			int <- rbind(a[dim(a)[1],], b)
		} else {
			int <- rbind(a[dim(a)[1],], b[1,])
		}	
		if(x==int[1,1]) { res <- int[1,2]
		} else if (x==int[2,1]) { res <- int[2,2]
		} else {
			res <- int[1,2] +  ( x - int[1,1] ) * ((int[2,2] - int[1,2]) / (int[2,1] - int[1,1]))
		}
	}
	return(res[[1]])
}
