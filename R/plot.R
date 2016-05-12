# R classes 
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.1
# Licence GPL v3


setMethod ('plot', signature(x='SEIR', y='missing'),
	function(x, y, type=1, ...) {
		x <- x@d
		if (type==1) {.splot1(x)
		} else if (type==2) {.splot2(x)
		} else if (type==3) {.splot3(x)
		} else if (type==4) {.splot4(x)
		} else if (type==5) {.splot5(x)
		} else {
			stop('invalid type')
		}
	}
)
		

.splot1 <- function(x, ylim=c(0,500), ...) {
	plot(x$date, x$sites, ylim=ylim, ylab="sites",...)
	points(x$date, x$diseased, col="gray")
	points(x$date, x$removed, col="blue")
	points(x$date, x$latent, col="red")
	points(x$date, x$infectious,  col="green")
	legend(x$date[2], ylim[2]-10, c("diseased", "removed", "latent", "infectious"), col=c("gray", "blue", "red", "green"), pch=21)
}	



.splot2 <- function(x, ...) {
	plot(x$date, x$sites, ylab="sites", ...)
	points(x$date, x$diseased, col="gray", ...)
	points(x$date, x$removed, col="blue", ...)
	points(x$date, x$latent, col="red",...)
	points(x$date, x$infectious,  col="green", ...)
	legend(0, max(x$sites), legend=c("sites", "diseased", "removed", "latent", "infectious"), col=c("black", "gray", "blue", "red", "green"), pch=21)
}	

.splot3 <- function(x, ...) {
	plot(x$date, x$severity, ylim=c(0, 100),  ylab="incidence or severity (%)", ...)
	legend(0, 60, c("severity"), col=c("black"), pch=21)
}	



.splot4 <- function(x, vars=c('sites', 'diseased', 'removed', 'latent', 'infectious'), cols=c('black', 'gray', 'blue', 'red', 'green'), ...) {
	plot(x$date, x[[vars[1]]], xlab="time", ylab="sites", col=cols[1], ...)
	if (length(vars) > 1) {
		for (i in 2:length(vars)) {
			points(x$date, x[[vars[i]]], ylab="sites", col=cols[i], ...)
		}
	}
}



.splot5 <- function(x, vars=2:length(x[1,]), ...) {
	cols=c('black', 'gray', 'blue', 'red', 'green')
	plot(x$date, x[,vars[1]], xlab="time", ylab="sites", col=cols[1], ...)
	if (length(vars) > 1) {
		for (i in 2:length(vars)) {
			points(x$date, x[,vars[i]], ylab="sites", col=cols[i], ...)
		}
	}
}
