
audpc <- function(x) {
	if (length(x$severity) > 0) {
		return(sum(x$severity))
	} else 	if (length(x$incidence) > 0) {
		return(sum(x$incidence))
	} else {
		stop('cannot find incidence or severity')
	}
}


raudpc <- function(x) {
	audpc(x) / length(x[,1])
}
