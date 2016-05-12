# R classes 
# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : September 2009
# Version 0.1
# Licence GPL v3


setClass('SEIR',
	representation (
		d      = 'data.frame'
	),
	prototype (	
		d = data.frame()
	),	
	validity = function(object)
	{
		return(TRUE)
	}
)
	


setMethod ('show' , 'SEIR', 
	function(object) {
		cat('class   :' , class(object), '\n')
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



