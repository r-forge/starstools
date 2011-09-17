#' circ.mean with na.rm capability.
#' @param x vector of data points measured in radians or degrees.
#' @param na.rm Logical. Remove NAs before calculating?
#' @param unit Character. Input direction unit. Default is 'radians'.  Setting to 'degrees' assumes x is in degrees, and also returns the values in degrees.
#' @author Jonathan A. Greenberg \email{STARStools@@estarcion.net}
#' @examples 
#' degree_vector_with_NA=c(359,1,45,NA)
#' circ.mean.na.rm(degree_vector_with_NA,na.rm=FALSE,unit='degrees')
#' circ.mean.na.rm(degree_vector_with_NA,na.rm=TRUE,unit='degrees')
#' @export 

circ.mean.na.rm = function(x,unit="radians",na.rm=TRUE)
{
	if(missing(unit))
	{
		unit="radians"
	}
	
	if(unit=="degrees")
	{
		x=rad(x)
	}
	
#	if(na.rm)
#	{
#		x_mean=circ.mean(na.omit(x))
#	} else
#	{
#		x_mean=circ.mean(x)
#	}
	
	x_mean=as.numeric(mean.circular(x,na.rm=na.rm))
	
	if(unit=="degrees")
	{
		x_mean=deg(x_mean)
	}
	return(x_mean)
}