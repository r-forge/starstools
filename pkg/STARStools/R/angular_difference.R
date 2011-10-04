#' Calculates the positive angular difference between two angles.
#' @title angular_distance
#' @param angle1 Numeric. First angle(s).
#' @param angle2 Numeric. Second angle(s).
#' @param units Character. The units of the input (and output).  Can be "degrees" (default) or "radians".
#' @param na.rm Logical. Unused.  Available for compatibility with raster.
#' @name angular_distance
#' @author Jonathan A. Greenberg \email{STARStools@@estarcion.net}
#' @examples
#' angle1=c(45,90,355,10)
#' angle2=c(45,95,5,345)
#' angular_difference(angle1,angle2)
#' @export

angular_difference <- function(angle1,angle2,na.rm=TRUE,units="degrees")
{
	if(units=="radians")
	{
		angle1=deg(angle1)
		angle2=deg(angle2)
	}
	
	diffangle=180-abs(180-abs(angle1-angle2))
	
	if(units=="radians")
	{
		diffangle=rad(diffangle)
	}
	
	return(diffangle)
}