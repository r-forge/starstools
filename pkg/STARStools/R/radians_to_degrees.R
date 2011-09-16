#' Converts radians to degrees (with circular correction)
#' @param radians Numeric vector of radians.
#' @param circular Logical. TRUE=Ouput degrees are corrected for circularity, e.g. 3*pi yields 180 degrees, not 540 degrees.
#' @examples 
#' radians=c(0,(1/2)*pi,pi,(3/2)*pi,2*pi,3*pi)
#' # Convert radians to pi with circularity correction.
#' radians_to_degrees(radians,circular=TRUE)
#' # Convert radians to pi without circularity correction.
#' radians_to_degrees(radians,circular=FALSE)
#' @name radians_to_degrees
#' @author Jonathan A. Greenberg \email{STARStools@@estarcion.net}
#' @export

radians_to_degrees=function(radians,circular=TRUE)
{
	if(circular)
	{
		degrees=(radians*180/pi) %% 360
		
	} else
	{
		degrees=(radians*180/pi)
	}
	return(degrees)
}
