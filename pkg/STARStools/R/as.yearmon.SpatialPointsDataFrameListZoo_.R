#' as.yearmon.SpatialPointsDataFrameListZoo
#' @param x TODO
#' @param ... TODO
#' @export

as.yearmon.SpatialPointsDataFrameListZoo=function(x,...)
{
	newlist=mapply(zoo:::as.yearmon,x@list,MoreArgs=list(...),simplify=FALSE)
	x@list=newlist
	return(x)
}



