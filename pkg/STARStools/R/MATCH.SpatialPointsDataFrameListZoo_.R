#' MATCH.SpatialPointsDataFrameListZoo
#' @param x TODO
#' @param ... TODO
#' @export

MATCH.SpatialPointsDataFrameListZoo=function(x,...)
{
	newlist=mapply(MATCH,index(x,SpatialDataFrameList=FALSE),MoreArgs=list(...),SIMPLIFY=FALSE)
	x@list=newlist
	return(x)
	
}
