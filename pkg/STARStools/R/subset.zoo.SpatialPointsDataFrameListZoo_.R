#' subset.zoo.SpatialPointsDataFrameListZoo
#' @param x TODO
#' @param subset TODO
#' @param ... TODO
#' @export

subset.zoo.SpatialPointsDataFrameListZoo=function(x,subset,...)
{	
	newlist=mapply(function(x,subset,...) { subset(x,!is.na(subset)) },x@list,subset@list,MoreArgs=list(...),simplify=FALSE)
	x@list=newlist
	return(x)
}
