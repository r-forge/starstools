#' aggregate.SpatialPointsDataFrameListZoo
#' @param x TODO
#' @param by TODO
#' @param ... TODO
#' @export

aggregate.SpatialPointsDataFrameListZoo=function(x,by,...)
{
	aggregation=mapply(zoo:::aggregate.zoo,x@list,by=by@list,MoreArgs=list(...),simplify=TRUE)
	x@list=aggregation
	return(x)
}
