#' index.SpatialPointsDataFrameListZoo
#' @param x TODO
#' @param SpatialDataFrameList TODO
#' @param ... TODO
#' @export

index.SpatialPointsDataFrameListZoo=function(x,SpatialDataFrameList=TRUE,...)
{
	newlist=mapply(zoo:::index,x@list,MoreArgs=list(...),SIMPLIFY=FALSE)
	if(SpatialDataFrameList)
	{
		x@list=newlist
		return(x)
	} else
	{
		return(newlist)
	}
}
