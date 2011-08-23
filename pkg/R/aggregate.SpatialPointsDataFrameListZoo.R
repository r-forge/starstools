# TODO: Add comment
# 
# Author: jonathan
###############################################################################


aggregate.SpatialPointsDataFrameListZoo=function(x,by,...)
{
	aggregation=mapply(zoo:::aggregate.zoo,x@list,by=by@list,MoreArgs=list(...),simplify=TRUE)
	x@list=aggregation
	return(x)
}
