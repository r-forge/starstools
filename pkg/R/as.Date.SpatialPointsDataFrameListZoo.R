# TODO: Add comment
# 
# Author: jonathan
###############################################################################

as.Date.SpatialPointsDataFrameListZoo=function(x,...)
{
	newlist=mapply(zoo:::as.Date,x@list,MoreArgs=list(...),simplify=FALSE)
	x@list=newlist
	return(x)
}

