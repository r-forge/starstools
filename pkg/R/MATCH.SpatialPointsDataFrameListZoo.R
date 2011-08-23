# TODO: Add comment
# 
# Author: jonathan
###############################################################################


MATCH.SpatialPointsDataFrameListZoo=function(x,...)
{
	newlist=mapply(MATCH,index(x,SpatialDataFrameList=FALSE),MoreArgs=list(...),SIMPLIFY=FALSE)
	x@list=newlist
	return(x)
	
}
