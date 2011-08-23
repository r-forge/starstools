# TODO: Add comment
# 
# Author: jonathan
###############################################################################


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
