time.SpatialPointsDataFrameListZoo=function(x,...)
{
	newlist=mapply(zoo:::time.zoo,x@list,simplify=FALSE)
	x@list=newlist
	return(x)
	
}
