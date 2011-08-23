# TODO: Add comment
# 
# Author: jonathan
###############################################################################

as.SpatialPointsDataFrameListZoo=function(SpatialPointsDataFrame,list,by=NA,all=FALSE)
{
	# The only real difference between this class and SpatialPointsDataFrameList is that all members of the list must be zoo objects.
	if(class(SpatialPointsDataFrame)!="SpatialPointsDataFrame")
	{
		print('SpatialPointsData must be of class(SpatialPointsDataFrame)=="SpatialPointsDataFrame"')
		return()
	}
	
	if(class(list)!="list")
	{
		print('list must be of class(list)=="list"')
		return()
	}

	zoo_test=check_list_classes(list,"zoo",check_inheritance=FALSE)
	
#	zoo_test=sapply(list,class)
#	zoo_test_unique=unique(zoo_test)
	if(!zoo_test)
	{
		print("Each element of the list must be a zoo object.")
		return
	} else
	{
		SpatialPointsDataFrameList=as.SpatialPointsDataFrameList(SpatialPointsDataFrame,list,by,all)
		
	}
	
	SpatialPointsDataFrameListZoo=new("SpatialPointsDataFrameListZoo",list=SpatialPointsDataFrameList@list,data=SpatialPointsDataFrameList@data,
			coords.nrs=SpatialPointsDataFrameList@coords.nrs,coords=SpatialPointsDataFrameList@coords,bbox=SpatialPointsDataFrameList@bbox,
			proj4string=SpatialPointsDataFrameList@proj4string)
	
	return(SpatialPointsDataFrameListZoo)
	
}
