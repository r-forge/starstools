#' as.SpatialPointsDataFrameList
#' @param SpatialPointsDataFrame TODO
#' @param list TODO
#' @param by TODO
#' @param all TODO
#' @export

as.SpatialPointsDataFrameList=function(SpatialPointsDataFrame,list,by,all=FALSE)
{
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
	
	SpatialPointsDataFrame_N=length(SpatialPointsDataFrame)
	list_N=length(list)
	
	if(!missing(by))
	{
		# Look for merge field in SpatialPointsData@data.
		SpatialPointsDataFrame_fields=names(SpatialPointsDataFrame@data)
		if(by %in% SpatialPointsDataFrame_fields)
		{
			unique_ids=intersect(SpatialPointsDataFrame@data[,which(names(SpatialPointsDataFrame@data)==by)],names(list))
			if(!all)
			{
				# Remove the points which do not have matches.
				SpatialPointsDataFrame=SpatialPointsDataFrame[SpatialPointsDataFrame@data[,which(names(SpatialPointsDataFrame@data)==by)] %in% unique_ids,]

			}
			# Create an index to help with the merge.
			list_merge_dataframe=data.frame(names(list),1:list_N)
			names(list_merge_dataframe)=c(by,"list_index")
			SpatialPointsDataFrame_merge_dataframe=data.frame(SpatialPointsDataFrame@data[,which(names(SpatialPointsDataFrame@data)==by)])
			names(SpatialPointsDataFrame_merge_dataframe)=by
			merge_dataframe=merge(SpatialPointsDataFrame_merge_dataframe,list_merge_dataframe,all.x=TRUE,by=by)
			ordered_list=list[merge_dataframe$list_index]
		} else
		{
			print(paste("Could not find '",by,"' in the names.",sep=""))
			return()
		}
		
		
	} else
	{
	# Assumes (but checks) equal sized and ordered SpatialPointsDataFrame and list.
		if(SpatialPointsDataFrame_N != list_N)
		{
			print("length(SpatialPointsDataFrame) must equal length(list).  Perhaps try by.x=(common field),by.y=(common field)")
			return()
		} else
		{
			ordered_list=list
		}
	}	
	SpatialPointsDataFrameList=new("SpatialPointsDataFrameList",list=ordered_list,data=SpatialPointsDataFrame@data,
			coords.nrs=SpatialPointsDataFrame@coords.nrs,coords=SpatialPointsDataFrame@coords,bbox=SpatialPointsDataFrame@bbox,
			proj4string=SpatialPointsDataFrame@proj4string)
	
}
