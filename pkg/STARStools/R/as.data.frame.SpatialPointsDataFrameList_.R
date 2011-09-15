#' as.data.frame.SpatialPointsDataFrameList
#' @param x TODO
#' @param ... TODO
#' @export

as.data.frame.SpatialPointsDataFrameList=function(x,...)
{
	data=x@data
	
	# We need to attempt to coerce the list to a data.frame.
	f <- function(x) { rbind(as.data.frame(x)) }
	list_as_data.frame=mapply(function(x) {
				raw_data.frame=as.data.frame(x)
				if(length(raw_data.frame[,1])==0) { 
					raw_data.frame=as.data.frame(NA) 
				} else { 
					raw_data.frame=x 
				}
				names(raw_data.frame)=NA
				return(raw_data.frame)
			},x@list,SIMPLIFY=TRUE)
	
	list_to_data.frame=as.data.frame(do.call("rbind",list_as_data.frame))
	output=cbind(data,list_to_data.frame)
}
