# TODO: Add comment
# 
# Author: jonathan
###############################################################################


rstarspan=function(vectors,rasters,outformat="SpatialDataFrame")
{
	if(!inherits(vectors,"Spatial") && !check_list_classes(vectors,"Spatial",check_inheritance=TRUE))
	{
		print("vectors must be a single vector of class inheriting Spatial or a list of vectors each of class inheriting Spatial.")
		return()
	}
	
	if(!inherits(rasters,"Raster") && !check_list_classes(rasters,"Raster",check_inheritance=TRUE))
	{
		print("rasters must be a single raster of class inheriting Raster or a list of rasters each of class inheriting Raster.")
		return()
	}
	
	if(class(vectors)!="list"){ vectors=list(vectors) }
	if(class(rasters)!="list"){ rasters=list(rasters) }
	
	single_vector_extraction=function(vector,rasters,merge_with_dataframe=TRUE,return_Spatial=TRUE)
	{
		rasters_names=unlist(sapply(rasters,function(x) x@layernames))
		single_vector_extraction_data=mapply(raster::extract,x=rasters,MoreArgs=list(y=vector))
		if(class(single_vector_extraction_data)=="list")
		{
			single_vector_extraction_data=do.call("cbind",single_vector_extraction_data)
		}
		
		
		colnames(single_vector_extraction_data)=rasters_names
		if(merge_with_dataframe)
		{
			single_vector_extraction_data=cbind(vector@data,single_vector_extraction_data)
		}
		
		if(return_Spatial)
		{
			vector@data=single_vector_extraction_data
			return(vector)
		} else
		{
			return(single_vector_extraction_data)
		}
	}
	
	### Appends the extracted data to the existing data frame.
	if(outformat=="SpatialDataFrame")
	{
		rstarspan_output=mapply(single_vector_extraction,vector=vectors,
				MoreArgs=list(rasters=rasters,merge_with_dataframe=TRUE,return_Spatial=TRUE))
		if(length(vectors)==1)
		{
			rstarspan_output=rstarspan_output[[1]]
		}
		
	}
	
	if(outformat=="DataFrame")
	{
		rstarspan_output=mapply(single_vector_extraction,vector=vectors,
				MoreArgs=list(rasters=rasters,merge_with_dataframe=TRUE,return_Spatial=FALSE))
		if(length(vectors)==1)
		{
			rstarspan_output=rstarspan_output[[1]]
		}
	}
	
	
	return(rstarspan_output)
}
