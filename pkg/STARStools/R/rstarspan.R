#' Extracts raster data at vector locations.
#' @param vectors a vector of superclass "Spatial" or a list of vectors.
#' @param rasters a raster of superclass "Raster" or a list of rasters.
#' @param outformat Character.  Can be "SpatialDataFrame" (appends the extracted data to the original Spatial* object) or "DataFrame" (only returns the data.frame portion).
#' @param ... See ?extract for parameters (e.g. "fun") that can be used.
#' @note If the input vector(s) are SpatialPolygonsDataFrame, the user must assign "fun" if outformat="SpatialDataFrame".
#' #' @examples
#' tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="STARStools"))
#' tahoe_highrez_training <- readOGR(dsn=system.file("external", package="STARStools"),layer="tahoe_highrez_training")
#' tahoe_training_extraction <- rstarspan(vectors=tahoe_highrez_training,rasters=tahoe_highrez,outformat="SpatialDataFrame",fun=mean)
#' @author Jonathan A. Greenberg \email{STARStools@@estarcion.net}
#' @name rstarspan
#' @export

rstarspan=function(vectors,rasters,outformat="SpatialDataFrame",...)
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
	
	single_vector_extraction=function(vector,rasters,merge_with_dataframe=TRUE,return_Spatial=TRUE,...)
	{
		rasters_names=unlist(sapply(rasters,function(x) x@layernames))
		single_vector_extraction_data=mapply(extract_synced,x=rasters,MoreArgs=list(y=vector,...),SIMPLIFY=FALSE)
		
		if(class(vector)=="SpatialPolygonsDataFrame")
		{
			# If vectors are polygons...
			single_vector_extraction_data_list=single_vector_extraction_data[[1]]
			if(class(single_vector_extraction_data_list)=="list")
			{
				single_vector_extraction_data_list_t=mapply(t,x=single_vector_extraction_data_list,SIMPLIFY=FALSE)
				single_vector_extraction_data=t(do.call("cbind",single_vector_extraction_data_list_t))
				single_vector_extraction_data_list_t_N=sapply(single_vector_extraction_data_list_t,length)/ncol(single_vector_extraction_data)
			
			} else
			{
				single_vector_extraction_data=as.data.frame(single_vector_extraction_data_list)
				single_vector_extraction_data_list_t_N=rep(1,nrow(single_vector_extraction_data_list))
			}
			colnames(single_vector_extraction_data)=rasters_names
			
			if(merge_with_dataframe)
			{
				single_vector_data=vector@data
				
				# http://stackoverflow.com/questions/2894775/how-can-you-replicate-each-row-of-an-r-data-frame-and-specify-the-number-of-repli
				single_vector_data_wfreq=cbind(single_vector_data,single_vector_extraction_data_list_t_N)
				names(single_vector_data_wfreq)[ncol(single_vector_data_wfreq)]="freq"
				single_vector_data_expanded=
						single_vector_data_wfreq[rep(row.names(single_vector_data_wfreq), 
						single_vector_data_wfreq$freq), 1:(ncol(single_vector_data_wfreq)-1)]				
				single_vector_extraction_data=cbind(single_vector_data_expanded,single_vector_extraction_data)
			}
		}
		
		if(inherits(vector,"SpatialPointsDataFrame"))
		{
			# If vectors are points...
			single_vector_extraction_data=do.call("cbind",single_vector_extraction_data)
			colnames(single_vector_extraction_data)=rasters_names
			if(merge_with_dataframe)
			{
				single_vector_extraction_data=cbind(vector@data,single_vector_extraction_data)
			}
			single_vector_extraction_data_list_t_N=rep(1,nrow(single_vector_extraction_data))
		}		
		
#		print(sum(single_vector_extraction_data_list_t_N))
#		print(length(single_vector_extraction_data_list_t_N))
#		
		if(return_Spatial)
		{
			if(sum(single_vector_extraction_data_list_t_N) != length(single_vector_extraction_data_list_t_N) && class(vector)=="SpatialPolygonsDataFrame")
			{
				print("return_Spatial=TRUE will not work if fun is not set for a SpatialPolygonsDataFrame, returning data frame.")
				return(single_vector_extraction_data)				
			} else
			{
				vector@data=single_vector_extraction_data
				return(vector)
			}
		} else
		{
			return(single_vector_extraction_data)
		}
	}
	
	### Appends the extracted data to the existing data frame.
	if(outformat=="SpatialDataFrame")
	{
		rstarspan_output=mapply(single_vector_extraction,vector=vectors,
				MoreArgs=list(rasters=rasters,merge_with_dataframe=TRUE,return_Spatial=TRUE,...))
		if(length(vectors)==1)
		{
			rstarspan_output=rstarspan_output[[1]]
		}
		
	}
	
	if(outformat=="DataFrame")
	{
		rstarspan_output=mapply(single_vector_extraction,vector=vectors,
				MoreArgs=list(rasters=rasters,merge_with_dataframe=TRUE,return_Spatial=FALSE, ...),SIMPLIFY=FALSE)
		if(length(vectors)==1)
		{
			rstarspan_output=rstarspan_output[[1]]
		}
	}
	
	
	return(rstarspan_output)
}
