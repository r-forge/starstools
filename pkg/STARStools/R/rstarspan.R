#' Extracts raster data at vector locations.
#' @param vectors a vector of superclass "Spatial" or a list of vectors.
#' @param rasters a raster of superclass "Raster" or a list of rasters.
#' @param outformat Character.  Can be "SpatialDataFrame" (appends the extracted data to the original Spatial* object) or "DataFrame" (only returns the data.frame portion).
#' @param ... See ?extract for parameters (e.g. "fun") that can be used.
#' @note If the input vector(s) are SpatialPolygonsDataFrame, the user must assign "fun" if outformat="SpatialDataFrame".
#' #' @examples
#' tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="STARStools"))
#' tahoe_highrez_training_polygons <- readOGR(dsn=system.file("external", package="STARStools"),layer="tahoe_highrez_training")
#' tahoe_training_extraction <- rstarspan(vectors=tahoe_highrez_training,rasters=tahoe_highrez,outformat="SpatialDataFrame",fun=mean)
#' tahoe_highrez_training_points <- readOGR(dsn=system.file("external", package="STARStools"),layer="tahoe_highrez_training_points")
#' tahoe_training_extraction_minirasterstrip <- rstarspan(vectors=tahoe_highrez_training_points,
#' rasters=tahoe_highrez,outformat="MiniRasterStrip",borderx=5,bordery=5)
#' @author Jonathan A. Greenberg \email{STARStools@@estarcion.net}
#' @name rstarspan
#' @export

rstarspan=function(vectors,rasters,outformat="SpatialDataFrame",borderx=0,bordery=0,force_window_odd=TRUE,
		miniraster_direction="vertical",miniraster_vector=TRUE,verbose=TRUE,...)
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
	
	if(verbose)
	{
		print("Coercing raster and vectors to as list...")
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
	
	single_vector_window_extraction=function(vector_raster_list,borderx,bordery,force_window_odd,...)
	{
		vector=vector_raster_list[[1]]
		raster=vector_raster_list[[2]]
		# Create extent objects for cropping, this will only work on one raster at a time currently.
		
		minirasters_list=crop_extents_synced(x=raster,y=vector,borderx=borderx,bordery=bordery,force_window_odd,verbose=verbose,...)
	
		return(minirasters_list)
	}
	
	### Appends the extracted data to the existing data frame.
	if(outformat=="SpatialDataFrame")
	{
		if(verbose)
		{
			print("Preparing a SpatialDataFrame...")
		}
		
		rstarspan_output=mapply(single_vector_extraction,vector=vectors,
				MoreArgs=list(rasters=rasters,merge_with_dataframe=TRUE,return_Spatial=TRUE,...))
		if(length(vectors)==1)
		{
			rstarspan_output=rstarspan_output[[1]]
		}
		
	}
	
	if(outformat=="DataFrame")
	{
		if(verbose)
		{
			print("Preparing a DataFrame...")
		}
		rstarspan_output=mapply(single_vector_extraction,vector=vectors,
				MoreArgs=list(rasters=rasters,merge_with_dataframe=TRUE,return_Spatial=FALSE, ...),SIMPLIFY=FALSE)
		if(length(vectors)==1)
		{
			rstarspan_output=rstarspan_output[[1]]
		}
	}
	
	if(outformat=="MiniRasters")
	{
		if(verbose)
		{
			print("Preparing MiniRasters...")
		}
		
		# Create a list combining all vectors and rasters.
		vector_raster_combos=expand.list(vectors,rasters)
		rstarspan_output=mapply(single_vector_window_extraction,vector_raster_list=vector_raster_combos,
				MoreArgs=list(borderx=borderx,bordery=bordery,force_window_odd,...),SIMPLIFY=FALSE)
		if(length(vectors)==1)
		{
			rstarspan_output=rstarspan_output[[1]]
		}
	}
	
	if(outformat=="MiniRasterStrip")
	{
		setOptions(todisk=TRUE)
		
		if(verbose)
		{
			print("Preparing a MiniRasterStrip...")
		}
		
		# Create a list combining all vectors and rasters.
		if(verbose)
		{
			print("Create a list combining all vectors and rasters...")
		}
	
		vector_raster_combos=expand.list(vectors,rasters)
		
		# Create minirasters first
		if(verbose)
		{
			print("Extracting minirasters...")
		}
		minirasters_list=mapply(single_vector_window_extraction,vector_raster_list=vector_raster_combos,
				MoreArgs=list(borderx=borderx,bordery=bordery,force_window_odd=TRUE,...),SIMPLIFY=FALSE)
		
		if(verbose)
		{
			print("Finished extracting minirasters...")
		}
		
		# TODO: This needs to work for multiple rasters and vectors
		if(length(vectors)==1)
		{
			minirasters_list=minirasters_list[[1]]
		}
		
		# Determine the maximum size of the minirasters
		minirasters_max_ncol=max(sapply(minirasters_list,ncol))
		minirasters_max_nrow=max(sapply(minirasters_list,nrow))
		
		# Sync all the minirasters
		if(verbose)
		{
			print(paste("Coercing all minirasters to ",minirasters_max_ncol," by ",minirasters_max_nrow," pixels...",sep=""))
		}
		minirasters_list_synced=mapply(spatial_sync_raster,unsynced=minirasters_list,
				MoreArgs=list(size_only=TRUE,raster_size=c(minirasters_max_ncol,minirasters_max_nrow)))
		
		# Add offsets to all of their extents
		if(miniraster_direction=="vertical")
		{
			if(verbose)
			{
				print("Minirasterstrip will be vertical...")
			}
			
			
			minirasters_offsets_ymin=as.list((length(minirasters_list_synced)-1):0*minirasters_max_nrow)
			minirasters_offsets_ymax=as.list(((length(minirasters_list_synced)-1):0*minirasters_max_nrow)+minirasters_max_nrow)
			minirasters_offsets_xmin=sapply(minirasters_list_synced,xmin,simplify=FALSE)
			minirasters_offsets_xmax=sapply(minirasters_list_synced,xmax,simplify=FALSE)
		} else
		{
			# TODO: Horizontal minirasterstrips
		}
		
		if(verbose)
		{
			print("Modifying miniraster extents for mosaic...")
		}
		
		minirasters_list_synced_offsets=mapply(
			function(miniraster,minirasters_offsets_xmin,minirasters_offsets_xmax,
				minirasters_offsets_ymin,minirasters_offsets_ymax)
				{
					offset_extent=extent(minirasters_offsets_xmin,minirasters_offsets_xmax,
						minirasters_offsets_ymin,minirasters_offsets_ymax)
					extent(miniraster)=offset_extent
					return(miniraster)
				},
				miniraster=minirasters_list_synced,
				minirasters_offsets_xmin=minirasters_offsets_xmin,minirasters_offsets_xmax=minirasters_offsets_xmax,
				minirasters_offsets_ymin=minirasters_offsets_ymin,minirasters_offsets_ymax=minirasters_offsets_ymax,
				SIMPLIFY=FALSE
		)

		if(verbose)
		{
			print("Mosaicking minirasters into the minirasterstrip...")
			print(minirasters_list_synced_offsets)
			print(class(minirasters_list_synced_offsets))
		}
		
#		minirasterstrip=mosaic(minirasters_list_synced_offsets,fun=mean)
		minirasterstrip=merge(minirasters_list_synced_offsets)
		print(class(minirasterstrip))
		rstarspan_output=minirasterstrip
		
		if(miniraster_vector)
		{
			if(verbose)
			{
				print("Creating the minirasterstrip vector...")
			}
			
			# Reform vectors
			minirasterstrip_vectors=mapply(
				function(vector_raster_combo,minirasters_offsets_xmin,minirasters_offsets_xmax,
						minirasters_offsets_ymin,minirasters_offsets_ymax)
				{
					vector=vector_raster_combo[[1]]
					if(class(vector)=="SpatialPointsDataFrame")
					{
						x_mean=mapply(function(x1,x2) { return(mean(c(x1,x2))) },x1=minirasters_offsets_xmin,x2=minirasters_offsets_xmax)
						y_mean=mapply(function(x1,x2) { return(mean(c(x1,x2))) },x1=minirasters_offsets_ymin,x2=minirasters_offsets_ymax)
						minirasterstrip_vector = as(vector, "data.frame")
						minirasterstrip_vector$x=x_mean
						minirasterstrip_vector$y=y_mean
						coordinates(minirasterstrip_vector) <- ~x+y
						return(minirasterstrip_vector)
					}
					minirasters_vector_id=1:length(minirasters_vector)
				},
				vector_raster_combo=vector_raster_combos,
				MoreArgs=list(minirasters_offsets_xmin=minirasters_offsets_xmin,minirasters_offsets_xmax=minirasters_offsets_xmax,
				minirasters_offsets_ymin=minirasters_offsets_ymin,minirasters_offsets_ymax=minirasters_offsets_ymax)
			)
			if(length(minirasterstrip_vectors)==1)
			{
				minirasterstrip_vectors=minirasterstrip_vectors[[1]]
			}
			rstarspan_output=list(rstarspan_output,minirasterstrip_vectors)
			setOptions(todisk=FALSE)
		}
		
	}
	if(verbose)
	{
		print("Finished!  Returning output.")
	}
	return(rstarspan_output)
}
