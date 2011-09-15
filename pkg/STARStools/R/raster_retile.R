#' @title Tiles raster data
#' Tiles raster data (with optional overlap).
#' @param raster Input raster, brick or stack.
#' @param tilebasename Character. Output basename for tiles.
#' @param targetDir Character. Output directory for the tiles.
#' @param ps Numeric or vector. Tile size in pixel units.  Can be a single value or c(x tile size,y tile size).
#' @param ol Numeric or vector. Overlap size in pixel units.  Can be a single value or c(x overlap size,y overlap size).
#' @param outformat Character. Output format of the tiles.  Use gdalDrivers() (part of rgdal) for the format codes.
#' @param overwrite Logical. Should existing files be overwritten?
#' @param progress: Character. Display processing progress. Valid values are "text", "window" and "" (no processing bar).
#' @name raster_retile
#' @author Jonathan A. Greenberg \email{STARStools@@estarcion.net}


raster_retile=function(raster,tilebasename,targetDir,
		ps=c(256,256),ol=c(0,0),
		outformat="raster",overwrite=FALSE,progress="")
{
	# Do some checks
	if(length(ps)==1)
	{
		ps=c(ps,ps)
	}
	
	if(length(ol)==1)
	{
		ol=c(ol,ol)
	}
	
	initial_directory=getwd()
	setwd(targetDir)
	raster_extent=extent(raster)
	raster_resolution=res(raster)
	ps_native_resolution=ps*raster_resolution
	ol_native_resolution=ol*raster_resolution
	num_x_tiles=ceiling((raster_extent@xmax-raster_extent@xmin)/ps_native_resolution[1])
	num_y_tiles=ceiling((raster_extent@ymax-raster_extent@ymin)/ps_native_resolution[2])
	
	# Extent objects are sorted, so we need to check for inverted coordinates
	ul_coord=xyFromCell(raster,1)
	lr_coord=xyFromCell(raster,ncol(raster)*nrow(raster))
	
	invert_coord=ul_coord>lr_coord
	
	if(invert_coord[1]) 
	{
		ul_x=raster_extent@xmax-(((1:num_x_tiles)-1)*ps_native_resolution[1])
	} else {
		ul_x=raster_extent@xmin+(((1:num_x_tiles)-1)*ps_native_resolution[1])
	}
	
	if(invert_coord[2]) 
	{
		ul_y=raster_extent@ymax-(((1:num_y_tiles)-1)*ps_native_resolution[2])
	} else
	{
		ul_y=raster_extent@ymin+(((1:num_y_tiles)-1)*ps_native_resolution[2])
	}
	
	ul=expand.grid(ul_x,ul_y)
	ul_list=split(ul,1:nrow(ul))
	tile_ids=expand.grid(((1:num_x_tiles)-1),((1:num_y_tiles)-1))
	max_number_width=max(ceiling(log10(num_x_tiles+1)),ceiling(log10(num_y_tiles+1)))
	tilenames=paste(tilebasename,
			formatC(tile_ids[,1],width=max_number_width,flag="0"),
			formatC(tile_ids[,2],width=max_number_width,flag="0"),sep="_")
	
	raster_retile_single_tile=function(raster,tilename,targetDir,
			ps_native_resolution,ol_native_resolution,outformat,overwrite,progress,
			ul_single,invert_coord)
	{
		if(invert_coord[1]) 
		{
			xmin_tile=as.numeric(ul_single[1])+ol_native_resolution[1]
			xmax_tile=as.numeric(ul_single[1])-ps_native_resolution[1]-ol_native_resolution[1]
		} else {
			xmin_tile=as.numeric(ul_single[1])-ol_native_resolution[1]
			xmax_tile=as.numeric(ul_single[1])+ps_native_resolution[1]+ol_native_resolution[1]
		}
		
		if(invert_coord[2]) 
		{
			ymin_tile=as.numeric(ul_single[2])+ol_native_resolution[2]
			ymax_tile=as.numeric(ul_single[2])-ps_native_resolution[2]-ol_native_resolution[2]
		} else
		{
			ymin_tile=as.numeric(ul_single[2])-ol_native_resolution[2]
			ymax_tile=as.numeric(ul_single[2])+ps_native_resolution[2]+ol_native_resolution[2]
		}

		tile_extent=extent(c(sort(c(xmin_tile,xmax_tile)),sort(c(ymin_tile,ymax_tile))))
		
#		tile_extent=extent(c(
#				as.numeric(ul_single[1])-ol_native_resolution[1],
#				as.numeric(ul_single[1])+ps_native_resolution[1]+ol_native_resolution[1],
#				as.numeric(ul_single[2])-ol_native_resolution[2],
#				as.numeric(ul_single[2])+ps_native_resolution[2]+ol_native_resolution[2]
#		))
		tile_raster=crop(raster,tile_extent,filename=tilename,format=outformat,overwrite=overwrite)
		return(tile_raster)
	}
	
	raster_tiles=mapply(raster_retile_single_tile,tilename=tilenames,ul_single=ul_list,
			MoreArgs=list(raster=raster,targetDir=targetDir,
			ps_native_resolution=ps_native_resolution,ol_native_resolution=ol_native_resolution,
			outformat=outformat,overwrite=overwrite,progress=progress,invert_coord))
	
	setwd(intial_directory)
	return(raster_tiles)
	
}