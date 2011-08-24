# TODO: Add comment
# 
# Author: jonathan
###############################################################################


tile_raster=function(raster,ps=c(256,256),overlap=c(0,0),corner="ul")
{
	
	if(!inherits(rasters,"Raster"))
	{
		print("raster must be a single raster of class inheriting Raster (e.g. raster, stack or brick).")
		return()
	}
	
	if(length(ps)==1)
	{
		ps=c(ps,ps)
	}
	
	raster_extent=extent(raster)
	
}

## Scratch for titan
#require("raster")
#setwd("/proj/tahoe/raster/lidar/transforms")
#raster=raster("tahoe_hh_3x3_lowpass.envi")

