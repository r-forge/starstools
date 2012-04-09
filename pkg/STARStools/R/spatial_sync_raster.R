#' Spatially Sync Rasters
#' 
#' Aligns ("syncs") a Raster to a reference Raster.
#' 
#' Uses bilinear or nearest neighbor resampling to align a raster to the extent
#' and projection of a reference raster and match the resolution of the
#' reference raster.
#' 
#' @param unsynced A Raster object to be aligned to the reference raster.
#' @param reference A Raster object to be used as the reference for syncing.
#' Syncing will use the reference's projection, resolution, and extent.
#' @param method Method used to compute values for the new RasterLayer. Either
#' 'ngb' (nearest neighbor) or 'bilinear' (bilinear interpolation).
#' @param verbose verbose=TRUE gives feedback on the process (UNSUPPORTED AT
#' PRESENT).
#' @param size_only TODO
#' @param raster_size TODO
#' @return Returns a RasterLayer, RasterBrick or RasterStack object synced to
#' the reference raster object.
#' @author Jonathan A. Greenberg
#' @seealso \code{\link[climstats]{temporal_sync_raster}}
#' @keywords brick/stack calculate
#' @examples
#' 
#' \dontrun{
#' require(R.utils) #Can't be loaded after raster
#' require(climstats)
#' load(system.file("extdata/elevTahoe.RData",package="climstats"))
#' 
#' ppt=get_climate_data("PRISM-4km-ppt", 
#' 		date_range=c("1999/1/1","1999/12/31"), standardize=TRUE, 
#' 		enable_download=TRUE, overwrite=TRUE)
#' 		
#' pptTahoe99=spatial_sync_raster(ppt,elevTahoe,method="bilinear")
#' pptTahoe99
#' ppt99jan<-raster(pptTahoe99,layer=1)
#' plot(ppt99jan)
#' }
#' 
spatial_sync_raster <- function(unsynced,reference,method="ngb",size_only=FALSE,raster_size,verbose=FALSE)
{
	
	if(!size_only)
	{
		new_projection=projection(reference)
		old_projection=projection(unsynced)
		
		new_res=res(reference)
		old_res=res(unsynced)
		
		if(new_projection!=old_projection | new_res[1] != old_res[1] | new_res[2] != old_res[2])
		{
			pr_extent=projectExtent(unsynced, new_projection)
			# We need to fix the extent
			pr_extent <- setExtent(pr_extent,extent(reference))
			res(pr_extent)=res(reference)
			if(new_projection!=old_projection)
			{
				pr <- projectRaster(unsynced, pr_extent,method=method)
			} else
			{
				pr <- raster::resample(unsynced, pr_extent,method=method)
			}
		} else
		{
			pr=unsynced
		}
		
		expanded_raster=expand(pr,reference)
		synced_raster=crop(expanded_raster,reference)
	
	
		# This in theory shouldn't be neccessary...
		extent(synced_raster)=extent(reference)
	} else
	{
		if(missing(raster_size))
		{
			print("For size_only=TRUE you must set the raster_size as c(ncol,nrow)")
			return()
		} 
		
		unsynced_ncol=ncol(unsynced)
		unsynced_nrow=nrow(unsynced)
		
		# Eventually we should preserve the pixel size		
		unsynced_ulx=(raster_size[[1]]-unsynced_ncol)/2
		unsynced_uly=(raster_size[[2]]-unsynced_nrow)/2
		
		extent(unsynced)=extent(unsynced_ulx,unsynced_ulx+unsynced_ncol,unsynced_uly,unsynced_uly+unsynced_nrow)
		full_extent=extent(0,raster_size[[1]],0,raster_size[[2]])
		
		synced_raster=expand(unsynced,full_extent)
		extent(synced_raster)=full_extent
		res(synced_raster)=c(1,1)
	}

	return(synced_raster)
	
}
