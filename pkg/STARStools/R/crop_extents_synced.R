#' crop_extents_synced
#' @param x TODO
#' @param y TODO
#' @param borderx x border in pixels
#' @param bordery y border in pixels
#' @param force_window_odd TODO
#' @param verbose TODO
#' @export

crop_extents_synced=function(x,y,borderx=0,bordery=0,force_window_odd=TRUE,verbose=TRUE,...)
{
	require("raster")
	# Sync the vector to the same coordinate system.
	if(verbose)
	{
		print("Syncing the vectors to the raster's projection...")
		
	}
	y_synced=spatial_sync_vector(y,x,verbose=FALSE)
	
	# Extract the bounding boxes of each input
	if(verbose)
	{
		print("Extracting the bounding boxes of each vector...")
		
	}
	y_synced_ids=1:length(y_synced)
	y_synced_extents=mapply(function(id,y_synced) { extent(y_synced[id,]) },id=y_synced_ids,MoreArgs=list(y_synced=y_synced),
			SIMPLIFY=FALSE)
	
	if(verbose)
	{
		print("Snapping the extents to the cell center...")
		
	}
	# We need to snap the extents to the nearest cell center.
	y_synced_extents_snapped=mapply(
			function(extent,x){
				ul=c(xmin(extent),ymin(extent))
				lr=c(xmax(extent),ymax(extent))
				ul_snapped=xyFromCell(x,cellFromXY(x,ul))
				lr_snapped=xyFromCell(x,cellFromXY(x,lr))
				extent_snapped=extent(ul_snapped[1],lr_snapped[1],ul_snapped[2],lr_snapped[2])
				return(extent_snapped)
			},extent=y_synced_extents,MoreArgs=list(x=x),SIMPLIFY=FALSE)
	
	border=res(x)*c(borderx,bordery)
#	border_vector=c(-border[1],border[1],-border[2],border[2])
	
	res_x=res(x)[[1]]
	res_y=res(x)[[2]]
	
	border_vector=c(-border[1]-(res_x/2),border[1]+(res_x/2),-border[2]-(res_y/2),border[2]+(res_y/2))
	
#	print(force_window_odd)
	
	mod_extent=function(extent,border,res,force_window_odd=TRUE)
	{
		modded_extent=extent(extent@xmin+border[1],extent@xmax+border[2],extent@ymin+border[3],extent@ymax+border[4])
		
#		extent@xmin=extent@xmin+border[1]
#		extent@xmax=extent@xmax+border[2]
#		extent@ymin=extent@ymin+border[3]
#		extent@ymax=extent@ymax+border[4]
		
		if(force_window_odd)
		{
			window_ncol=round((modded_extent@xmax-modded_extent@xmin)/res[[1]])
			window_nrow=round((modded_extent@ymax-modded_extent@ymin)/res[[2]])
			
			if(window_ncol %% 2==0)
			{
				modded_extent=extent(modded_extent@xmin,modded_extent@xmax+res[[1]],modded_extent@ymin,modded_extent@ymax)
#				extent@xmax=extent@xmax+res[[1]]
			}
			
			if(window_nrow %% 2==0)
			{
				modded_extent=extent(modded_extent@xmin,modded_extent@xmax,modded_extent@ymin,modded_extent@ymax+res[[2]])
#				extent@ymax=extent@ymax+res[[2]]
			}
			
		}
		
		return(modded_extent)
	}

	if(verbose)
	{
		print("Modifying the extents...")
		
	}
	
	y_synced_extents_expanded=mapply(mod_extent,extent=y_synced_extents_snapped,
			MoreArgs=list(border=border_vector,force_window_odd=force_window_odd,res=res(x)),SIMPLIFY=FALSE)
	
	if(verbose)
	{
		print("Cropping...")
		
	}
	minirasters_cropped=mapply(crop,y=y_synced_extents_expanded,MoreArgs=list(x=x),SIMPLIFY=FALSE)
	if(verbose)
	{
		print("Expanding...")
		
	}
	
#	minirasters_expanded=minirasters_cropped
	minirasters_expanded=mapply(expand,y=y_synced_extents_expanded,x=minirasters_cropped,SIMPLIFY=FALSE)
	
	if(verbose)
	{
		print("Finished crop_extents_synced!")
		
	}
	# Note, we need to eventually check for edge images and use "expand".
	return(minirasters_expanded)
}
