#' crop_extents_synced
#' @param x TODO
#' @param y TODO
#' @param borderx x border in pixels
#' @param bordery y border in pixels
#' @param ... TODO
#' @export

crop_extents_synced=function(x,y,borderx=0,bordery=0,...)
{
	require("raster")
	# Sync the vector to the same coordinate system.
	y_synced=spatial_sync_vector(y,x,verbose=FALSE)
	
	# Extract the bounding boxes of each input
	y_synced_ids=1:length(y_synced)
	y_synced_extents=mapply(function(id,y_synced) { extent(y_synced[id,]) },id=y_synced_ids,MoreArgs=list(y_synced=y_synced),
			SIMPLIFY=FALSE)
	
	border=res(x)*c(borderx,bordery)
	border_vector=c(-border[1],border[1],-border[2],border[2])
	
	mod_extent=function(extent,border)
	{
		extent@xmin=extent@xmin+border[1]
		extent@xmax=extent@xmax+border[2]
		extent@ymin=extent@ymin+border[3]
		extent@ymax=extent@ymax+border[4]
		return(extent)
	}
	
	y_synced_extents_expanded=mapply(mod_extent,extent=y_synced_extents,MoreArgs=list(border=border_vector),SIMPLIFY=FALSE)
	minirasters=mapply(crop,y=y_synced_extents_expanded,MoreArgs=list(x=x),SIMPLIFY=FALSE)
	# Note, we need to eventually check for edge images and use "expand".
	return(minirasters)
}
