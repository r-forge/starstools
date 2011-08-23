# TODO: Add comment
# 
# Author: jonathan
###############################################################################


extract_synced=function(x,y,...)
{
	require("raster")
	y_synced=spatial_sync_vector(y,x,verbose=FALSE)
	return(raster::extract(x,y_synced,...))
}
