#' Coerces two rasters to have a shared subset of their common overlap.
#' @title extract_overlap_rasters
#' @param raster1 TODO
#' @param raster2 TODO
#' @name extract_overlap_rasters
#' @author Jonathan A. Greenberg \email{STARStools@@estarcion.net}
#' @examples
#' TODO
#' @export


extract_overlap_rasters <- function(raster1,raster2)
{
	extent1=bbox(raster1)
	extent2=bbox(raster2)
	
	overlap_extent=extent1
	for(i in 1:2)
	{
	overlap_extent[i]=max(c(extent1[i],extent2[i]))
	}
	for(i in 3:4)
	{
		overlap_extent[i]=min(c(extent1[i],extent2[i]))
	}
	
	raster1_overlap=crop(raster1,overlap_extent)
	raster2_overlap=crop(raster2,overlap_extent)
	
	return(list(raster1_overlap,raster2_overlap))
}

