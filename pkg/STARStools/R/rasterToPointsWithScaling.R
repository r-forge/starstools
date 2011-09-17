#' Converts a raster to points, with optional rescaling (combines aggregate and rasterToPoints from package "raster").
#' @param x	 A Raster* object
#' @param fact	 Integer. Aggregation factor expressed as number of cells in each direction (horizontally and vertically). Or two integers (horizontal and vertical aggregation factor). Default is 2. 
#' @param fun	 Function used to aggregate values (default=mean).
#' @param expand	 Logical. If TRUE the output RasterLayer will be larger then the input RasterLayer if a division of the number of columns or rows with factor is not an integer.
#' @param na.rm	 logical. If TRUE, NA cells are removed from calculations.
#' @param filename	 Character. Output filename.
#' @param subset_fun	Function to select a subset of raster values.
#' @param spatial	Logical. If TRUE, the function returns a SpatialPointsDataFrame object.
#' @param ... Additional arguments.
#' @author Jonathan A. Greenberg \email{STARStools@@estarcion.net} and Robert Hijimans
#' @example 
#' tahoe_lidar_bareearth <- raster(system.file("external/tahoe_lidar_bareearth.tif", package="STARStools"))
#' tahoe_lidar_bareearth_points=rasterToPointsWithScaling(tahoe_lidar_bareearth,fact=8,spatial=TRUE)
#' spplot(tahoe_lidar_bareearth_points)
#' @seealso \code{\link{rasterToPoints}} 
#' @export

rasterToPointsWithScaling = function(x,fact=2, fun=mean,expand=TRUE, na.rm=TRUE,
		filename="", subset_fun=NULL,spatial=FALSE,...)
{

	x_agg = aggregate(x=x, fact=fact, fun=fun, expand=expand, na.rm=na.rm, filename=filename, ... )
	x_pts = rasterToPoints(x_agg, fun=subset_fun, spatial=spatial, ...)
	return(x_pts)
}