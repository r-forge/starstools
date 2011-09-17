#' Plots an azimuth raster using arrows.
#' @param azimuth_raster RasterLayer. A raster with azimuth values in units of radians.
#' @param basemap_raster RasterLayer. The underlying image (defaults to azimuth_raster).
#' @param magnitude_raster RasterLayer. An optional raster which is used to scale the arrow lengths.
#' @param max_arrows_per_dim Numeric. The maximum number of arrows to display in the longest dimension.
#' @param arrowfun	The arrow function to use (see ?arrow.plot). 
#' @param arrow.ex Numeric. Controls the length of the arrows (see ?arrow.plot).  In general, don't set this as the length will be chosen based on the arrow spacing.
#' @param length Numeric. Length of the edges of the arrow head (in inches).
#' @param magnitude_stetch Ignore.  Not currently working.
#' @param azimuth_raster_aggregate_method Character. Function used when resampling the azimuth raster. Defaults to "ngb" (nearest neighbor), but can be "pa" (pixel aggregate).
#' @param magnitude_raster_aggregate_method Character. Function used when resampling the magnitude raster. Defaults to "ngb" (nearest neighbor), but can be "pa" (pixel aggregate).
#' @param na.rm Logical. Remove NAs when aggregate_method="pa"?
#' @examples 
#' tahoe_lidar_bareearth <- raster(system.file("external/tahoe_lidar_bareearth.tif", package="STARStools"))
#' tahoe_lidar_bareearth_azimuth <- slopeAspect(tahoe_lidar_bareearth,out=c('aspect'),unit='radians')
#' plot_azimuth(azimuth_raster=tahoe_lidar_bareearth_azimuth,basemap_raster=tahoe_lidar_bareearth,magnitude_raster=tahoe_lidar_bareearth,max_arrows_per_dim=25)
#' @author Jonathan A. Greenberg \email{STARStools@@estarcion.net}
#' @seealso \code{\link{arrow.plot}\link{arrows}} 
#' @export


plot_azimuth <- function(azimuth_raster,basemap_raster,magnitude_raster,
		max_arrows_per_dim=25,arrowfun=arrows,arrow.ex,length=.05,magnitude_stetch=c(.2,.8),
		azimuth_raster_aggregate_method="ngb",magnitude_raster_aggregate_method,na.rm=TRUE)
{
	if(missing(basemap_raster))
	{
		basemap_raster=azimuth_raster
	}
	# Check the dimensions of the image.
	ncol_azimuth_raster=ncol(azimuth_raster)
	nrow_azimuth_raster=nrow(azimuth_raster)
	
	col_scale=ncol_azimuth_raster/max_arrows_per_dim
	row_scale=nrow_azimuth_raster/max_arrows_per_dim
	
	fact=max(col_scale,row_scale)
	
	# These needs to be fixed, since this really slows this down.
	if(azimuth_raster_aggregate_method=="ngb")
	{
		azimuth_raster_extent=projectExtent(azimuth_raster,projection(azimuth_raster))
		new_res=res(azimuth_raster)*fact
		res(azimuth_raster_extent)=new_res
		azimuth_raster_agg=raster::resample(azimuth_raster, azimuth_raster_extent,method="ngb")
	} else
	{
		azimuth_raster_agg=aggregate(x=azimuth_raster,fact=fact,fun=circ.mean.na.rm)
	}
	
	azimuth_raster_agg_points=rasterToPoints(azimuth_raster_agg,spatial=FALSE)
	
	if(missing(magnitude_raster))
	{
		azimuth_raster_agg_points_u=sin(azimuth_raster_agg_points[,3])
		azimuth_raster_agg_points_v=cos(azimuth_raster_agg_points[,3])
	} else
	{
		if(azimuth_raster_aggregate_method=="ngb")
		{
			magnitude_raster_extent=projectExtent(magnitude_raster,projection(magnitude_raster))
			new_res=res(magnitude_raster)*fact
			res(magnitude_raster_extent)=new_res
			magnitude_raster_agg=raster::resample(magnitude_raster, magnitude_raster_extent,method="ngb")
		} else
		{
			magnitude_raster_agg=aggregate(x=magnitude_raster,fact=fact,fun=mean)
		}
		
		magnitude=setMinMax(magnitude_raster_agg)
		min_magnitude=minValue(magnitude_raster_agg)
		max_magnitude=maxValue(magnitude_raster_agg)
		magnitude_raster_agg_points=rasterToPoints(magnitude_raster_agg,spatial=FALSE)
		magnitude_raster_agg_points_normalized=(magnitude_stetch[2]-magnitude_stetch[1])*(
				(magnitude_raster_agg_points[,3]-min_magnitude)/(max_magnitude-min_magnitude))+magnitude_stetch[1]
		azimuth_raster_agg_points_u=sin(azimuth_raster_agg_points[,3])*(magnitude_raster_agg_points_normalized)
		azimuth_raster_agg_points_v=cos(azimuth_raster_agg_points[,3])*(magnitude_raster_agg_points_normalized)	
	}
	
	if(missing(arrow.ex))
	{
		arrow.ex=(1/min(nrow(azimuth_raster_agg),ncol(azimuth_raster_agg)))
	}
	
	plot(basemap_raster)
	arrow.plot(azimuth_raster_agg_points[,1],azimuth_raster_agg_points[,2],
			u=azimuth_raster_agg_points_u,
			v=azimuth_raster_agg_points_v,
			arrowfun=arrowfun,arrow.ex=arrow.ex,length=length)
}