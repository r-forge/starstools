\name{plot_azimuth}
\alias{plot_azimuth}
\title{Plots an azimuth raster using arrows.}
\usage{plot_azimuth(azimuth_raster, basemap_raster, magnitude_raster, max_arrows_per_dim=25, arrowfun=arrows,
    arrow.ex, length=0.05, magnitude_stetch=c(0.2, 0.8), azimuth_raster_aggregate_method="ngb",
    magnitude_raster_aggregate_method, na.rm=TRUE)}
\description{Plots an azimuth raster using arrows.}
\author{Jonathan A. Greenberg \email{STARStools@estarcion.net}}
\seealso{\code{\link{arrow.plot}\link{arrows}}}
\arguments{\item{azimuth_raster}{RasterLayer. A raster with azimuth values in units of radians.}
\item{basemap_raster}{RasterLayer. The underlying image (defaults to azimuth_raster).}
\item{magnitude_raster}{RasterLayer. An optional raster which is used to scale the arrow lengths.}
\item{max_arrows_per_dim}{Numeric. The maximum number of arrows to display in the longest dimension.}
\item{arrowfun}{The arrow function to use (see ?arrow.plot).}
\item{arrow.ex}{Numeric. Controls the length of the arrows (see ?arrow.plot).  In general, don't set this as the length will be chosen based on the arrow spacing.}
\item{length}{Numeric. Length of the edges of the arrow head (in inches).}
\item{magnitude_stetch}{Ignore.  Not currently working.}
\item{azimuth_raster_aggregate_method}{Character. Function used when resampling the azimuth raster. Defaults to "ngb" (nearest neighbor), but can be "pa" (pixel aggregate).}
\item{magnitude_raster_aggregate_method}{Character. Function used when resampling the magnitude raster. Defaults to "ngb" (nearest neighbor), but can be "pa" (pixel aggregate).}
\item{na.rm}{Logical. Remove NAs when aggregate_method="pa"?}}
\examples{tahoe_lidar_bareearth <- raster(system.file("external/tahoe_lidar_bareearth.tif", package="STARStools"))
tahoe_lidar_bareearth_azimuth <- slopeAspect(tahoe_lidar_bareearth,out=c('aspect'),unit='radians')
plot_azimuth(azimuth_raster=tahoe_lidar_bareearth_azimuth,basemap_raster=tahoe_lidar_bareearth,magnitude_raster=tahoe_lidar_bareearth,max_arrows_per_dim=25)}
