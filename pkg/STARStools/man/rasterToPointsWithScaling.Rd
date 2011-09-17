\name{rasterToPointsWithScaling}
\alias{rasterToPointsWithScaling}
\title{Converts a raster to points, with optional rescaling (combines aggregate and rasterToPoints from package "raster").}
\usage{rasterToPointsWithScaling(x, fact=2, fun=mean, expand=TRUE, na.rm=TRUE, filename="", subset_fun,
    spatial=FALSE, ...)}
\description{Converts a raster to points, with optional rescaling (combines aggregate and rasterToPoints from package "raster").}
\author{Jonathan A. Greenberg \email{STARStools@estarcion.net} and Robert Hijimans}
\seealso{\code{\link{rasterToPoints}}}
\arguments{\item{x}{A Raster* object}
\item{fact}{Integer. Aggregation factor expressed as number of cells in each direction (horizontally and vertically). Or two integers (horizontal and vertical aggregation factor). Default is 2.}
\item{fun}{Function used to aggregate values (default=mean).}
\item{expand}{Logical. If TRUE the output RasterLayer will be larger then the input RasterLayer if a division of the number of columns or rows with factor is not an integer.}
\item{na.rm}{logical. If TRUE, NA cells are removed from calculations.}
\item{filename}{Character. Output filename.}
\item{subset_fun}{Function to select a subset of raster values.}
\item{spatial}{Logical. If TRUE, the function returns a SpatialPointsDataFrame object.}
\item{...}{Additional arguments.}}
\examples{tahoe_lidar_bareearth <- raster(system.file("external/tahoe_lidar_bareearth.tif", package="STARStools"))
tahoe_lidar_bareearth_points=rasterToPointsWithScaling(tahoe_lidar_bareearth,fact=8,spatial=TRUE)
spplot(tahoe_lidar_bareearth_points)}
