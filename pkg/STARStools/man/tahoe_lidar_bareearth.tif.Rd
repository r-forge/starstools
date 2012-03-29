\docType{data}
\name{tahoe_lidar_bareearth.tif}
\alias{tahoe_lidar_bareearth.tif}
\title{Lidar-derived bare earth digital elevation model from the Lake Tahoe Basin.}
\description{
  Lidar-derived bare earth digital elevation model from the
  Lake Tahoe Basin.

  Lidar-derived bare earth digital elevation model from the
  Lake Tahoe Basin.
}
\examples{
tahoe_highrez_training_polygons <- readOGR(dsn=system.file("external", package="STARStools"),layer="tahoe_highrez_training")
spplot(tahoe_highrez_training_polygons,zcol="Class")
tahoe_highrez_training_points <- readOGR(dsn=system.file("external", package="STARStools"),layer="tahoe_highrez_training_points")
spplot(tahoe_highrez_training_points,zcol="SPECIES")
tahoe_lidar_bareearth <- raster(system.file("external/tahoe_lidar_bareearth.tif", package="STARStools"))
plot(tahoe_lidar_bareearth)
}
\author{
  Jonathan A. Greenberg \email{STARStools@estarcion.net}

  Jonathan A. Greenberg \email{STARStools@estarcion.net}
}
\keyword{data}
\keyword{datasets}

