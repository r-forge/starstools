\name{rstarspan}
\alias{rstarspan}
\title{Extracts raster data at vector locations.}
\usage{
  rstarspan(vectors, rasters,
    outformat = "SpatialDataFrame", borderx = 0,
    bordery = 0, force_window_odd = TRUE,
    miniraster_direction = "vertical",
    miniraster_vector = TRUE, verbose = TRUE, ...)
}
\arguments{
  \item{vectors}{a vector of superclass "Spatial" or a list
  of vectors.}

  \item{rasters}{a raster of superclass "Raster" or a list
  of rasters.}

  \item{outformat}{Character.  Can be "SpatialDataFrame"
  (appends the extracted data to the original Spatial*
  object) or "DataFrame" (only returns the data.frame
  portion).}

  \item{...}{See ?extract for parameters (e.g. "fun") that
  can be used.}
}
\description{
  Extracts raster data at vector locations.
}
\note{
  If the input vector(s) are SpatialPolygonsDataFrame, the
  user must assign "fun" if outformat="SpatialDataFrame".
  #'
}
\examples{
tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="STARStools"))
tahoe_highrez_training_polygons <- readOGR(dsn=system.file("external", package="STARStools"),layer="tahoe_highrez_training")
tahoe_training_extraction <- rstarspan(vectors=tahoe_highrez_training,rasters=tahoe_highrez,outformat="SpatialDataFrame",fun=mean)
tahoe_highrez_training_points <- readOGR(dsn=system.file("external", package="STARStools"),layer="tahoe_highrez_training_points")
tahoe_training_extraction_minirasterstrip <- rstarspan(vectors=tahoe_highrez_training_points,
rasters=tahoe_highrez,outformat="MiniRasterStrip",borderx=5,bordery=5)
}
\author{
  Jonathan A. Greenberg \email{STARStools@estarcion.net}
}

