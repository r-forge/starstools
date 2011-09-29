\name{rstarspan}
\alias{rstarspan}
\title{Extracts raster data at vector locations.}
\usage{rstarspan(vectors, rasters, outformat="SpatialDataFrame", ...)}
\description{Extracts raster data at vector locations.}
\note{If the input vector(s) are SpatialPolygonsDataFrame, the user must assign "fun" if outformat="SpatialDataFrame".
#'}
\author{Jonathan A. Greenberg \email{STARStools@estarcion.net}}
\arguments{\item{vectors}{a vector of superclass "Spatial" or a list of vectors.}
\item{rasters}{a raster of superclass "Raster" or a list of rasters.}
\item{outformat}{Character.  Can be "SpatialDataFrame" (appends the extracted data to the original Spatial* object) or "DataFrame" (only returns the data.frame portion).}
\item{...}{See ?extract for parameters (e.g. "fun") that can be used.}}
\examples{tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="STARStools"))
tahoe_highrez_training <- readOGR(dsn=system.file("external", package="STARStools"),layer="tahoe_highrez_training")
tahoe_training_extraction <- rstarspan(vectors=tahoe_highrez_training,rasters=tahoe_highrez,outformat="SpatialDataFrame",fun=mean)}
