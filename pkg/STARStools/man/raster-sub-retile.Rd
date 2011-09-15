\name{raster_retile}
\alias{raster_retile}
\title{Tiles raster data}
\usage{raster_retile(raster, tilebasename, targetDir, ps=c(256, 256), ol=c(0, 0), outformat="raster",
    overwrite=FALSE, progress="")}
\description{Tiles raster data (with optional overlap).}
\author{Jonathan A. Greenberg \email{STARStools@estarcion.net}}
\arguments{\item{raster}{Input raster, brick or stack.}
\item{tilebasename}{Character. Output basename for tiles.}
\item{targetDir}{Character. Output directory for the tiles.  If this is missing, the files are assumed to be temporary.}
\item{ps}{Numeric or vector. Tile size in pixel units.  Can be a single value or c(x tile size,y tile size).}
\item{ol}{Numeric or vector. Overlap size in pixel units.  Can be a single value or c(x overlap size,y overlap size).}
\item{outformat}{Character. Output format of the tiles.  Use gdalDrivers() (part of rgdal) for the format codes.}
\item{overwrite}{Logical. Should existing files be overwritten?}
\item{progress}{Character. Display processing progress. Valid values are "text", "window" and "" (no processing bar).}}
\examples{tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="STARStools"))
# Create 50x25 pixel tiles with 10 pixel overlap.
tahoe_highrez_tiles <- raster_retile(tahoe_highrez,ps=c(50,25),ol=10)}
