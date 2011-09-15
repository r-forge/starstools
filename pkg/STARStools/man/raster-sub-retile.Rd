\name{raster_retile}
\alias{raster_retile}
\title{Tiles raster data
Tiles raster data (with optional overlap).}
\usage{raster_retile(raster, tilebasename, targetDir, ps=c(256, 256), ol=c(0, 0), outformat="raster",
    overwrite=FALSE, progress="")}
\author{Jonathan A. Greenberg \email{STARStools@estarcion.net}}
\arguments{\item{raster}{Input raster, brick or stack.}
\item{tilebasename}{Character. Output basename for tiles.}
\item{targetDir}{Character. Output directory for the tiles.}
\item{ps}{Numeric or vector. Tile size in pixel units.  Can be a single value or c(x tile size,y tile size).}
\item{ol}{Numeric or vector. Overlap size in pixel units.  Can be a single value or c(x overlap size,y overlap size).}
\item{outformat}{Character. Output format of the tiles.  Use gdalDrivers() (part of rgdal) for the format codes.}
\item{overwrite}{Logical. Should existing files be overwritten?}
\item{progress:}{Character. Display processing progress. Valid values are "text", "window" and "" (no processing bar).}}
