\name{raster_retile}
\alias{raster_retile}
\title{Tiles raster data}
\usage{raster_retile(raster, tilebasename, targetDir, ps=c(256, 256), ol=c(0,
    0), outformat="raster", overwrite=FALSE, progress="",
    snow_enabled=TRUE, snow_cpus=4)}
\description{Tiles raster data (with optional overlap).}
\author{Jonathan A. Greenberg \email{STARStools@estarcion.net}}
\arguments{\item{raster}{Raster*. Raster to be retiled.}
\item{tilebasename}{Character.  Output basename for each tile.}
\item{targetDir}{Character.  Folder to save tiles to.}
\item{ps}{Numeric. Pixel size of the output tile without overlap (if length(ps)==1 then the tiles are assumed to be square).}
\item{ol}{Numeric.  Overlap buffer size (in pixel units).}
\item{outformat}{Character.  The output format of each tile.}
\item{overwrite}{Logical.  Overwrite existing tiles?}
\item{snow_enabled}{Logical.  Currently unsupported.}
\item{snow_cpus}{Numeric.  Currently unsupported.}
\item{progress}{Character. Display processing progress. Valid values are "text", "window" and "" (no processing bar).}}
\examples{tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="STARStools"))
# Create 50x25 pixel tiles with 10 pixel overlap.
tahoe_highrez_tiles <- raster_retile(tahoe_highrez,ps=c(50,25),ol=10)}
