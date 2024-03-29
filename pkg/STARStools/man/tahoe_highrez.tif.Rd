\docType{data}
\name{tahoe_highrez.tif}
\alias{tahoe_highrez.tif}
\title{High resolution false color infrared image from the Lake Tahoe Basin.}
\description{
  High resolution false color infrared image from the Lake
  Tahoe Basin.
}
\examples{
tahoe_highrez <- brick(system.file("external/tahoe_highrez.tif", package="STARStools"))
plotRGB(tahoe_highrez)
}
\author{
  Jonathan A. Greenberg \email{STARStools@estarcion.net}
}
\keyword{data}
\keyword{datasets}

