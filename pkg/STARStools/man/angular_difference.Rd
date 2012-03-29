\name{angular_difference}
\alias{angular_difference}
\title{angular_difference}
\usage{
  angular_difference(angle1, angle2, na.rm = TRUE,
    units = "degrees")
}
\arguments{
  \item{angle1}{Numeric. First angle(s).}

  \item{angle2}{Numeric. Second angle(s).}

  \item{units}{Character. The units of the input (and
  output).  Can be "degrees" (default) or "radians".}

  \item{na.rm}{Logical. Unused.  Available for
  compatibility with raster.}
}
\description{
  Calculates the positive angular difference between two
  angles.
}
\examples{
angle1=c(45,90,355,10)
angle2=c(45,95,5,345)
angular_difference(angle1,angle2)
}
\author{
  Jonathan A. Greenberg \email{STARStools@estarcion.net}
}

