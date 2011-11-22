\name{radians_to_degrees}
\alias{radians_to_degrees}
\title{Converts radians to degrees (with circular correction)...}
\usage{radians_to_degrees(radians, circular=TRUE)}
\description{Converts radians to degrees (with circular correction)}
\author{Jonathan A. Greenberg \email{STARStools@estarcion.net}}
\arguments{\item{radians}{Numeric vector of radians.}
\item{circular}{Logical. TRUE=Ouput degrees are corrected for circularity, e.g. 3*pi yields 180 degrees, not 540 degrees.}}
\examples{radians=c(0,(1/2)*pi,pi,(3/2)*pi,2*pi,3*pi)
# Convert radians to pi with circularity correction.
radians_to_degrees(radians,circular=TRUE)
# Convert radians to pi without circularity correction.
radians_to_degrees(radians,circular=FALSE)}
