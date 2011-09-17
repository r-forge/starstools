\name{circ.mean.na.rm}
\alias{circ.mean.na.rm}
\title{circ...}
\usage{circ.mean.na.rm(x, unit="radians", na.rm=TRUE)}
\description{circ.mean with na.rm capability.}
\author{Jonathan A. Greenberg \email{STARStools@estarcion.net}}
\arguments{\item{x}{vector of data points measured in radians or degrees.}
\item{na.rm}{Logical. Remove NAs before calculating?}
\item{unit}{Character. Input direction unit. Default is 'radians'.  Setting to 'degrees' assumes x is in degrees, and also returns the values in degrees.}}
\examples{degree_vector_with_NA=c(359,1,45,NA)
circ.mean.na.rm(degree_vector_with_NA,na.rm=FALSE,unit='degrees')
circ.mean.na.rm(degree_vector_with_NA,na.rm=TRUE,unit='degrees')}
