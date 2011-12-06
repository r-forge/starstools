\name{list_to_data.frame}
\alias{list_to_data.frame}
\title{Convert a List to a Data Frame...}
\usage{list_to_data.frame(x)}
\description{Convert a List to a Data Frame}
\details{Takes a list of equal-sized objects and returns a data frame.}
\value{A data frame with listed vectors as rows.}
\author{Jonathan A. Greenberg, Alison R. Mynsberge}
\seealso{%% ~~objects to See Also as \code{\link{help}}, ~~~}
\keyword{format}
\arguments{\item{x}{a list of vectors of equal length}}
\examples{x<-c(4,1:3,NA,4)
y<-c(2:4,1,1,NA)
length(x)==length(y)
list_to_data.frame(list(x,y))}
