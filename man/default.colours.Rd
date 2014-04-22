\name{default.colours}
\alias{default.colours}
\title{Get a vector of default colours.}
\description{Return a vector of default colours based on the RColorBrewer package.}
\usage{
default.colours(n = 2, palette = "Set1")
}
\arguments{
  \item{n}{Number of colours to return.}
  \item{palette}{RColorBrewer palette to select colours from.}
}
\value{Returns a character vector with number of colours matching the "n" argument.}
\author{
Richard de Borja <richard.deborja@sickkids.ca>
}
\seealso{
\code{\link{RColorBrewer}}
}
\examples{
  default.colours(n = 3)
}
\keyword{colour}
\keyword{default}
