\name{create.histogram}
\alias{create.histogram}
\title{
Create histogram.
}
\description{
Create a histogram using the ggplot2 framework.
}
\usage{
create.histogram(x, data, xlab, ylab, binwidth, filename)
}
\arguments{
  \item{x}{data frame column name to use in histogram, must be passed as a string}
  \item{data}{data frame containing the data}
  \item{xlab}{label for the x-axis}
  \item{ylab}{label for the y-axis}
  \item{binwidth}{unit width for binning}
  \item{filename}{filename to save plot to, if none is provided, plot goes to STDOUT}
}
\author{
Richard de Borja <richard.deborja@sickkids.ca>
}
\examples{
}
\keyword{ plotting.general }
\keyword{ ggplot2 }
\keyword{ histogram }
