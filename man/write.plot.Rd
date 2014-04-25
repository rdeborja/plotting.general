\name{write.plot}
\alias{write.plot}
\title{
Write the plot object to a file.
}
\description{
Write the plot object to a file with publication ready parameters.  This is an internal function and
is not exported to the user.
}
\usage{
write.plot(filename = NULL, plot = NULL, size.units = 'in', width = 8, height = 8, resolution = 1600)
}
\arguments{
  \item{filename}{The filename to write the plot to}
  \item{plot}{The ggplot object to output}
  \item{size.units}{Plots size units (default: in)}
  \item{width}{Width of plot (default: 8)}
  \item{height}{Height of plot (default: 8)}
  \item{resolution}{Resolution of output image in dpi (default: 1600)}
}
\author{
Author: Richard de Borja <richard.deborja@sickkids.ca>
Maintainer: Richard de Borja <richard.deborja@sickkids.ca>
}
\keyword{ write.plot }
\keyword{ plotting.general }
\keyword{ ggplot }
\keyword{ ggsave }
