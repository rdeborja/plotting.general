\name{create.stripplot}
\alias{create.stripplot}
\title{
Create a strip plot.
}
\description{
Create a strip plot based on a dataframe and two dataframe elements.
}
\usage{
create.stripplot(data, x, y, rotate = FALSE, xlab = NULL, ylab = NULL, axis.label.font.size = 30, axis.tick.font.size = 10)
}
\arguments{
  \item{data}{Dataframe containing data to be analyzed}
  \item{x}{Data for x-axis values}
  \item{y}{Data for y-axis values}
  \item{rotate}{Rotate plot (default = FALSE)}
  \item{xlab}{X-axis label}
  \item{ylab}{Y-axis label}
  \item{axis.label.font.size}{Font size for the x-axis and y-axis labels (default = 30)}
  \item{axis.tick.font.size}{Font size for the x-axis and y-axis tick labels (default = 10)}
}
\author{
Author: Richard de Borja <richard.deborja@sickkids.ca>
Maintainer: Richard de Borja <richard.deborja@sickkids.ca>
}
\examples{
create.stripplot(
  data = quakes,
  x = 'depth',
  y = 'mag',
  rotate = TRUE,
  xlab = 'Depth',
  ylab = 'Magnitude'
  );
}
\keyword{ stripplot }
\keyword{ plotting.general }
\keyword{ ggplot }
\keyword{ geom_point }
