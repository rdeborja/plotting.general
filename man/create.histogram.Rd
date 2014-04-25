\name{create.histogram}
\alias{create.histogram}
\title{
Create histogram.
}
\description{
Create a histogram using the ggplot2 framework.
}
\usage{
create.histogram(data = NULL, x = NULL, xlab = NULL, ylab = NULL, binwidth = NULL, group = NULL, group.col = FALSE, group.row = FALSE, filename = NULL, resolution = 1200, theme = NULL)
}
\arguments{
  \item{data}{data frame containing the data}
  \item{x}{data frame column name to use in histogram, must be passed as a string}
  \item{xlab}{label for the x-axis}
  \item{ylab}{label for the y-axis}
  \item{binwidth}{unit width for binning}
  \item{group}{Dataframe element to use to group bars using facet_grid()}
  \item{group.col}{Boolean to group data for facet_grid() by column}
  \item{group.row}{Boolean to group data for facet_grid() by row}
  \item{filename}{filename to save plot to, if none is provided, plot goes to STDOUT}
  \item{resolution}{the resolution of the plot to be passed to write.plot}
  \item{width}
  \item{height}
  \item{theme}{a ggplot2 theme}
}
\author{
Richard de Borja <richard.deborja@sickkids.ca>
}
\examples{
# create a simple histogram
create.histogram(
  data = mtcars,
  x = 'hp',
  binwidth = 50
  )
  
# add custom X and Y axis labels
create.histogram(
  data = mtcars,
  x = 'gear',
  binwidth = 1,
  xlab = 'Number of Gears',
  ylab = 'Frequency'
  )
}
\keyword{ plotting.general }
\keyword{ ggplot2 }
\keyword{ histogram }
