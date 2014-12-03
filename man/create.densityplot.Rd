\name{create.densityplot}
\alias{create.densityplot}
\title{Create a density plot.}
\description{
A wrapper function for creating density plots using ggplot.
}
\usage{
create.densityplot(data = NULL, x = NULL, xlab = NULL, ylab = NULL, group = NULL, group.col = FALSE, group.row = FALSE, filename = NULL, resolution = 1200, width = 8, height = 8, theme = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{data frame containing data to plot}
  \item{x}{[Str] data frame column name to use in histogram, must be passed as a string}
  \item{xlab}{[Str] label for the x-axis}
  \item{ylab}{[Str] label for the y-axis}
  \item{group}{Dataframe element to use to group bars using facet_grid()}
  \item{group.col}{Boolean to group data for facet_grid() by column}
  \item{group.row}{Boolean to group data for facet_grid() by row}
  \item{filename}{[Str] filename to save plot to, if none is provided, plot goes to STDOUT}
  \item{resolution}{[Int] the resolution of the plot to be passed to write.plot}
  \item{width}{Num] plot width}
  \item{height}{[Num] plot height}
  \item{theme}{[Function] a ggplot2 theme function}
}
\value{
Returns a ggplot plot object.
}
\author{
Richard de Borja <richard.deborja@sickkids.ca>
}
\examples{
create.densityplot(data=mtcars, x='mpg', xlab='Miles Per Gallon', y='Density')
}
\keyword{ ggplot }
\keyword{ density }
