\name{create.barplot}
\alias{create.barplot}
\title{Create a barplot.}
\description{Create a barplot from a dataframe based on ggplot2's geom_bar().}
\usage{
create.barplot(data = NULL, x = NULL, y = NULL, fill = NULL, group = NULL, group.col = FALSE, group.row = FALSE, rotate = FALSE, width = 0.9, ylab = NULL, xlab = NULL, yaxis.log.scale = FALSE, xaxis.log.scale = FALSE, theme = NULL)
}
\arguments{
  \item{data}{Dataframe containing data to plot}
  \item{x}{Dataframe element to plot as the x-axis}
  \item{y}{Dataframe element to plot on the y-axis}
  \item{fill}{Dataframe element to use as the colour to fill the bars}
  \item{group}{Dataframe element to use to group bars using facet_grid()}
  \item{group.col}{Boolean to group data for facet_grid() by column}
  \item{group.row}{Boolean to group data for facet_grid() by row}
  \item{rotate}{Boolean to rotate the plot}
  \item{width}{Bin width}
  \item{ylab}{Text to label the y-axis}
  \item{xlab}{Text to label the x-axis}
  \item{yaxis.log.scale}{Boolean to show yaxis in log scale}
  \item{xaxis.log.scale}{Boolean to show xaxis in log scale}
  \item{theme}{Theme function to use for the plot}
}
\value{ggplot object}
\author{
Richard de Borja <richard.deborja@sickkids.ca>
}
\examples{
create.barplot(data = mtcars, x = 'cyl')
}
\keyword{ barplot }
