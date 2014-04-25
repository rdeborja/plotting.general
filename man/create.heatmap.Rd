\name{create.heatmap}
\alias{create.heatmap}
\title{
Create a heatmap.
}
\description{
Create a heatmap from data within a data frame.
}
\usage{
create.heatmap(data, x, y, fill = NULL, xlab = NULL, ylab = NULL, filename = NULL, resolution = NULL, base.size = 20, theme = NULL, colour.scheme = NULL)
}
\arguments{
  \item{data}{data frame containing the data to plot}
  \item{x}{the dataframe column name containing the data to plot on the x-axis}
  \item{y}{the dataframe column name containing the data to plot on the y-axis}
  \item{fill}{column to use for the heatmap colours}
  \item{xlab}{label for the x-axis}
  \item{ylab}{label for the y-axis}
  \item{filename}{filename containing plot}
  \item{resolution}{resolution of the plot}
  \item{base.size}{base size for fonts on the heatmap, this is used by default.heatmap.theme (default: 20)}
  \item{theme}{theme for the plot}
  \item{colour.scheme}{A vector of two colours to use for the colourkey in the legend}
}
\value{
Returns the ggplot2 object if no filename is passed, otherwise returns the exit code for the creation of the plot.
}
\author{
Richard de Borja <richard.deborja@sickkids.ca>
}
\seealso{
  \code{\link{write.plot}}
}
\examples{
  # create a simple heatmap
  mtcars$car <- rownames(mtcars)
  create.heatmap(
    data = mtcars,
    x = 'car',
    y = 'cyl',
    xlab = '',
    ylab = 'Number of Cylinders'
    )
}
\keyword{ plotting }
\keyword{ heatmap }
\keyword{ ggplot2 }