\name{create.heatmap}
\alias{create.heatmap}
\title{
Create a heatmap.
}
\description{
Create a heatmap from data within a data frame.
}
\usage{
create.heatmap(filename, data, x, y, resolution)
}
\arguments{
  \item{filename}{filename containing plot}
  \item{data}{data frame containing the data to plot}
  \item{x}{the dataframe column name containing the data to plot on the x-axis}
  \item{y}{the dataframe column name containing the data to plot on the y-axis}
  \item{resolution}{resolution of the plot}
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