\name{create.boxplot}
\alias{create.boxplot}
\title{
Create a boxplot
}
\description{
Create a barplot from a dataframe.
}
\usage{
create.boxplot(data = NULL, x = NULL, y, xlab = NULL, ylab = NULL, filename = NULL, resolution = 1600)
}
\arguments{
  \item{data}{Data frame containing data to plot (required)}
  \item{x}{The data column inside the dataframe to plot on the x-axis.  The data should be a factor (required)}
  \item{y}{The data column inside the dataframe to plot on the y-axis}
  \item{xlab}{Label for the x-axis}
  \item{ylab}{Label for the y-axis}
  \item{filename}{filename to write plot to}
  \item{resolution}{resolution for the plot}
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
# create a simple boxplot from the mtcars dataset
mtcars$cyl <- factor(mtcars$cyl)
create.boxplot(data = mtcars, x = 'cyl', y = 'mpg', xlab = 'Number of Cylinders', ylab = 'Miles per Gallon')
}
\keyword{ boxplot }
\keyword{ ggplot2 }