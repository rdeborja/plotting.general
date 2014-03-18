\name{create.scatterplot}
\alias{create.scatterplot}
\title{Create a scatterplot}
\description{Create a scatterplot given a dataframe using two columns}
\usage{
create.scatterplot(filename = NULL, data, x, y, regression.line = FALSE, regression.line.error = FALSE, xlab = NULL, ylab = NULL, resolution = NULL)
}
\arguments{
  \item{filename}{Filename to write plot to (default: return plot object)}
  \item{data}{Dataframe}
  \item{x}{Item defined for the x-axis}
  \item{y}{Item defined for the y-axis}
  \item{regression.line}{Add a regression line using the lm function (default: FALSE)}
  \item{regression.line.error}{Add the regression line standard error (default: FALSE)}
  \item{xlab}{label for the x-axis}
  \item{ylab}{label for the y-axis}
  \item{resolution}{the resolution of the plot to be passed to write.plot}
}
\author{
Author:     Richard de Borja <richard.deborja@sickkids.ca>
Maintainer: Richard de Borja <richard.deborja@sickkids.ca>
}
\examples{
# simple scatterplot
create.scatterplot(data = mtcars, x = 'cyl', y = 'mpg')

# add x and y axis labels
create.scatterplot(data = mtcars, x = 'cyl', y = 'mpg', xlab = 'Number of Cylinders', ylab = 'Miles/Gallon [mpg]')

# add a regression line
create.scatterplot(data = mtcars, x = 'cyl', y = 'mpg', regression.line = TRUE)
}
\keyword{ scatterplot }
\keyword{ create.scatterplot }
\keyword{ ggplot2 }
