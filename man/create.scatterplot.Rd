\name{create.scatterplot}
\alias{create.scatterplot}
\title{Create a scatterplot}
\description{Create a scatterplot given a dataframe using two columns}
\usage{
create.scatterplot(data, x, y, regression.line)
}
\arguments{
  \item{data}{Dataframe}
  \item{x}{Item defined for the x-axis}
  \item{y}{Item defined for the y-axis}
  \item{regression.line}{logical; add a regression line using the lm function, default FALSE}
}
\author{
Author:     Richard de Borja <richard.deborja@sickkids.ca>
Maintainer: Richard de Borja <richard.deborja@sickkids.ca>
}
\examples{
create.scatterplot(data = mtcars, x = 'cyl', y = 'mpg')
}
\keyword{ scatterplot }
\keyword{ create.scatterplot }
\keyword{ ggplot2 }
