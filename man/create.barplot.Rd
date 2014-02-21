\name{create.barplot}
\alias{create.barplot}
\title{Create a barplot}
\description{Create a barplot given a dataframe.}
\usage{
create.barplot(x, data, rotate = FALSE, width = 0.9, ylab = NULL, xlab = NULL, covariate = NULL)
}
\arguments{
  \item{x}{String containing name of dataframe element to process}
  \item{data}{Dataframe}
  \item{rotate}{Flag to determine whether to rotate barplot}
  \item{width}{Width of the bar}
  \item{ylab}{Label for the y-axis}
  \item{xlab}{Label for the x-axis}
  \item{covariate}{Covariate bar, future feature}
}
\author{
Author: Richard de Borja <richard.deborja@sickkids.ca>
Maintainer: Richard de Borja <richard.deborja@sickkids.ca>
}
\examples{
create.barplot(data = mtcars, x = 'cyl', width = 1)
}
\keyword{ barplot }
\keyword{ create.barplot }
\keyword{ ggplot2 }
