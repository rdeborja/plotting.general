\name{create.violinplot}
\alias{create.violinplot}
\title{
Create a violin plot.
}
\description{
Create a violin plot from a dataframe.
}
\usage{
create.violinplot(data = NULL, x = NULL, y, xlab = NULL, ylab = NULL, theme = NULL, filename = NULL, resolution = 1600)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{dataframe to process}
  \item{x}{Data column representing the x-axis}
  \item{y}{Data column representing the y-axis}
  \item{xlab}{Label for the x-axis}
  \item{ylab}{Label for the y-axis}
  \item{theme}{A theme function for the violin plot (default: defeault.violinplot.theme)}
  \item{filename}{Filename to output plot to}
  \item{resolution}{resolution of the plot image}
}
\references{
http://ggplot2.org
}
\author{
Richard de Borja <richard.deborja@gmail.com>
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)
create.violinplot(data=mtcars, x='cyl', y='wt', xlab='# Cylinders', ylab='Weight')
}
\keyword{ violingplot }
