\name{create.stepplot}
\alias{create.stepplot}
\title{Create a step plot.}
\description{
Create a step plot from a dataframe.
}
\usage{
create.stepplot(data = NULL, x = NULL, y = NULL, group = NULL, xlab = NULL, ylab = NULL, theme = NULL, filename = NULL, resolution = NULL, width = 8, height = 11)
}
\arguments{
  \item{data}{a data frame}
  \item{x}{[Str] column from the data frame identifying the independent variable}
  \item{y}{[Str] column from the data frame identifying the dependent variable}
  \item{group}{[Str] field identifying the grouping for the data}
  \item{xlab}{[Str] label for the x-axis}
  \item{ylab}{[Str] lable for the y-axis}
  \item{theme}{[Function] theme function for the plot aesthetics}
  \item{filename}{[Str] name of output file to write plot to}
  \item{resolution}{[Int] resolution of the plot, default: 1200}
  \item{width}{[Num] plot width}
  \item{height}{[Num] plot height}
}
\value{
Returns a ggplot object containing the step plot.
}
\author{
Richard de Borja <richard.deborja@sickkids.ca>
}
\examples{
create.stepplot(data=mtcars, x='qsec', y='wt')
}
\keyword{ ggplot }
\keyword{ stepplot }
