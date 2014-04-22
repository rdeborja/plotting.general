\name{create.boxplot.from.summary}
\alias{create.boxplot.from.summary}
\title{
Create a boxplot from summarized data.
}
\description{
Create a boxplot from a dataframe that provides summary statistics.
}
\usage{
create.boxplot.from.summary(data = NULL, x = NULL, min, max, upper, lower, middle, xlab = NULL, ylab = NULL, theme = NULL, filename = NULL, resolution = 1600)
}
\arguments{
  \item{data}{Dataframe containing data to plot}
  \item{x}{The dat column inside the dataframe to plot on the x-axis.}
  \item{min}{Minimum value for the yaxis, this will be plotted as the minimum whisker value}
  \item{max}{Maximum value for the yaxis, this will be plotted as the maximum whisker value}
  \item{upper}{Third quartile value}
  \item{lower}{First quartile value}
  \item{middle}{Median value}
  \item{xlab}{x-axis label}
  \item{ylab}{y-axis label}
  \item{theme}{Theme function}
  \item{filename}{Filename for the plot, if left blank the plot will be shown on STDOUT}
  \item{resolution}{resolution for the plot}
}
\author{
Richard de Borja <richard.deborja@sickkids.ca>
}
\examples{
abc <- adply(matrix(rnorm(100), ncol = 5), 2, quantile, c(0, .25, .5, .75, 1))
colnames(abc) <- c('sample', 'min', 'first_quartile', 'median', 'third_quartile', 'max')
create.boxplot.from.summary(data = abc, x = 'sample', min = 'min', lower = 'first_quartile', middle = 'median', upper = 'third_quartile', max = 'max')
}
\keyword{ boxplot }
