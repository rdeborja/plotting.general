\name{create.multiplot}
\alias{create.multiplot}
\title{Align multiple plots}
\description{
Align multiple plots in a single arrangement.  For now it supports two plots but will expand.
}
\usage{
create.multiplot(plotA = NULL, plotB = NULL)
}
\arguments{
  \item{plotA}{first ggplot object}
  \item{plotB}{second ggplot object}
}
\value{
Returns a grid object with the multiple plot layout.
}
\author{
Richard de Borja <richard.deborja@sickkids.ca>
}
\examples{
set.seed(1234)
plotA <- plotting.general::create.scatterplot(data=mtcars, x='disp', y='qsec', regression.line=TRUE, regression.line.error=TRUE)
plotB <- plotting.general::create.scatterplot(data=mtcars, x='disp', y='wt', regression.line=TRUE, regression.line.error=TRUE)
create.multiplot(plotA=plotA, plotB=plotB)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ multiplot }
\keyword{ ggplot }
