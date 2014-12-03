\name{default.stripplot.theme}
\alias{default.stripplot.theme}
\title{
Default stripplot theme.
}
\description{
This function provides the set of default "theme" values for stripplots.  The original base theme is theme_bw().
}
\usage{
default.stripplot.theme(base_size = 24, base_family = "Helvetica", xaxis.angle = 0)
}
\arguments{
  \item{base_size}{[Int] base size for the fonts in the heatmap}
  \item{base_family}{[Str] base family for the fonts in the heatmap}
  \item{xaxis.angle}{[Num] angle of the tick labels on the x-axis}
}
\value{Returns a "theme" object to be used with ggplot2}
\references{
http://docs.ggplot2.org/dev/vignettes/themes.html
}
\author{
Richard de Borja <richard.deborja@sickkids.ca>
}
\seealso{
\code{\link{ggplot2}}
}
\keyword{ ggplot2 }
\keyword{ stripplot }
\keyword{ theme }
