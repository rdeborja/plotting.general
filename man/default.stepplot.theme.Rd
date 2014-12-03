\name{default.stepplot.theme}
\alias{default.stepplot.theme}
\title{
Default step plot theme.
}
\description{
This function provides the set of default "theme" values for stepplots.  The original base theme is theme_bw().
}
\usage{
default.stepplot.theme(base_size = 30, base_family = "Helvetica")
}
\arguments{
  \item{base_size}{[Int] base size for the fonts in the heatmap}
  \item{base_family}{[Str] base family for the fonts in the heatmap}
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
\keyword{ stepplot }
\keyword{ theme }