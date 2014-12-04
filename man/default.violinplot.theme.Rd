\name{default.violinplot.theme}
\alias{default.violinplot.theme}
\title{
Default violin plot theme.
}
\description{
This function provides the set of default "theme" values for violin plots.  The original base theme is theme_bw().
}
\usage{
default.violinplot.theme(base_size = 30, base_family = "Helvetica")
}
\arguments{
  \item{base_size}{base size for the fonts in the violin plot}
  \item{base_family}{base family for the fonts in the violin plot}
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
\keyword{ violinplot }
\keyword{ theme }