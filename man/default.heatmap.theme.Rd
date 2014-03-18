\name{default.heatmap.theme}
\alias{default.heatmap.theme}
\title{
Default heatmap theme.
}
\description{
This function provides the set of default "theme" values for heatmaps.  The original base theme is theme_bw().
}
\usage{
default.heatmap.theme(base_size = 24, base_family = "Helvetica", angle = 90)
}
\arguments{
  \item{base_size}{base size for the fonts in the heatmap}
  \item{base_family}{base family for the fonts in the heatmap}
  \item{angle}{x-axis label angle}
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
\keyword{ heatmap }
\keyword{ theme }