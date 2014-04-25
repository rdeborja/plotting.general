\name{create.stacked.barplot}
\alias{create.stacked.barplot}
\title{
Create a stacked barplot.
}
\description{
Create a barplot with a fill variable to show a stacked barplot.
}
\usage{
create.stacked.barplot(filename = NULL, x = NULL, fill = NULL, data, rotate = FALSE, width = 0.9, xlab = NULL, ylab = NULL, xaxis.log.scale = FALSE, yaxis.log.scale = FALSE, show.legend = TRUE, group.by = NULL, group = 'row', theme = NULL, resolution = 900)
}
\arguments{
  \item{filename}{Name of file to write plot to, if NULL is provided the plots displays to STDOUT (default: NULL)}
  \item{x}{The value for the x-axis (required)}
  \item{fill}{The value to fill the bars (required)}
  \item{data}{The dataframe containing the data for the plot (required)}
  \item{rotate}{A boolean variable to rotate the barplot (default: FALSE)}
  \item{width}{Defines the width of the bars (default: 0.9)}
  \item{xlab}{The label for the x-axis (default: NULL)}
  \item{ylab}{The label for the y-axis (default: NULL)}
  \item{xaxis.log.scale}{BOOLEAN to set xaxis to log scale (default: FALSE)}
  \item{yaxis.log.scale}{BOOLEAN to set yaxis to log scale (default: FALSE)}
  \item{show.legend}{A boolean to determine whether to show or hide the legend (default: TRUE)}
  \item{group.by}{Dataframe element to use in facet_grid() to group bars (default: NULL)}
  \item{group}{Group by 'row' or 'col', (default: row)}
  \item{theme}{Theme function for plot (default: NULL)}
  \item{resolution}{Resolution of plot (default: 900)}
}
\value{
Returns a ggplot object.
}
\author{
Richard de Borja <richard.deborja@sickkids.ca>
}
\examples{
create.stacked.barplot(data = diamonds, x = 'clarity', width = 0.9, fill = 'cut', xlab = 'Diamond Clarity', ylab = 'Count', show.legend = FALSE)
}
\keyword{ plotting.general }
\keyword{ ggplot2 }
\keyword{ stacked }
\keyword{ barplot }
