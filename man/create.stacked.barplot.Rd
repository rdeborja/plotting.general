\name{create.stacked.barplot}
\alias{create.stacked.barplot}
\title{
Create a stacked barplot.
}
\description{
Create a barplot with a fill variable to show a stacked barplot.
}
\usage{
create.stacked.barplot(x, fill = NULL, data, rotate = FALSE, width = 0.9, xlab = NULL, ylab = NULL, show.legend = TRUE, group.by = NULL, group = 'row')
}
\arguments{
  \item{x}{The value for the x-axis (required)}
  \item{fill}{The value to fill the bars (required)}
  \item{data}{The dataframe containing the data for the plot (required)}
  \item{rotate}{A boolean variable to rotate the barplot (default: FALSE)}
  \item{width}{Defines the width of the bars (default: 0.9)}
  \item{xlab}{The label for the x-axis (default: NULL)}
  \item{ylab}{The label for the y-axis (default: NULL)}
  \item{show.legend}{A boolean to determine whether to show or hide the legend (default: TRUE)}
  \item{group.by}{Dataframe element to use in facet_grid() to group bars}
  \item{group}{Group by 'row' or 'col'}
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
