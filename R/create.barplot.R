create.barplot <- function(x, data, rotate = FALSE, width = 0.9, ylab = NULL, xlab = NULL, covariate = NULL) {
  # covariate parameter is a future implementation to add a covariate bar(s) to the plot, for now, it does nothing
  
  # initialize the ggplot object
  ggplot.object <- ggplot(data = data, aes_string(x = x))
  
  # plot the data
  if (rotate == TRUE) {
    plot.object <- ggplot.object + geom_bar(width = width) + coord_flip() + xlab(xlab) + ylab(ylab)
  } else {
    plot.object <- ggplot.object + geom_bar(width = width)
    }
  return(plot.object)
  }
