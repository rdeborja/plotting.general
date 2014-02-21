create.barplot <- function(x, data, rotate = FALSE, width = 0.9, ylab = NULL, xlab = NULL, covariate = NULL) {
  # covariate parameter is a future implementation to add a covariate bar(s) to the plot, for now, it does nothing
  
  # initialize the ggplot object
  ggplot.object <- ggplot(data = data, aes_string(x = x))
  
  # plot the data
  if (rotate == TRUE) {
    plot.object <- ggplot.object + geom_bar(width = width) + coord_flip()
  } else {
    plot.object <- ggplot.object + geom_bar(width = width)
    }

  # add custom labels to the x and y axes
  if (!is.null(xlab)) {
    plot.object <- plot.object + xlab(xlab)
    }
  if (!is.null(ylab)) {
    plot.object <- plot.object + ylab(ylab)
    }
  
  return(plot.object)
  }
