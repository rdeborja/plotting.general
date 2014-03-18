create.scatterplot <- function(filename = NULL, data, x, y, regression.line = FALSE, regression.line.error = FALSE, xlab = NULL, ylab = NULL, resolution = NULL) {
  ggplot.object <- ggplot(data = data, aes_string(x = x, y = y))
  
  # add a regression line to the scatterplot with standard error (i.e. se) set to FALSE
  if (TRUE == regression.line) {
    plot.object <- ggplot.object + geom_point() + geom_smooth(method = 'lm', se = regression.line.error)
  } else if (FALSE == regression.line) {
    plot.object <- ggplot.object + geom_point() 
    }
  
  # add x and/or y axis labels
  if (!is.null(xlab)) {
    plot.object <- plot.object + xlab(xlab)
    }
  if (!is.null(ylab)) {
    plot.object <- plot.object + ylab(ylab)
    }
  
  # if a filename is provided, write the plot to a file otherwise return the plot object
  if (!is.null(filename)) {
    if (!is.null(resolution)) {
      plotting.general::write.plot(filename = filename, plot = plot.object, resolution = resolution)      
    } else {
      plotting.general::write.plot(filename = filename, plot = plot.object)      
      }
  } else {
    return(plot.object)
    }
  }