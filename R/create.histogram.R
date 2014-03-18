create.histogram <- function(x, data, xlab = NULL, ylab = NULL, binwidth = NULL, filename = NULL, resolution = NULL) {
  # initialize the plot object
  plot.object <- ggplot(data = data, aes_string(x = x))
  
  # create the plot
  if (!is.null(binwidth)) {
    plot.object <- plot.object + geom_histogram(binwidth = binwidth)    
  } else {
    plot.object <- plot.object + geom_histogram()
    }
  
  # add the x and/or y labels
  if(!is.null(xlab)) {
    plot.object <- plot.object + xlab(xlab)  
    }
  if(!is.null(ylab)) {
    plot.object <- plot.object + ylab(ylab)
    }
  
  # if the filename is present then write the histogram to a file, if not just return
  # the plot object
  if (is.null(filename)) {
    return(plot.object)
  } else {
    if (!is.null(resolution)) {
      plotting.general::write.plot(filename = filename, plot = plot.object, resolution = resolution)      
    } else {
      plotting.general::write.plot(filename = filename, plot = plot.object)  
      }
    }
  }
