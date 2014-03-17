create.histogram <- function(x, data, xlab = NULL, ylab = NULL, binwidth = 0.1, filename = NULL, resolution) {
  # initialize the plot object
  plot.object <- ggplot(data = data, aes_string(x = x));
  
  # create the plot
  plot.object <- plot.object + geom_histogram(), binwidth = binwidth);
  
  if (is.null(filename)) {
    return(plot.object);
  } else {
    if (is.null)
    plotting.general::write.plot(filename = filename, plot = plot.object)
    }
  }
