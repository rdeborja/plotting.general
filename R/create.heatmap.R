create.heatmap <- function(data, x, y, xlab = NULL, ylab = NULL, filename = NULL, resolution = NULL, base.size = 20) {
  plot.object <- ggplot(data = data, aes_string(x = x, y = y))
  plot.object <- plot.object + geom_raster(colour = 'red') + default.heatmap.theme(base_size = 24, base_family = 'Arial')

  # add custom labels to the x and y axes
  if (!is.null(xlab)) {
    plot.object <- plot.object + xlab(xlab)
    }
  if (!is.null(ylab)) {
    plot.object <- plot.object + ylab(ylab)
    }
    
  # write the plot to a file if the filename is defined
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
