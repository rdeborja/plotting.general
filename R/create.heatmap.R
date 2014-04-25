create.heatmap <- function(data, x, y, fill = NULL, xlab = NULL, ylab = NULL, filename = NULL, resolution = NULL, base.size = 20, theme = NULL, colour.scheme = NULL) {
  plot.object <- ggplot(data = data, aes_string(x = x, y = y))
  if (!is.null(fill)) {
    plot.object <- plot.object + geom_raster(aes_string(fill = fill))
  } else {
    plot.object <- plot.object + geom_raster()
    }
  
  # add custom labels to the x and y axes
  if (!is.null(xlab)) {
    plot.object <- plot.object + xlab(xlab)
    }
  if (!is.null(ylab)) {
    plot.object <- plot.object + ylab(ylab)
    }
  
  # get rid of the small padding surrounding the plot and the axes
  plot.object <- plot.object + scale_y_discrete(expand = c(0, 0))    
  plot.object <- plot.object + scale_x_discrete(expand = c(0, 0))

  # add a theme to the plot
  if (!is.null(theme)) {
    plot.object <- plot.object + theme
  } else {
    plot.object <- plot.object + plotting.general::default.heatmap.theme()
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
