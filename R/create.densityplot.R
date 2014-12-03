create.densityplot <- function(data = NULL, x = NULL, xlab = NULL, ylab = NULL, group = NULL, group.col = FALSE, group.row = FALSE, filename = NULL, resolution = 1200, width = 8, height = 8, theme = NULL) {
  if (is.null(data)) stop("Mandatory argument data is missing")
  
  # initialize the plot object
  plot.object <- ggplot(data = data, aes_string(x = x))
  
  # create the plot
  plot.object <- plot.object + geom_density()    
  
  # if the group argument is not null, then add the group variable as a facet_grid parameter
  if (!is.null(group) & length(group) == 2) {
    if (group.col == TRUE & group.row == TRUE) {
      plot.object <- plot.object + facet_grid(group[1] ~ group[2])
    } else {
      stop('To group row-wise and column-wise must have a vector of 2 fields')
    }
  }
  else if (!is.null(group) & length(group) == 1) {
    if (group.col == TRUE) {
      plot.object <- plot.object + facet_grid(paste(sep = ' ', '. ~', group), scales = 'free', space = 'free')
    } else if (group.row == TRUE) {
      plot.object <- plot.object + facet_grid(paste(sep = ' ', group, '~ .'))
    } else {
      plot.object <- plot.object + facet_grid(paste(sep = ' ', group, '~ .'))
    }
  }
  
  # add the x and/or y labels
  if(!is.null(xlab)) {
    plot.object <- plot.object + xlab(xlab)  
  }
  if(!is.null(ylab)) {
    plot.object <- plot.object + ylab(ylab)
  }
  
  # add a theme, if none is provided as a passed argument, use the default theme
  if (is.null(theme)) {
    plot.object <- plot.object + default.histogram.theme()
  }
  
  # if the filename is present then write the histogram to a file, if not just return
  # the plot object
  if (is.null(filename)) {
    return(plot.object)
  } else {
    ggplot2::ggsave(filename = filename, plot = plot.object, units = 'in', width = width, height = height, dpi = resolution)
  }
}
