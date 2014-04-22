create.stacked.barplot <- function(filename = NULL, x = NULL, fill = NULL, data, rotate = FALSE, width = 0.9, xlab = NULL, ylab = NULL, xaxis.log.scale = FALSE, yaxis.log.scale = FALSE, show.legend = TRUE, group.by = NULL, group = 'row', theme = NULL, resolution = 900) {
  if (is.null(x)) stop("Mandatory argument x is missing")
  if (is.null(fill)) stop('fill variable has not been defined')

  # initialize the ggplot object
  ggplot.object <- ggplot(data = data, aes_string(x = x, fill = fill))
  
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

  # make the x-axis start at the bottom of the plot
  if (yaxis.log.scale == TRUE) {
    plot.object <- plot.object + scale_y_log10(expand = c(0, 0))
  } else {
    plot.object <- plot.object + scale_y_continuous(expand = c(0, 0))    
  }
  if (xaxis.log.scale == TRUE) {
    plot.object <- plot.object + scale_x_log10(expand = c(0, 0))
  }
    
  # show or hide the legend (default is to show the legend which is also the default for ggplot)
  if (show.legend == FALSE) {
    plot.object <- plot.object + theme(legend.position = 'none')
    }

  # create a facet of plots based on the group variable
  if (length(group.by) == 1 & !is.null(group.by)) {
    if (!is.null(group.by)) {
      if (group == 'row') {
        facets = paste(sep = ' ', group.by, '~', '.')
        plot.object <- plot.object + facet_grid(facets = facets)
      } else if (group == 'column') {
        facets = paste(sep = ' ', group.by, '~', '.')
        plot.object <- plot.object + facet_grid(facets = facets)
        }
      }
  }
  
  # modify the theme to include any modifications made at the command line (for now it's just the xaxis label angle)
  if (is.null(theme)) {
    plot.object <- plot.object + plotting.general::default.barplot.theme()   
  } else {
    plot.object <- plot.object + theme
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
