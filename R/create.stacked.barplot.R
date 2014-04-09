create.stacked.barplot <- function(x, fill = NULL, data, rotate = FALSE, width = 0.9, xlab = NULL, ylab = NULL, show.legend = TRUE, group.by = NULL, group = 'row') {
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
  
  return(plot.object)
  }
