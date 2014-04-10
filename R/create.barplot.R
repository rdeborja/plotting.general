create.barplot <- function(data = NULL, x = NULL, y = NULL, fill = NULL, group = NULL, group.col = FALSE, group.row = FALSE, rotate = FALSE, width = 0.9, ylab = NULL, xlab = NULL, yaxis.log.scale = FALSE, xaxis.log.scale = FALSE, theme = NULL) {  
  # validate the arguments
  if (is.null(x)) stop('Missing x argument')
  if (is.null(data)) stop('Missing data argument')
  
  # initialize the ggplot object
  if (!is.null(y) & is.null(fill)) {
    plot.object <- ggplot(data = data, aes_string(x = x, y = y))
  } else if (!is.null(y) & !is.null(fill)) {
    plot.object <- ggplot(data = data, aes_string(x = x, y = y, fill = fill))
  } else {
    plot.object <- ggplot(data = data, aes_string(x = x))    
    }
  
  # if y is passed as an argument, then change the stat property to 'identity' so we plot the actual
  # y values passed to the function
  if (!is.null(y)) {
    plot.object <- plot.object + geom_bar(stat = 'identity', width = width)
    }
  else if (is.null(y)) {
    plot.object <- plot.object + geom_bar(stat = 'bin', width = width)
    }
  
  # if the group argument is not null, then add the group variable as a facet_grid parameter
  if (!is.null(group) & length(group) == 2) {
    if (group.col == TRUE & group.row == TRUE) {
      plot.object <- plot.object + facet_grid(group[1] ~ group[2])
    } else {
      stop('To group row and column wise must have a vector of 2 items')
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
  
  # plot the data
  if (rotate == TRUE) {
    plot.object <- plot.object + coord_flip()
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
  
  # modify the theme to include any modifications made at the command line (for now it's just the xaxis label angle)
  if (is.null(theme)) {
    plot.object <- plot.object + plotting.general::default.barplot.theme()   
  } else {
    plot.object <- plot.object + theme
    }
  
  return(plot.object)
  }
