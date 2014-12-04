create.violinplot <- function(data = NULL, x = NULL, y, xlab = NULL, ylab = NULL, theme = NULL, filename = NULL, resolution = 1600) {
  if (is.null(data)) stop('data is missing with no default')
  if (is.null(x)) stop('x is missing with no default')
  
  # note that "x" should already be a factor
  plot.object <- ggplot(data = data, aes_string(x = x, y = y)) + geom_violin()

  # add a theme to the plot
  if (is.null(theme)) {
    plot.object <- plot.object + plotting.general::default.violinplot.theme()
  } else {
    plot.object <- plot.object + theme
    }
  
  # add x and y axis labels if they are provided in the argument list
  if (!is.null(xlab)) {
    plot.object <- plot.object + xlab(xlab)
    }
  
  if (!is.null(ylab)) {
    plot.object <- plot.object + ylab(ylab)
    }

    if (is.null(filename)) {
        return(plot.object)
    } else {
        write.plot(filename = filename, resolution = resolution, plot = plot.object)
        }
  
  return(plot.object)
  }
