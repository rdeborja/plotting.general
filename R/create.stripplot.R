create.stripplot <- function(data, x, y, rotate = FALSE, xlab = NULL, ylab = NULL, axis.label.font.size = 30, axis.tick.font.size = 10) {
  plot.object <- ggplot(data = data);
  plot.object <- plot.object + geom_point(aes_string(x = x, y = y), shape = 1)
  if (rotate == TRUE) {
    plot.object <- plot.object + coord_flip()
    }
  if (!is.null(xlab)) {
    plot.object <- plot.object + xlab(xlab)
    }
  if (!is.null(ylab)) {
    plot.object <- plot.object + ylab(ylab)
    }
  plot.object <- plot.object + 
    theme(
      axis.title = element_text(size = axis.label.font.size),
      axis.text = element_text(size = axis.tick.font.size)
      )

  return(plot.object);
  }
