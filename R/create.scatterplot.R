create.scatterplot <- function(data, x, y, regression.line = FALSE) {
  ggplot.object <- ggplot(data = data, aes_string(x = x, y = y))
  if (TRUE == regression.line) {
    plot.object <- ggplot.object + geom_point() + geom_smooth(method = 'lm', )
  } else if (FALSE == regression.line) {
    plot.object <- ggplot.object + geom_point() 
    }
  return(plot.object)
  }