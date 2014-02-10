create.scatterplot <- function(data, x, y) {
  ggplot.object <- ggplot(data = data, aes_string(x = x, y = y))
  ggplot.object + geom_point()
  }
