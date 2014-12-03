default.stripplot.theme <- function(base_size=24, base_family='Helvetica', xaxis.angle=0) {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x = element_text(angle = xaxis.angle, size = rel(0.8), hjust = 1)
    )
}