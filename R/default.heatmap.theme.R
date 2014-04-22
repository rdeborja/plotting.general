default.heatmap.theme <- function(base_size = 24, base_family = 'Helvetica') {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x = element_text(angle = 90, size = rel(0.8), hjust = 1)
      )
  }
