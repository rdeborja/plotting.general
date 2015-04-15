default.stepplot.theme <- function(base_size=30, base_family='Helvetica', xticks=TRUE, yticks=TRUE) {
  if (xticks==FALSE) {
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
      theme(
        axis.text.x = element_text(size = rel(0.8), hjust = 1)
      )
    }
  if (yticks==FALSE) {
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
      theme(
        axis.text.x = element_text(size = rel(0.8), hjust = 1)
      )    
    }
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text.x = element_text(size = rel(0.8), hjust = 1)
    )
  }
