write.plot <- function(filename = NULL, plot, resolution = 1600) {
  ggsave(
    filename = filename,
    plot = plot,
    units = 'in',
    dpi = resolution
    );
  }