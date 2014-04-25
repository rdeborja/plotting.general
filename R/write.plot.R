write.plot <- function(filename = NULL, plot = NULL, size.units = 'in', width = 8, height = 8, resolution = 1600) {
  if(is.null(plot)) stop("Mandatory argument plot is missing")
  ggsave(
    filename = filename,
    plot = plot,
    units = size.units,
    width = width,
    height = height,
    dpi = resolution
    );
  }
