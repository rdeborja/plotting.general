write.plot <- function(filename = NULL, plot, device, path = NULL, scale = 1, plot.width = 6, plot.height = 6, plot.units = 'in', resolution = 1600) {
  
  # setup a few variables
  old.bitmaptype <- getOptions('bitmapType')
  options(bitmapType = 'cairo')
  
  if (!is.null(filename)) {
    ggsave(
      filename = filename,
      plot = plot,
      device = device,
      path = path,
      scale = scale,
      width = plot.width,
      height = plot.height,
      units = plot.units,
      dpi = resolution
      )
    options(bitmapType = old.bitmapType)  
    }
  }
