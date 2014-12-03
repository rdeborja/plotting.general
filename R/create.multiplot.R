create.multiplot <- function(plotA=NULL, plotB=NULL) {
  if (is.null(plotA)) stop("Mandatory argument plotA is missing")
  if (is.null(plotB)) stop("Mandatory argument plotB is missing")
  
  # setup the ggplots as grobs
  grobA <- ggplotGrob(plotA)
  grobB <- ggplotGrob(plotB)
  maxWidth <- grid::unit.pmax(grobA$widths[2:5], grobB$widths[2:5])
  grobA$widths[2:5] <- as.list(maxWidth)
  grobB$widths[2:5] <- as.list(maxWidth)
  plot.object <- arrangeGrob(grobA, grobB)
  return(plot.object)
  }