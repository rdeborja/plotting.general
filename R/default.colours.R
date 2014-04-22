default.colours <- function(n = 2, palette = 'Set1') {
  if (n == 1) return ('#8DD3C7')
  if (n == 2) return (c('#8DD3C7', '#FFFFB3'))
  if (n > 12) stop("Too many number of colours, will need to select manually")
  if (n > length(palette)) palette = 'Set3'
  colour.vector <- brewer.pal(n = n, name = palette)
  return(colour.vector[1:n])
  }
