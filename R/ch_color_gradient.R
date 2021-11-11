#' ch_color_gradient
#'
#'
#' @param x  array of variable
#' @param colors an array of colours to form gradient



ch_color_gradient <- function(x, colors=c("darkred", "red","white","green", "darkgreen"), colsteps = 100) {
  if(is.null(x)) stop("x is NULL")
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out = colsteps)) ] )
}
