#' Gets the height for an x-axis object
#' 
#' A function that gets the height of an axisGrob. It is for some
#' reason not included by default in the \code{grid}-package.
#' 
#' @param x The \code{\link[grid]{xaxisGrob}} object
#' @return A unit object
#' 
#' @author Max
#' @examples
#' grid.newpage()
#' xg <- xaxisGrob(c(1:3))
#' convertY(grobHeight(xg), "lines")
#' @export
heightDetails.xaxis <- function(x) {
  cex <- prGetTextGrobCex(x$children$labels)
  
  grobHeight(x$children$ticks) + 
    unit(1.5*cex, "line")
}