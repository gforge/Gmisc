#' Gets the height for an x-axis object
#'
#' A function that gets the height of an \code{\link[grid]{xaxisGrob}}.
#' It is for some reason not included by default in the \pkg{grid}-package.
#'
#' @param x The \code{\link[grid]{xaxisGrob}} object
#' @return A unit object
#'
#' @examples
#' grid.newpage()
#' xg <- xaxisGrob(c(1:3))
#' convertY(grobHeight(xg), "lines")
#' @export
#' @keywords internal
heightDetails.xaxis <- function(x) {
  cex <- prGetTextGrobCex(x$children$labels)

  grobHeight(x$children$ticks) +
    unit(1.5*cex, "line")
}