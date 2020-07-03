#' @section The \code{plot}/\code{print}:
#' 
#' To output the \code{\link[grid:grid.grob]{grob}} objects to the plot either call \code{plot} 
#' on the object or \code{print} it. Note that R automatically prints any object that is outputted
#' to the console. The function calls in turn the \code{\link[grid]{grid.draw}} function on the object.
#' 
#' @param ... Passed to \code{\link[grid]{grid.draw}}
#' @rdname box
#' @export
#' @order 2
print.box <- function(x, ...) {
  grid.draw(x, ...)
}

#' @rdname box
#' @export
#' @order 3
plot.box <- print.box

#' @section S3 from the \pkg{grid} package:
#' 
#' Width and height functions address the \code{\link{coords}} attribute for the corresponding information.
#' The \code{\link[grid:widthDetails]{widthDetails}} and \code{\link[grid:widthDetails]{heightDetails}}
#' that provide information on an object.
#' 
#' @rdname box
#' @export
#' @order 4
widthDetails.box <- function(x) {
  coords(x)$width
}

#' @rdname box
#' @export
#' @order 5
heightDetails.box <- function(x) {
  coords(x)$height
}