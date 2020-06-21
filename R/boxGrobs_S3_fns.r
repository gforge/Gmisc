#' The print/plot calls the \code{\link[grid]{grid.draw}} function on the object
#' @param ... Passed to \code{\link[grid]{grid.draw}}
#' @rdname box
#' @export
print.box <- function(x, ...) {
  grid.draw(x, ...)
}

#' @rdname box
#' @export
plot.box <- print.box