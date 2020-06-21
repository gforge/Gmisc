#' @rdname box
#' @export
widthDetails.box <- function(x) {
  attr(x, "coords")$width
}