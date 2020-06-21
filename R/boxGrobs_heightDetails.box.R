#' @rdname box
#' @export
heightDetails.box <- function(x) {
  attr(x, "coords")$height
}