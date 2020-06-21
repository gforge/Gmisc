#' Get the box coordinates
#'
#' Retrieves the boxes \code{"coords"} attribute.
#'
#' @param box The boxGrob
#' @return A list with the cooordinates
#'
#' @importFrom checkmate assert_class assert checkString checkNumeric
#' @export
#' @examples
#' box <- boxGrob("A test box")
#' coords(box)
coords <- function(box) {
  # Check if not already a coordinate element
  if (inherits(box, "coords")) {
    return(box)
  }
  assert_class(box, "box")
  attr(box, "coords")
}