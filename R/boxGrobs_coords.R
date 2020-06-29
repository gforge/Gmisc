#' Get the box coordinates
#'
#' Retrieves the boxes \code{"coords"} attribute.
#'
#' @param box The \code{\link{boxGrob}} or \code{\link{boxPropGrob}}
#' @return A list with the coordinates
#'
#' @importFrom checkmate assert_class
#' @family flowchart components
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