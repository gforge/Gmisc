#' Get the box coordinates
#'
#' Retrieves the boxes \code{"coords"} attribute.
#'
#' @param box The \code{\link{boxGrob}} or \code{\link{boxPropGrob}}
#' @return A list with grid unit coordinates. Standard boxes include:
#' \describe{
#'   \item{\code{x}, \code{y}}{The box center.}
#'   \item{\code{left}, \code{right}}{The horizontal edges.}
#'   \item{\code{top}, \code{bottom}}{The vertical edges.}
#'   \item{\code{width}, \code{height}}{The full box dimensions.}
#'   \item{\code{half_width}, \code{half_height}}{Half of the box dimensions.}
#' }
#' Split boxes such as \code{\link{boxPropGrob}} may add extra coordinates,
#' for example \code{left_x}, \code{right_x}, and \code{prop_x}.
#'
#' @importFrom checkmate assert_class
#' @family flowchart components
#' @export
#' @examples
#' box <- boxGrob("A test box")
#' coords(box)
#'
#' # Extract a single position as a unit
#' position(box, position = "left", type = "x")
coords <- function(box) {
  # Check if not already a coordinate element
  if (inherits(box, "coords")) {
    return(box)
  }

  assert_class(box, "box")
  attr(box, "coords")
}
