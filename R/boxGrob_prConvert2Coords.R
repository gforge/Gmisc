#' Converts an object to coordinates
#'
#' Sometimes we have an object that can be either a box,
#' a coprdinate, a unit or a numerical value and all we
#' want is a `list` of coordinates that we can use for
#' calculating distance, alignment and other things.
#'
#' @param obj A [`boxGrob`], [`boxPropGrob`], [`coords`] output, [`unit`][grid::unit] or a number ranging to
#'  be converted to a `npc` [`unit`][grid::unit]
#' @return A `list` with all the points that [`coords`] returns
#' @md
prConvert2Coords <- function(obj) {
  if (inherits(obj, "coords")) {
    return(obj)
  }

  if (inherits(obj, "box")) {
    return(coords(obj))
  }

  if (!is.unit(obj)) {
    if (!is.numeric(obj)) {
      stop("Expected a box object, coords, unit or number input, the variable '", substitute(obj), "' has value: ", obj)
    }
    obj <- unit(obj, "npc")
  }

  return(list(
    y = obj,
    x = obj,
    top = obj,
    bottom = obj,
    left = obj,
    right = obj,
    height = unit(0, "npc"),
    half_height = unit(0, "npc"),
    width = unit(0, "npc"),
    half_width = unit(0, "npc")
  ))
}
