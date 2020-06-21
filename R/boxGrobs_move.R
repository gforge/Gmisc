#' Move a boxGrob
#'
#' Moves a [`boxGrob`]/[`boxPropGrob`] by modifying it's [`viewport`][grid::viewport].
#' This can be useful if you want to create a series of boxes whose position are relative
#' to each other and depend on each box's width/height.
#'
#' @param element A [`boxGrob`]/[`boxPropGrob`] object.
#' @param x A [`unit`][grid::unit] element or a numeric that can be converted to a `npc` unit object.
#' @param y A [`unit`][grid::unit] element or a numeric that can be converted to a `npc` unit object.
#' @param space We can provide `absolute` that confers the box absolute position within the parent 
#'  [`viewport`][grid::viewport]. If `relative` the movement is related to the current position. 
#' @return The element with updated 
#'
#' @md
#' @importFrom checkmate assert_class assert checkString checkNumeric
#' @export
#' @example inst/examples/moveBox_ex.R
#' @importFrom checkmate assert_class assert_list
moveBox <- function(element, x = NULL, y = NULL, space = c('absolute', 'relative')) {
  space <- match.arg(space)
  assert_class(element, "box")
  if (is.null(x) && is.null(y)) {
    stop('You have to specify at least x or y move parameters')
  }
  
  vp_args <- attr(element, 'viewport_data')
  assert_list(vp_args)
  
  toUnit <- function(x) {
    if (is.unit(x) || is.null(x)) {
      return(x)
    }
    
    unit(x, units = "npc")
  }

  x <- toUnit(x)
  y <- toUnit(y)
  
  if (space == "relative") {
    if (!is.null(x)) {
      x <- vp_args$x + x
    }
    if (!is.null(y)) {
      y <- vp_args$y + y
    }
  }

  if (!is.null(x)) {
    vp_args$x <- x
  }

  if (!is.null(y)) {
    vp_args$y <- y
  }
  
  gl <- editGrob(element, vp = do.call(viewport, vp_args))
  attr(gl, 'viewport_data') <- vp_args
  attr(gl, 'coords') <- prCreateBoxCoordinates(viewport_data = vp_args,
                                               extra_coordinate_functions = attr(element, 'extra_coordinate_functions'))
  return(gl)
}
