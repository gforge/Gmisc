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
#' @param just The justification of an argument as used by [`viewport`][grid::viewport] some tiny differences:
#'  (1) you only want to change the justification in the vertical direction you can retain the
#'  existing justification by using `NA`, e.g. `c(NA, 'top')`, (2) if you specify only one string
#'  and that string is either `top` or `bottom` it will assume vertical justification.
#' @return The element with updated
#'
#' @md
#' @importFrom checkmate assert_class assert checkString checkNumeric checkCharacter
#' @export
#' @example inst/examples/moveBox_ex.R
#' @importFrom checkmate assert_class assert_list
#' @family flowchart components
moveBox <- function(element,
                    x = NULL, y = NULL,
                    space = c('absolute', 'relative'),
                    just = NULL) {
  space <- match.arg(space)
  assert_class(element, "box")
  if (is.null(x) && is.null(y)) {
    stop('You have to specify at least x or y move parameters')
  }

  vp_args <- attr(element, 'viewport_data')
  assert_list(vp_args)

  if (!is.null(just)) {
    assert(checkCharacter(just, min.len = 1, max.len = 2),
           checkNumeric(just, lower = 0, upper = 1, min.len = 1, max.len = 2))
    if (is.null(vp_args$just)) {
      vp_args$just <- just
    } else {
      if (length(just) == 1) {
        if (just %in% c("bottom", "top")) {
          vp_args$just[2] <- just
        } else {
          vp_args$just[1] <- just
        }
      } else {
        for (i in 1:2) {
          if (!is.na(just[i])) {
            vp_args$just[i] <- just[i]
          }
        }
      }
      if (!all(vp_args$just %in% c("center", "centre", "left", "right", "bottom", "top"))) {
        vp_args$just <- sapply(vp_args$just,
                               function(x) {
                                 if (x %in% c("center", "centre")) {
                                   return(0.5)
                                 }
                                 if (x %in% c("left", "bottom")) {
                                   return(0)
                                 }
                                 if (x %in% c("right", "top")) {
                                   return(1)
                                 }
                                 if (!is.na(as.numeric(x))) {
                                   return(as.numeric(x))
                                 }
                                 stop("The justification ", x, " has not been implemented")
                               })
      }
    }
  }

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
