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
#' @param .subelement If a `list` of boxes is provided, this can be a name, index, a deep path (e.g. `c("detail", 1)`) or a
#'  vector of names/indices to target a single nested element to move. You can also pass
#'  multiple targets as a list of paths (e.g. `list(c("detail", 1), c("followup", 1))`).
#'  The function will return the original list with the targeted element(s) replaced by their moved version(s).
#' @return The element with updated viewport and coordinates
#'
#' @md
#' @importFrom checkmate assert_class assert checkString checkNumeric checkCharacter
#' @export
#' @example inst/examples/moveBox_ex.R
#' @importFrom checkmate assert_class assert_list
#' @family flowchart components
moveBox <- function(element,
                    x = NULL, y = NULL,
                    space = c("absolute", "relative"),
                    just = NULL,
                    .subelement = NULL) {
  space <- match.arg(space)

  to_unit <- function(u) if (is.unit(u) || is.null(u)) u else unit(u, "npc")

  if (is.list(element) && !inherits(element, "box")) {
    if (is.null(x) && is.null(y)) {
      stop("You have to specify at least x or y move parameters")
    }

    if (!is.null(.subelement)) {
      # Normalize into list of paths
      paths <- if (is.list(.subelement) && all(sapply(.subelement, is.atomic))) .subelement else list(.subelement)

      for (path in paths) {
        # If first level not found, try to unwrap the first element (consistent with other helpers)
        if (is.null(element[[path[1]]])) {
          if (length(element) > 1 &&
            is.list(element[[1]]) &&
            !inherits(element[[1]], "box") &&
            (function(el, path) {
              cur <- el
              for (p in path) {
                if (is.null(cur[[p]])) {
                  return(FALSE)
                }
                cur <- cur[[p]]
              }
              TRUE
            })(element[[1]], path)) {
            element <- element[[1]]
          } else {
            stop("The .subelement '", paste(path, collapse = "/"), "' was not found in the provided boxes.",
              if (any(names(element) %in% c("x", "y", "space", "just"))) {
                "\nDid you forget the leading '.' for your arguments, e.g. .subelement='name' instead of subelement='name'?"
              } else {
                ""
              },
              call. = FALSE
            )
          }
        }

        norm_seg <- function(s) {
          if (is.numeric(s) || (is.character(s) && grepl("^[0-9]+$", s))) {
            return(as.integer(s))
          }
          s
        }

        exists_nested <- function(el, path) {
          cur <- el
          for (p in path) {
            pp <- norm_seg(p)
            if (is.null(cur[[pp]])) {
              return(FALSE)
            }
            cur <- cur[[pp]]
          }
          TRUE
        }

        get_nested <- function(el, path) {
          cur <- el
          for (p in path) {
            cur <- cur[[norm_seg(p)]]
          }
          cur
        }

        set_nested <- function(el, path, value) {
          if (length(path) == 1) {
            el[[norm_seg(path[[1]])]] <- value
            return(el)
          }
          idx <- norm_seg(path[[1]])
          el[[idx]] <- set_nested(el[[idx]], path[-1], value)
          el
        }

        if (!exists_nested(element, path)) {
          stop("The .subelement '", paste(path, collapse = "/"), "' was not found in the provided boxes.",
            if (any(names(element) %in% c("x", "y", "space", "just"))) {
              "\nDid you forget the leading '.' for your arguments, e.g. .subelement='name' instead of subelement='name'?"
            } else {
              ""
            },
            call. = FALSE
          )
        }

        target <- get_nested(element, path)
        moved_target <- moveBox(target, x = x, y = y, space = space, just = just)
        element <- set_nested(element, path, moved_target)
      }

      return(prExtendClass(element, "Gmisc_list_of_boxes"))
    }

    if (space == "absolute") {
      # For absolute, we need to know where the group is currently to calculate relative shift
      grp_coords <- prConvert2Coords(element)

      target_x <- to_unit(x)
      target_y <- to_unit(y)

      if (!is.null(just)) {
        if (!is.null(x)) target_x <- prAdjustXPos(just, target_x, grp_coords$width)
        if (!is.null(y)) target_y <- prAdjustYPos(just, target_y, grp_coords$height)
      }

      # Use convertX/Y to ensure we can do subtraction if units differ, although usually it's npc or mm
      shift_x <- if (!is.null(target_x)) target_x - grp_coords$x else unit(0, "npc")
      shift_y <- if (!is.null(target_y)) target_y - grp_coords$y else unit(0, "npc")

      ret <- sapply(element, \(el) moveBox(el, x = shift_x, y = shift_y, space = "relative"), simplify = FALSE)
    } else {
      ret <- sapply(element, \(el) moveBox(el, x = x, y = y, space = "relative"), simplify = FALSE)
    }
    return(prExtendClass(ret, "Gmisc_list_of_boxes"))
  }

  assert_class(element, "box")
  if (is.null(x) && is.null(y)) {
    stop("You have to specify at least x or y move parameters")
  }

  vp_args <- attr(element, "viewport_data")
  assert_list(vp_args)

  if (!is.null(just)) {
    assert(
      checkCharacter(just, min.len = 1, max.len = 2),
      checkNumeric(just, lower = 0, upper = 1, min.len = 1, max.len = 2)
    )
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
        vp_args$just <- sapply(
          vp_args$just,
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
          }
        )
      }
    }
  }

  # use internal helper `to_unit()` defined above to convert numeric/nil to npc units

  x <- to_unit(x)
  y <- to_unit(y)

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
  attr(gl, "viewport_data") <- vp_args
  attr(gl, "coords") <- prCreateBoxCoordinates(
    viewport_data = vp_args,
    extra_coordinate_functions = attr(element, "extra_coordinate_functions")
  )
  return(gl)
}
