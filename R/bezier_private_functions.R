#' Gets the bezier points adjusted for an arrow
#'
#' @param x The x start and end points
#' @param y The spline control points
#' @param arrow_length The desired length of the arrow
#' @param length_out Increases the resolution for the final bezier
#'  points, i.e. generating more fine-grained intervals
#' @return list
#'
#' @import magrittr
#' @importFrom utils tail
getBezierAdj4Arrw <- function(x, y, arrow_length, length_out = 100) {
  org <- data.frame(x = x, y = y)
  true_bezier <- gnrlBezierPoints(cbind(x, y), length_out = length_out)
  true_bezier <- list(
    x = true_bezier[, 1],
    y = true_bezier[, 2]
  )
  true_bezier$distance <-
    with(
      true_bezier,
      sqrt((x - tail(x, 1))^2 + (y - tail(y, 1))^2)
    )
  if (missing(arrow_length) ||
    arrow_length == 0) {
    return(structure(true_bezier,
      true_bezier = true_bezier,
      cut_point = NA,
      spline_ctrl = org
    ))
  }

  # The distance between the tail and the second last spline point is indicative
  # of how strong the adjustment should be
  spl_len <- sqrt((tail(x, 1) - tail(x, 2)[1])^2 +
    (tail(y, 1) - tail(y, 2)[1])^2)
  if (spl_len < arrow_length * 3 || arrow_length == 0) {
    mult <- 1
  } else {
    mult <- spl_len / arrow_length
  }

  if (length(x) >= 4) {
    # The strength of the previous element is also of importance
    # A weak second element will cause the line to be more dependent
    # on the first element
    scnd_spl_len <- sqrt((tail(x, 3)[1] - tail(x, 4)[1])^2 +
      (tail(y, 3)[1] - tail(y, 4)[1])^2)
    if (scnd_spl_len < arrow_length * 3) {
      mult <- spl_len / (arrow_length * 2)
    }
  }

  cut_point <- which.min(abs(true_bezier$distance - arrow_length))

  dx <- tail(x, 1) - true_bezier$x[cut_point]
  dy <- tail(y, 1) - true_bezier$y[cut_point]

  # Fancy fixing of arrows fails. We therefore only to adjust last two points
  retain <- 1:(length(x) - 2)
  x <- x[retain]
  y <- y[retain]

  x <- c(x, true_bezier$x[cut_point] - dx * mult * .9)
  y <- c(y, true_bezier$y[cut_point] - dy * mult * .9)

  x <- c(x, true_bezier$x[cut_point])
  y <- c(y, true_bezier$y[cut_point])

  adjusted_bp <- gnrlBezierPoints(cbind(x, y), length_out = length_out)
  adjusted_bp <- list(
    x = adjusted_bp[, 1],
    y = adjusted_bp[, 2]
  )

  structure(adjusted_bp,
    true_bezier = true_bezier,
    cut_point = cut_point,
    spline_ctrl = list(x = x, y = y)
  )
}


#' Checks the input of a vector
#'
#' It checks that a vector makes sense in its
#' grid parameters. It also creates an origo
#' at point 0 if none is provided.
#'
#' @param x The x point of the vector
#' @param y The y point of the vector
#' @param x_origo The x origin if other than 0
#' @param y_origo The y origin if other than 0
#' @return list with the input variables checked and
#'  converted if necessary.
#'
#' @keywords internal
validateAndConvertVectorInputs <- function(x, y,
                                           x_origo, y_origo) {
  if (missing(x_origo) != missing(y_origo)) {
    stop("You must specify both origo points!")
  }

  if (missing(x) || missing(y)) {
    stop("You must specify both x and y points!")
  }

  # Just som sanity input check
  if (!identical(class(y), class(x))) {
    stop(
      "The x and y point don't have the same class,",
      " should be either numeric or units.",
      " Currently you have provided y = ", class(y), " & x = ", class(x)
    )
  }


  if (missing(x_origo)) {
    if (inherits(y, "unit")) {
      y_origo <- x_origo <- unit(0, attr(y, "unit"))
    } else {
      x_origo <- y_origo <- 0
    }
  }

  if (!setequal(class(y_origo), class(x_origo))) {
    stop(
      "The x and y point for the origo point don't have the same class,",
      " should be either numeric or units.",
      " Currently you have provided y = ", class(y_origo),
      " & x = ", class(x_origo)
    )
  }

  if (!setequal(class(y), class(y_origo))) {
    stop(
      "The angle won't make any sense if your x and y point",
      " don't have the same unit as the origo x and y point.",
      " Currently you have provided point class = ", class(y),
      " & origo class = ", class(y_origo)
    )
  }

  return(list(
    y = y,
    x = x,
    y_origo = y_origo,
    x_origo = x_origo
  ))
}

#' Translates "npc" widths into absolute units
#'
#' The "npc" \code{\link[grid]{unit}} is not absolute, i.e.
#' it depends on the relation between height and  width within
#' the current viewport. We therefore need to change these values
#' to the value most likely intended. This is believed to be the
#' main direction of the arrow as specified by the first and last
#' bezier control points \code{x} and \code{y}.
#'
#' @inheritParams bezierArrowSmpl
#' @param w The width that is to be changed
#' @return The width transformed into a \code{\link[grid]{unit}} of "mm"
#' @keywords internal
#' @importFrom utils tail
getAbsoluteWidth <- function(w, default.units, x = x, y = y) {
  if (!inherits(w, "unit")) {
    w <- unit(w, default.units)
  }

  horizontal <- (getGridVal(x[1], default.units = "mm", axisTo = "x") -
    getGridVal(tail(x, 1), default.units = "mm", axisTo = "x")) >
    (getGridVal(y[1], default.units = "mm", axisTo = "y") -
      getGridVal(tail(y, 1), default.units = "mm", axisTo = "y"))

  if (attr(w, "unit") == "npc") {
    if (!horizontal) {
      w <- unit(getGridVal(w, default.units = "mm", axisTo = "y"), "mm")
    } else {
      w <- unit(getGridVal(w, default.units = "mm", axisTo = "x"), "mm")
    }
  }
  return(w)
}

#' Gets an angle
#'
#' Uses a vector to get an angle by \code{\link{atan2}}.
#'
#' @param x The x point of the vector
#' @param y The y point of the vector
#' @param x_origo The x origin if other than 0
#' @param y_origo The y origin if other than 0
#' @return angle in radians (see \code{\link{atan2}})
#'
#' @keywords internal
getVectorAngle <- function(x, y,
                           x_origo = NA, y_origo = NA) {
  v <- validateAndConvertVectorInputs(
    x = x, y = y,
    x_origo = x_origo, y_origo = y_origo
  )

  if ("unit" %in% class(v$y)) {
    y_diff <- convertY(v$y - v$y_origo, unitTo = "mm", valueOnly = TRUE)
    x_diff <- convertX(v$x - v$x_origo, unitTo = "mm", valueOnly = TRUE)
  } else {
    y_diff <- v$y - v$y_origo
    x_diff <- v$x - v$x_origo
  }

  return(atan2(y = y_diff, x = x_diff))
}

#' Just a simple help with interpreting the radians
#'
#' @param angle_radian The angle in radians
#' @return boolean
#'
#' @keywords internal
isHorizontal <- function(angle_radian) {
  angle_prop <- abs(angle_radian / pi * 180)
  if (angle_prop < 45 || angle_prop > 135) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Start with the easy part, cutting off excess line
shortenLine <- function(x, y,
                        ref_x, ref_y,
                        internal.units,
                        axis) {
  if (axis == "y") {
    if (x[1] < x[2]) {
      keep <- which(x > ref_x)[1]:length(x)
    } else {
      keep <- which(x < ref_x)[1]:length(x)
    }
  } else {
    if (y[1] < y[2]) {
      keep <- which(y > ref_y)[1]:length(y)
    } else {
      keep <- which(y < ref_y)[1]:length(y)
    }
  }

  x <- c(rep(ref_x, keep[1] - 1 + 10), x[keep])
  y <- c(rep(ref_y, keep[1] - 1 + 10), y[keep])

  return(list(x = x, y = y))
}


# Adds the missing piece by generating another bezier curve
# for that specific section
extendLine <- function(x, y,
                       ref_x, ref_y,
                       internal.units,
                       axis) {
  distanceX <- x[1] - ref_x
  distanceY <- y[1] - ref_y

  # Generate a grob for the remaining spline
  ctrl_x <- c(x[1], ref_x, ref_x)
  ctrl_y <- c(y[1], ref_y, ref_y)

  add_bg_pt <- gnrlBezierPoints(cbind(ctrl_x, ctrl_y), length_out = 10)
  x <- c(rev(add_bg_pt[, 1]), x)
  y <- c(rev(add_bg_pt[, 2]), y)
  return(list(x = x, y = y))
}

align2Axis <- function(bp, lines, width, internal.units, axis) {
  if (!axis %in% c("x", "y")) {
    angle <- getVectorAngle(
      x_origo = bp$x[1],
      y_origo = bp$y[1],
      x = bp$x[2],
      y = bp$y[2]
    )
    if (isHorizontal(angle)) {
      axis <- "y"
    } else {
      axis <- "x"
    }
  }

  adaptLine_SL_ER <- function(lines,
                              org_left, org_right,
                              internal.units, axis) {
    # left is shorter due to the left skew
    lines$left <- shortenLine(
      x = lines$left$x, y = lines$left$y,
      ref_x = org_left[["x"]],
      ref_y = org_left[["y"]],
      internal.units = internal.units,
      axis = axis
    )
    lines$right <- extendLine(
      x = lines$right$x, y = lines$right$y,
      ref_x = org_right[["x"]],
      ref_y = org_right[["y"]],
      internal.units = internal.units,
      axis = axis
    )
    return(lines)
  }

  adaptLine_SR_EL <- function(lines,
                              org_left, org_right,
                              internal.units, axis) {
    # left is shorter due to the left skew
    lines$right <- shortenLine(
      x = lines$right$x, y = lines$right$y,
      ref_x = org_right[["x"]],
      ref_y = org_right[["y"]],
      internal.units = internal.units,
      axis = axis
    )
    lines$left <- extendLine(
      x = lines$left$x, y = lines$left$y,
      ref_x = org_left[["x"]],
      ref_y = org_left[["y"]],
      internal.units = internal.units,
      axis = axis
    )
    return(lines)
  }

  dy <- bp$y[2] - bp$y[1]
  dx <- bp$x[2] - bp$x[1]
  if (axis == "x") {
    # If the line is straight no changes necessary
    if (abs(dx) > .Machine$double.eps * 10^2) {
      if (dx > 0) {
        if (dy < 0) {
          org_left <- c(x = bp$x[1] + width / 2, y = bp$y[1])
          org_right <- c(x = bp$x[1] - width / 2, y = bp$y[1])
          lines <- adaptLine_SL_ER(
            lines = lines,
            org_left = org_left,
            org_right = org_right,
            internal.units = internal.units,
            axis = axis
          )
        } else {
          org_left <- c(x = bp$x[1] - width / 2, y = bp$y[1])
          org_right <- c(x = bp$x[1] + width / 2, y = bp$y[1])
          lines <- adaptLine_SR_EL(
            lines = lines,
            org_left = org_left,
            org_right = org_right,
            internal.units = internal.units,
            axis = axis
          )
        }
      } else {
        if (dy < 0) {
          org_left <- c(x = bp$x[1] + width / 2, y = bp$y[1])
          org_right <- c(x = bp$x[1] - width / 2, y = bp$y[1])
          lines <- adaptLine_SR_EL(
            lines = lines,
            org_left = org_left,
            org_right = org_right,
            internal.units = internal.units,
            axis = axis
          )
        } else {
          org_left <- c(x = bp$x[1] - width / 2, y = bp$y[1])
          org_right <- c(x = bp$x[1] + width / 2, y = bp$y[1])
          # Left turn
          lines <- adaptLine_SL_ER(
            lines = lines,
            org_left = org_left,
            org_right = org_right,
            internal.units = internal.units,
            axis = axis
          )
        }
      }
    }
  } else {
    if (abs(dy) > .Machine$double.eps) {
      if (dx > 0) {
        if (dy < 0) {
          org_left <- c(x = bp$x[1], y = bp$y[1] + width / 2)
          org_right <- c(x = bp$x[1], y = bp$y[1] - width / 2)
          lines <- adaptLine_SR_EL(
            lines = lines,
            org_left = org_left,
            org_right = org_right,
            internal.units = internal.units,
            axis = axis
          )
        } else {
          org_left <- c(x = bp$x[1], y = bp$y[1] + width / 2)
          org_right <- c(x = bp$x[1], y = bp$y[1] - width / 2)
          lines <- adaptLine_SL_ER(
            lines = lines,
            org_left = org_left,
            org_right = org_right,
            internal.units = internal.units,
            axis = axis
          )
        }
      } else {
        if (dy < 0) {
          org_left <- c(x = bp$x[1], y = bp$y[1] - width / 2)
          org_right <- c(x = bp$x[1], y = bp$y[1] + width / 2)
          lines <- adaptLine_SL_ER(
            lines = lines,
            org_left = org_left,
            org_right = org_right,
            internal.units = internal.units,
            axis = axis
          )
        } else {
          org_left <- c(x = bp$x[1], y = bp$y[1] - width / 2)
          org_right <- c(x = bp$x[1], y = bp$y[1] + width / 2)
          lines <- adaptLine_SR_EL(
            lines = lines,
            org_left = org_left,
            org_right = org_right,
            internal.units = internal.units,
            axis = axis
          )
        }
      }
    }
  }

  attr(lines, "axis") <- axis
  return(lines)
}
#' Gets grid value
#'
#' Returns the raw value in units if the provided is of type unit else
#' it returns the default unit.
#'
#' @param x Value
#' @param default.units The unit type
#' @param axisTo The axis that is used, useful for "npc" where
#'  there is a big difference in height and width
#' @return float
#'
#' @keywords internal
getGridVal <- function(x, default.units, axisTo = "x") {
  if ("unit" %in% class(x)) {
    return(convertUnit(x, unitTo = default.units, valueOnly = TRUE, axisTo = axisTo))
  } else {
    return(x)
  }
}