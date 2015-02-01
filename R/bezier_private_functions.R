#' Gets the bezier points adjusted for an arrow
#'
#' @param end_points The start and end points
#' @param spline_ctrl The spline control points
#' @param arrow_length The desired length of the arrow
#' @param internal.units The internal absolute unit that is used
#' @param vp The viewport if any
#' @param rez_mltpl Increases the resolution for the final bezier
#'  points by faking a larger line through multiplikation
#' @return list
#'
#' @import magrittr
getBezierAdj4Arrw <- function (end_points, spline_ctrl, arrow_length,
                               internal.units, vp, rez_mltpl = 100) {
  bp <- bezierGrob(x=c(end_points$start$x, spline_ctrl$x, end_points$end$x),
                   y=c(end_points$start$y, spline_ctrl$y, end_points$end$y),
                   default.units=internal.units, vp=vp) %>%
    bezierPoints()

  # Change to values that we can work with arithmetically
  bp$y <- convertY(bp$y, unitTo=internal.units, valueOnly=TRUE)
  bp$x <- convertX(bp$x, unitTo=internal.units, valueOnly=TRUE)

  bp$distance_from_end <-
    with(bp, sqrt((x-tail(x, 1))^2 +
                    (y-tail(y, 1))^2))

  #####################################
  # Get the best cut point and adjust #
  # so that it exactly matches the    #
  # arrow length                      #
  #####################################
  bp$cut_point <-
    (bp$distance_from_end - arrow_length) %>%
    abs %>%
    which.min

  l_diff <- arrow_length -
    sqrt((bp$x[bp$cut_point]-end_points$end$x)^2+
           (bp$y[bp$cut_point]-end_points$end$y)^2)
  if (l_diff != 0){
    if (l_diff < 0){
      # The arrow is a little short and we need to
      # shrink the distance according to the
      # length difference
      prev_el_length <-
        with(bp,
             sqrt((x[cut_point] - x[cut_point - 1])^2 +
                     (y[cut_point] - y[cut_point - 1])^2))
      rel_diff <- (1+l_diff/prev_el_length)
      bp$x[bp$cut_point] <-
        with(bp,
             x[cut_point - 1] +
               (x[cut_point] - x[cut_point - 1])*
               rel_diff)
      bp$y[bp$cut_point] <-
        with(bp,
             y[cut_point - 1] +
               (y[cut_point] - y[cut_point - 1])*
               rel_diff)
    }else{
      # Move the cut point one forward
      # but shrink the distance according to the
      # length difference
      bp$cut_point <-
        bp$cut_point + 1
      next_el_length <-
        with(bp,
             sqrt((x[cut_point] - x[cut_point - 1])^2 +
                     (y[cut_point] - y[cut_point - 1])^2))
      rel_diff <- l_diff/next_el_length
      bp$x[bp$cut_point] <-
        with(bp,
             x[cut_point - 1] +
               (x[cut_point] - x[cut_point - 1])*
               rel_diff)
      bp$y[bp$cut_point] <-
        with(bp,
             y[cut_point - 1] +
               (y[cut_point] - y[cut_point - 1])*
               rel_diff)
    }
  }


  ##############################################
  # Now we need to adjust the spline controls. #
  #                                            #
  # This is done by deducing the position of   #
  # new end, modifying the arrow spline ctrl   #
  # according to the size and direction of the #
  # current arrow.                             #
  #                                            #
  # Note that both the start and the end       #
  # spline controls need to be adjusted since  #
  # both are affected by the line shortening   #
  ##############################################

  # Special case where the end spline control isn't used
  if (length(spline_ctrl$end) == 0){
    multiplier <- 0
  }else{
    multiplier <- with(spline_ctrl$end,
                       (length-arrow_length*1.1)/length)
  }

  # Use the arrow's vector in the opposite direction as the new ctrl point
  adjust_ctr <- function(spl_point, org_endpoint,
                         new_endpoint,
                         multiplier){

    # Shorten/lengthen depending on the arrow direction
    if (new_endpoint < org_endpoint){
      direction <- 1
    }else{
      direction <- -1
    }

    # The minimum spline control is the arrow length
    min_adjusted <- new_endpoint-org_endpoint

    new_sppoint <- spl_point +
      direction*(org_endpoint - new_endpoint)*multiplier

    if (direction*(min_adjusted - new_sppoint) < 0)
      new_sppoint <- min_adjusted

    return(new_sppoint)
  }
  spline_ctrl$x[length(spline_ctrl$x)] <-
    adjust_ctr(tail(spline_ctrl$x, 1),
               tail(bp$x, 1),
               bp$x[bp$cut_point - 1],
               multiplier)
  spline_ctrl$y[length(spline_ctrl$y)] <-
    adjust_ctr(tail(spline_ctrl$y, 1),
               tail(bp$y, 1),
               bp$y[bp$cut_point - 1],
               multiplier)

  # Relate to full length
  tot_line_length <-
    with(bp,
         mapply(x1 = x[-length(x)],
                y1 = y[-length(y)],
                x2 = x[-1],
                y2 = y[-1],
                function(x1, y1, x2, y2) sqrt((x2-x1)^2 + (y2-y1)^2))) %>%
    sum

  simple_start_adjustment <- 1-arrow_length/tot_line_length/3
  # Remove a fraction of the distance for the spline controles
  spline_ctrl$x[1] <- end_points$start$x +
    (spline_ctrl$x[1]-end_points$start$x)*simple_start_adjustment
  spline_ctrl$y[1] <- end_points$start$y +
    (spline_ctrl$y[1]-end_points$start$y)*simple_start_adjustment

  ##################################
  # Done! Calculate the bezier     #
  # line that we want to follow    #
  # Note that the scale multiplier #
  # allows us to get a better      #
  # spline resolution              #
  ##################################
  new_bp <-
    bezierGrob(x=c(end_points$start$x, spline_ctrl$x,
                   bp$x[bp$cut_point])*rez_mltpl,
               y=c(end_points$start$y, spline_ctrl$y,
                   bp$y[bp$cut_point])*rez_mltpl,
               default.units=internal.units,
               vp=vp) %>%
    bezierPoints

  # Get the bezier points and scale back
  new_bp$y <- convertY(new_bp$y, unitTo=internal.units, valueOnly=TRUE)/rez_mltpl
  new_bp$x <- convertX(new_bp$x, unitTo=internal.units, valueOnly=TRUE)/rez_mltpl

  return(new_bp)
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
  x_origo=NA, y_origo=NA){
  # Just som sanity input check
  if (class(y) != class(x))
    stop("The x and y point don't have the same class,",
      " should be either numeric or units.",
      " Currently you have provided y=", class(y), " & x=", class(x))

  if (is.na(x_origo) != is.na(y_origo))
    stop("You must specify both origo points!")

  if (is.na(x) || is.na(y))
    stop("You must specify both x and y points!")

  if (is.na(x_origo)){
    if ("unit" %in% class(y))
      y_origo <- x_origo <- unit(0, attr(y, "unit"))
    else
      x_origo <- y_origo <- 0
  }

  if (class(y_origo) != class(x_origo))
    stop("The x and y point for the origo point don't have the same class,",
      " should be either numeric or units.",
      " Currently you have provided y=", class(y_origo),
      " & x=", class(x_origo))

  if (class(y) != class(y_origo))
    stop("The angle won't make any sense if your x and y point",
      " doesn't have the same unit as the origo x and y point.",
      " Currently you have provided point class=", class(y),
      " & origo class=", class(y_origo))

  return (list(y=y, x=x,
      y_origo=y_origo, x_origo=x_origo))
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
                           x_origo=NA, y_origo=NA){
  v <- validateAndConvertVectorInputs(x=x, y=y,
    x_origo=x_origo, y_origo=y_origo)

  if ("unit" %in% class(v$y)){
    y_diff <- convertY(v$y-v$y_origo, unitTo="mm", valueOnly=TRUE)
    x_diff <- convertX(v$x-v$x_origo, unitTo="mm", valueOnly=TRUE)
  }else{
    y_diff <- v$y-v$y_origo
    x_diff <- v$x-v$x_origo
  }

  return(atan2(y=y_diff, x=x_diff))
}

#' Just a simple help with interpreting the radians
#'
#' @param angle_radian The angle in radians
#' @return boolean
#'
#' @keywords internal
isHorizontal <- function(angle_radian){
  if ((angle_radian < pi/4 && angle_radian > -pi/4) ||
      (angle_radian < 2*pi-pi/4 && angle_radian > pi/2+pi/4))
    return (TRUE)
  else
    return (FALSE)
}

# Start with the easy part, cutting off excess line
shortenLine <- function(x, y,
                        ref_x, ref_y,
                        internal.units,
                        axis){
  if (axis == "y"){
    if (x[1] < x[2]){
      keep <- which(x > ref_x)[1]:length(x)
    }else{
      keep <- which(x < ref_x)[1]:length(x)
    }
  }else{
    if (y[1] < y[2]){
      keep <- which(y > ref_y)[1]:length(y)
    }else{
      keep <- which(y < ref_y)[1]:length(y)
    }
  }

  x <- c(ref_x, x[keep])
  y <- c(ref_y, y[keep])

  return(list(x=x, y=y))
}


# Adds the missing piece by generating another bezier curve
# for that specific section
extendLine <- function(x, y,
                       ref_x, ref_y,
                       internal.units,
                       axis){
  distanceX <- x[1] - ref_x
  distanceY <- y[1] - ref_y

  # Generate a grob for the remaining spline
  if (axis == "y"){
    ctrl_x <- c(x[1],
                mean(x[1], ref_x),
                mean(x[1], ref_x),
                ref_x)
    ctrl_y <- c(y[1],
                y[1],
                mean(y[1], ref_y),
                ref_y)
  }else{
    ctrl_x <- c(x[1],
                x[1],
                mean(x[1], ref_x),
                ref_x)
    ctrl_y <- c(y[1],
                mean(y[1], ref_y),
                mean(y[1], ref_y),
                ref_y)
  }

  add_bg <- bezierGrob(x = ctrl_x, y = ctrl_y,
                       default.units=internal.units)

  add_bg_pt <- bezierPoints(add_bg)
  add_x <- rev(convertX(add_bg_pt$x, unitTo=internal.units, valueOnly=TRUE))
  add_y <- rev(convertY(add_bg_pt$y, unitTo=internal.units, valueOnly=TRUE))
  x <- c(add_x, x)
  y <- c(add_y, y)
  return(list(x=x, y=y))
}

align2Axis <- function(bp, lines, width, internal.units, axis){
  if (!axis %in% c("x", "y")){
    angle <- getVectorAngle(x_origo=bp$x[1],
                            y_origo=bp$y[1],
                            x=bp$x[2],
                            y=bp$y[2])
    if (isHorizontal(angle)){
      axis <- "y"
    }else{
      axis <- "x"
    }
  }

  adaptLine_SL_ER <- function(lines,
                              org_left, org_right,
                              internal.units, axis){
    # left is shorter due to the left skew
    lines$left <- shortenLine(x = lines$left$x, y = lines$left$y,
                              ref_x = org_left[["x"]],
                              ref_y = org_left[["y"]],
                              internal.units = internal.units,
                              axis = axis)
    lines$right <- extendLine(x = lines$right$x, y = lines$right$y,
                              ref_x = org_right[["x"]],
                              ref_y = org_right[["y"]],
                              internal.units = internal.units,
                              axis = axis)
    return (lines)
  }

  adaptLine_SR_EL <- function(lines,
                                  org_left, org_right,
                                  internal.units, axis){
    # left is shorter due to the left skew
    lines$right <- shortenLine(x=lines$right$x, y=lines$right$y,
                               ref_x = org_right[["x"]],
                               ref_y = org_right[["y"]],
                               internal.units = internal.units,
                               axis = axis)
    lines$left <- extendLine(x=lines$left$x, y=lines$left$y,
                             ref_x = org_left[["x"]],
                             ref_y = org_left[["y"]],
                             internal.units = internal.units,
                             axis = axis)
    return (lines)
  }

  dy <- bp$y[2] - bp$y[1]
  dx <- bp$x[2] - bp$x[1]
  if (axis == "x"){
    # If the line is straight no changes necessary
    if (abs(dx) > .Machine$double.eps*10^2){
      if (dx > 0){
        if (dy < 0){
          org_left <- c(x = bp$x[1] + width/2, y = bp$y[1])
          org_right <- c(x = bp$x[1] - width/2, y = bp$y[1])
          lines <- adaptLine_SL_ER(lines = lines,
                                      org_left = org_left,
                                      org_right = org_right,
                                      internal.units = internal.units,
                                      axis = axis)
        }else{
          org_left <- c(x = bp$x[1] - width/2, y = bp$y[1])
          org_right <- c(x = bp$x[1] + width/2, y = bp$y[1])
          lines <- adaptLine_SR_EL(lines = lines,
                                       org_left = org_left,
                                       org_right = org_right,
                                       internal.units = internal.units,
                                       axis = axis)
        }
      }else{
        if (dy < 0){
          org_left <- c(x = bp$x[1] + width/2, y = bp$y[1])
          org_right <- c(x = bp$x[1] - width/2, y = bp$y[1])
          lines <- adaptLine_SR_EL(lines = lines,
                                   org_left = org_left,
                                   org_right = org_right,
                                   internal.units = internal.units,
                                   axis = axis)
        }else{
          org_left <- c(x = bp$x[1] - width/2, y = bp$y[1])
          org_right <- c(x = bp$x[1] + width/2, y = bp$y[1])
          # Left turn
          lines <- adaptLine_SL_ER(lines = lines,
                                       org_left = org_left,
                                       org_right = org_right,
                                       internal.units = internal.units,
                                       axis = axis)
        }
      }
    }
  }else{
    if (abs(dy) > .Machine$double.eps){
      if (dx > 0){
        if (dy < 0){
          org_left <- c(x = bp$x[1], y = bp$y[1] + width/2)
          org_right <- c(x = bp$x[1], y = bp$y[1] - width/2)
          lines <- adaptLine_SR_EL(lines = lines,
                                   org_left = org_left,
                                   org_right = org_right,
                                   internal.units = internal.units,
                                   axis = axis)
        }else{
          org_left <- c(x = bp$x[1], y = bp$y[1] + width/2)
          org_right <- c(x = bp$x[1], y = bp$y[1] - width/2)
          lines <- adaptLine_SL_ER(lines = lines,
                                   org_left = org_left,
                                   org_right = org_right,
                                   internal.units = internal.units,
                                   axis = axis)
        }
      }else{
        if (dy < 0){
          org_left <- c(x = bp$x[1], y = bp$y[1] - width/2)
          org_right <- c(x = bp$x[1], y = bp$y[1] + width/2)
          lines <- adaptLine_SL_ER(lines = lines,
                                   org_left = org_left,
                                   org_right = org_right,
                                   internal.units = internal.units,
                                   axis = axis)
        }else{
          org_left <- c(x = bp$x[1], y = bp$y[1] - width/2)
          org_right <- c(x = bp$x[1], y = bp$y[1] + width/2)
          lines <- adaptLine_SR_EL(lines = lines,
                                   org_left = org_left,
                                   org_right = org_right,
                                   internal.units = internal.units,
                                   axis = axis)
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
getGridVal <- function(x, default.units, axisTo="x"){
  if("unit" %in% class(x))
    return(convertUnit(x, unitTo=default.units, valueOnly=TRUE, axisTo=axisTo))
  else
    return(x)
}
