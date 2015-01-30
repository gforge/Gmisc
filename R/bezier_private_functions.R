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
  if (spline_ctrl$end$length == 0){
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
    min_adjusted <- new_endpoint-(org_endpoint-new_endpoint)
    
    new_sppoint <- spl_point + 
      direction*(org_endpoint - new_endpoint)*multiplier
    
    if (direction*(min_adjusted - new_sppoint) < 0)
      new_sppoint <- min_adjusted
    
    return(new_sppoint)
  }
  spline_ctrl$x[length(spline_ctrl$x)] <-
    adjust_ctr(tail(spline_ctrl$x, 1),
               tail(bp$x, 1),
               bp$x[bp$cut_point], 
               multiplier)
  spline_ctrl$y[length(spline_ctrl$y)] <-
    adjust_ctr(tail(spline_ctrl$y, 1),
               tail(bp$y, 1),
               bp$y[bp$cut_point], 
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
      (angle_radian < pi-pi/4 && angle_radian > pi+pi/4))
    return (TRUE)
  else
    return (FALSE)
}

#' Rotates a width to match a vector
#'
#' This function is provided with a vector and a width. The
#' vector is for knowing the direction/direction and this is
#' used for generating an angle. It then calls the subfunction
#' \code{\link{rotateWidthAccAngle}}.
#'
#' @param x The x point of the vector
#' @param y The y point of the vector
#' @param x_origo The x origin if other than 0
#' @param y_origo The y origin if other than 0
#' @param default.units The \code{\link[grid]{unit}} type
#' @param ... Passed on to \code{\link{rotateWidthAccAngle}} after
#'  calculating the angle from the vector.
#'
#' @return list A list with left & right vectors of c(x, y) format
#'  and a angle element
#'
#' @keywords internal
rotateWidthAccVector <-
  function (x, y,
            x_origo=NA, y_origo=NA,
            default.units,
            ...) {
  v <- validateAndConvertVectorInputs(x=x, y=y,
                                      x_origo=x_origo, y_origo=y_origo)
  angle <- getVectorAngle(x=x, y=y,
                          x_origo=x_origo, y_origo=y_origo)

  return (rotateWidthAccAngle(angle = angle,
      x_origo = x_origo,
      y_origo = y_origo,
      default.units = default.units,
      ...))
}

#' Rotates a width to match a vector
#'
#' This function is provided with an angle and a width.
#' It then creates a vector that uses the angle and the
#' origo points to generate a width vector used in \code{\link{getLines}},
#' \code{\link{getLinesWithArrow}}
#'
#' @param angle Angle to rotate according to
#' @param x_origo The x origin if other than 0
#' @param y_origo The y origin if other than 0
#' @param width The width of the desired left & right vectors
#' @param default.units As defined by the \pkg{grid} package
#' @param perpendicular If the width is perpendicular to
#'  the provided vector the set this to true
#' @param prev_angle If you want an average from this and the
#'  previous angle then set this parameter to the previous
#'  angle
#'
#' @return list A list with left & right vectors of c(x, y) format
#'  and a angle element
#'
#' @keywords internal
rotateWidthAccAngle <- function (angle,
  x_origo=NA, y_origo=NA,
  width = 0,
  default.units,
  perpendicular = TRUE,
  prev_angle = NA) {

  if (class(x_origo) != class(y_origo))
    stop("The two origo points should be of the same type:",
         " y_origo=", class(y_origo), " x_origo=", class(x_origo))

  working_angle <- mean(c(angle, prev_angle), na.rm=TRUE)

  # The mean causes issues due to a switch at 180 degrees
  if (is.na(prev_angle) == FALSE){
    if (abs(angle) > pi/2 &&
      (angle < 0 & prev_angle > 0 ||
        angle > 0 & prev_angle < 0 ))
      working_angle <- mean(c(angle, -prev_angle), na.rm=TRUE)

  }

  rot_mtrx <- rbind(c(cos(working_angle), -sin(working_angle)),
    c(sin(working_angle), cos(working_angle)))

  # We need to convert the width into a scalar before we can work with it
  if ("unit" %in% class(width)){
    w_unit <- attr(width, "unit")
    w_scalar <- convertUnit(width, unitTo=w_unit, valueOnly=TRUE)
  }else{
    w_scalar <- width
    w_unit <- default.units
  }

  if(perpendicular)
    width_vector <- c(x=0, y=w_scalar/2)
  else
    width_vector <- c(x=w_scalar/2, y=0)

  left <- as.vector(rot_mtrx %*% width_vector)
  right <- -left

  # Convert back to units
  left <- unit(left, w_unit)
  right <- unit(right, w_unit)

  if (!is.na(x_origo)){
    if ("unit" %in% class(x_origo)){
      left <- left + unit.c(x_origo, y_origo)
      right <- right + unit.c(x_origo, y_origo)
    }else{
      left <- left + unit(c(x_origo, y_origo), default.units)
      right <- right + unit(c(x_origo, y_origo), default.units)
    }
  }

  return (list(left=left,
      right=right,
      angle=angle))
}


#' Gets the lines shifted according to width
#'
#' The lines are the upper and the lower lines that will make up the
#' future polygon that will be used to generate the arrow. These lines
#' are separated from the points by a defined width. The width is perpendicular
#' to the lines angle at that particular point.
#'
#' @param bp Bezier points that define the line. Based upon
#'  a bezier grob.
#' @param end_point A list with x & y for the end, only used for the last element's
#'  angle.
#' @param width The width as a \pkg{grid} length \code{\link[grid]{unit}}
#' @param default.units The default unit (see the \pkg{grid} package for available \code{\link[grid]{unit}}s)
#' @param align_2_axis This indicates if the arrows origin should align it to an
#'  axis. Which is decided by which axis is the closes one.
#' @return A list with left and right elements indicating the two lines
#'
#' @importFrom sp point.in.polygon
#' @keywords internal
getLines <- function(bp, end_point,
                     width, default.units,
                     align_2_axis = TRUE){
  # This initiation is necessary due to the unit inflexibility
  lr_width <- rotateWidthAccVector(x_origo=bp$x[1],
    y_origo=bp$y[1],
    x=bp$x[2],
    y=bp$y[2],
    width=width,
    perpendicular=TRUE,
    default.units=default.units)
  lines <- list(left = list(x=lr_width$left[1],
      y=lr_width$left[2]),
    right = list(x=lr_width$right[1],
      y=lr_width$right[2]))

  # Add the offset to the return variable
  addLineOffset <- function(x, y, lines, offset){
    lines$left$x <- unit.c(lines$left$x, offset$left[1])
    lines$left$y <- unit.c(lines$left$y, offset$left[2])
    lines$right$x <- unit.c(lines$right$x, offset$right[1])
    lines$right$y <- unit.c(lines$right$y, offset$right[2])
    return(lines)
  }

  is_point_in_poly <- function(point, lines){
    point.in.polygon(point.x=convertX(point[1], unitTo="mm", valueOnly=TRUE),
                     point.y=convertY(point[2], unitTo="mm", valueOnly=TRUE),
                     pol.x = convertX(unit.c(lines$right$x,
                                             lines$left$x),
                                      unitTo="mm", valueOnly=TRUE),
                     pol.y = convertX(unit.c(lines$right$y,
                                             lines$left$y),
                                      unitTo="mm", valueOnly=TRUE),
                     mode.checked=TRUE)==1
  }
  
  lines <- with(bp,
                getLineOffset(x, y, offset = offset))
    
  # For the last element use the arrow direction
  lr_width <- rotateWidthAccVector(x=end_point$x,
    y=end_point$y,
    x_origo=bp$x[i+1],
    y_origo=bp$y[i+1],
    width=width,
    perpendicular=TRUE,
    default.units = default.units)
  lines <- addLineOffset(bp$x[i+1], bp$y[i+1],
    lines, lr_width)


  # Start with the easy part, cutting off excess line
  shortenLine <- function(x, y, ref_x, ref_y, default.units, shorten_by_x = TRUE){
    if ("unit" %in% class(x))
      x <- convertX(x, unitTo=default.units, valueOnly=TRUE)
    if ("unit" %in% class(y))
      y <- convertY(y, unitTo=default.units, valueOnly=TRUE)
    if ("unit" %in% class(ref_x))
      ref_x <- convertX(ref_x, unitTo=default.units, valueOnly=TRUE)
    if ("unit" %in% class(ref_y))
      ref_y <- convertY(ref_y, unitTo=default.units, valueOnly=TRUE)

    if (shorten_by_x)
      if (x[1] < x[2])
        keep <- which(x > ref_x)[1]:length(x)
      else
        keep <- which(x < ref_x)[1]:length(x)
    else
      if (y[1] < y[2])
        keep <- which(y > ref_y)[1]:length(y)
      else
        keep <- which(y < ref_y)[1]:length(y)

    x <- unit(c(ref_x, x[keep]), default.units)
    y <- unit(c(ref_y, y[keep]), default.units)

    return(list(x=x, y=y))
  }


  # Adds the missing piece by generating another bezier curve
  # for that specific section
  extendLine <- function(x, y,
                         ref_x, ref_y,
                         default.units, extend_by_x = TRUE){
    if ("unit" %in% class(x))
      x <- convertX(x, unitTo=default.units, valueOnly=TRUE)
    if ("unit" %in% class(y))
      y <- convertY(y, unitTo=default.units, valueOnly=TRUE)
    if ("unit" %in% class(ref_x))
      ref_x <- convertX(ref_x, unitTo=default.units, valueOnly=TRUE)
    if ("unit" %in% class(ref_y))
      ref_y <- convertY(ref_y, unitTo=default.units, valueOnly=TRUE)

    distanceX <- x[1] - ref_x
    distanceY <- y[1] - ref_y

    # Generate a grob for the remaining spline
    if (extend_by_x)
      add_bg <- bezierGrob(x=c(x[1],
          x[1] + distanceX/2,
          x[1] + distanceX/2,
          ref_x),
        y=c(y[1],
          y[1],
          y[1] + distanceY/2,
          ref_y),
        default.units=default.units)
    else
      add_bg <- bezierGrob(x=c(x[1],
          x[1],
          x[1] + distanceX/2,
          ref_x),
        y=c(y[1],
          y[1] + distanceY/2,
          y[1] + distanceY/2,
          ref_y),
        default.units=default.units)

    add_bg_pt <- bezierPoints(add_bg)
    add_x <- rev(convertX(add_bg_pt$x, unitTo=default.units, valueOnly=TRUE))
    add_y <- rev(convertY(add_bg_pt$y, unitTo=default.units, valueOnly=TRUE))
    x <- unit(c(add_x, x), default.units)
    y <- unit(c(add_y, y), default.units)
    return(list(x=x, y=y))
  }

  # If vertical
  if (align_2_axis){
    angle <- getVectorAngle(x_origo=bp$x[1],
                            y_origo=bp$y[1],
                            x=bp$x[2],
                            y=bp$y[2])

    adaptLine2LeftTurn <- function(lines, org_offset, default.units, horizontal){
      # left is shorter due to the left skew
      lines$left <- shortenLine(x = lines$left$x, y = lines$left$y,
        ref_x = org_offset$left[1],
        ref_y = org_offset$left[2],
        default.units = default.units,
        shorten_by_x = horizontal)
      lines$right <- extendLine(x = lines$right$x, y = lines$right$y,
        ref_x = org_offset$right[1],
        ref_y = org_offset$right[2],
        default.units = default.units,
        extend_by_x = horizontal)
      return (lines)
    }

    adaptLine2RightTurn <- function(lines, org_offset,
                                    default.units, horizontal){
      # left is shorter due to the left skew
      lines$right <- shortenLine(x=lines$right$x, y=lines$right$y,
        ref_x = org_offset$right[1],
        ref_y = org_offset$right[2],
        default.units = default.units,
        shorten_by_x = horizontal)
      lines$left <- extendLine(x=lines$left$x, y=lines$left$y,
        ref_x = org_offset$left[1],
        ref_y = org_offset$left[2],
        default.units = default.units,
        extend_by_x = horizontal)
      return (lines)
    }

    numerical_bp <- lapply(bp, function(x) getGridVal(x, default.units))
    if (isHorizontal(angle)){
      # Get the original points of interest
      if (numerical_bp$x[2] > numerical_bp$x[1]){
        # Going right
        angle <- 0
        if (numerical_bp$y[2] < numerical_bp$y[1])
          turn <- "right"
        else
          turn <- "left"
      }else{
        angle <- pi
        if (numerical_bp$x[2] > numerical_bp$x[1])
          turn <- "right"
        else
          turn <- "left"
      }

      org_offset <- rotateWidthAccAngle(angle,
        x_origo=bp$x[1],
        y_origo=bp$y[1],
        width=width,
        default.units = default.units)

    }else{
      # Vertical
      # Get the original points of interest
      if (numerical_bp$y[2] > numerical_bp$y[1]){
        # Going up
        angle <-pi/2
        if (numerical_bp$x[2] > numerical_bp$x[1])
          turn <- "right"
        else
          turn <- "left"
      }else{
        angle <-pi*3/2
        if (numerical_bp$x[2] < numerical_bp$x[1])
          turn <- "right"
        else
          turn <- "left"
      }

      org_offset <- rotateWidthAccAngle(angle=angle,
        x_origo=bp$x[1],
        y_origo=bp$y[1],
        width=width,
        default.units = default.units)

    }

    # Check if to or right line is the shorter one
    if (turn == "left"){
      lines <- adaptLine2LeftTurn(lines = lines,
        org_offset = org_offset,
        default.units = default.units,
        horizontal = isHorizontal(angle))
    }else{
      lines <- adaptLine2RightTurn(lines = lines,
        org_offset = org_offset,
        default.units = default.units,
        horizontal = isHorizontal(angle))
    }

  }

  return (lines)

}

#' Gets the lines for an arrow
#'
#' The lines are the upper and the lower lines that are later used to generate
#' future polygon that will be used to generate the arrow. These lines
#' are separated from the points by a defined width. The width is perpendicular
#' to the lines angle at that particular point.
#'
#' @param bp Bezier points that define the line. Based upon
#'  a bezier grob.
#' @param arrow The arrow list. This should contain a x and y element to indicate
#'  the vector in relation to the 0,0 origo. The base element sets the arrow width
#'  while the length element gives the length of the arrow.
#' @param width The width as a grid length \code{\link{unit}}
#' @param end_points The end points of a line. This is a list with a start list(x, y, length)
#'  and a end list(x, y, length) as elements.
#' @param default.units The default unit (see the grid package for available units)
#' @param align_2_axis This indicates if the arrows origin should align it to an
#'  axis. Which is decided by which axis is the closes one.
#' @return A list with left and right elements indicating the two lines
#'
#' @keywords internal
getLinesWithArrow <- function(bp, arrow, end_points, width, 
                              default.units, align_2_axis){
  lines <- getLines(bp = bp,
    end_point=end_points$end,
    width=width,
    default.units=default.units,
    align_2_axis = align_2_axis)

  tmp <- rotateWidthAccVector(x=arrow$x,
    y=arrow$y,
    width=arrow$base,
    default.units = default.units)
  arrow$left <- tmp$left
  arrow$right <- tmp$right
  lines$left$x <- unit.c(lines$left$x,
#    lines$left$x[length(lines$left$x)] +
      unit(bp$x[length(bp$x)], default.units) +
        arrow$left[1],
      unit(end_points$end$x, default.units))
  lines$left$y <- unit.c(lines$left$y,
#    lines$left$y[length(lines$left$y)] +
      unit(bp$y[length(bp$y)], default.units) +
          arrow$left[2],
      unit(end_points$end$y, default.units))
  lines$right$x <- unit.c(lines$right$x,
#    lines$right$x[length(lines$right$x)] +
      unit(bp$x[length(bp$x)], default.units) +
          arrow$right[1]
  )
  lines$right$y <- unit.c(lines$right$y,
#    lines$right$y[length(lines$right$y)] +
      unit(bp$y[length(bp$y)], default.units) +
          arrow$right[2])

  return (lines)
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
