#' A bezier arrow with gradient
#'
#' This is an experimental addition to the original \code{\link{bezierArrowSmpl}}
#' with the addition of a gradient in the center of the arrow that fades. \strong{Note:}
#' the arrow section is not currently included in the gradient.
#'
#' @param width The width of the arrow, either a numeric single number or a unit. \strong{Note:}
#'  The arrow does not rely on lwd but on actual width. Same as in the \code{\link{bezierArrowSmpl}}
#' @param default.units A string indicating the default units to use width is given as numeric vectors.
#' @param clr The color of the arrow. This is the main color of the arrow and not the gradient color.
#' @param align_2_axis Indicates if the arrow should be vertically/horizontally aligned. This
#'  is useful for instance if the arrow attaches to a box.
#' @param grdt_type The type of growth and gradient that is to be used,
#'  currently it only supports triangle (I'm considering adding bezier curves
#'  but currently I'm a little tired of coding)
#' @param grdt_decrease_prop The proportion of the full length that should be decreasing.
#' @param grdt_start_prop The proportion of the full length that should be a constant color
#'  before decreasing.
#' @param grdt_clr_prop The proportion of the gradient that should be decreasing. This is a
#'  proportion of the grdt_decrease_prop and the grdt_start_prop combined.
#' @param grdt_line_width The width of the border line. If not specified it defaults to 5 \% of
#'  the original width, note the gradient's width is thus 90 \%.
#' @param grdt_clr The color of the gradient. It is the color that transits into the clr of the arrow.
#' @param ... Passed on to \code{\link{bezierArrowSmpl}}
#' @param gp An object of class gpar, typically the output from a call to the function gpar.
#'  This is basically a list of graphical parameter settings.
#' @param vp A Grid viewport object (or NULL).
#' @return A grob of gList() type
#'
#' @examples
#' library(grid)
#' grid.newpage()
#' arrowGrob <- bezierArrowGradient(x = c(.1,.3,.6,.9),
#'                                      y = c(0.2, 0.2, 0.9, 0.9))
#' grid.draw(arrowGrob)
#' @export
bezierArrowGradient <- function(
  width = .05,
  clr = "#000000",
  default.units = "npc",
  align_2_axis = TRUE,
  grdt_type = c("triangle"),
  grdt_decrease_prop = .4,
  grdt_start_prop = .4,
  grdt_clr_prop = .7,
  grdt_line_width = NA,
  grdt_clr = "#2F4F2F",
  vp = NULL,
  gp = gpar(),
  ...){

  # Get initial values
  grdt_type <- match.arg(grdt_type)

  if (class(width) != "unit")
    width <- unit(width, default.units)

  if (is.na(grdt_line_width))
    grdt_line_width <- getGridVal(width, default.units)*.2

  # Sanity check for input parameters
  if (grdt_decrease_prop > 1 &&
    grdt_decrease_prop < 0)
    stop("The decrease proportion must be between 0-1! You provided ", grdt_decrease_prop)

  if (grdt_start_prop > 1 &&
    grdt_start_prop < 0)
    stop("The start proportion must be between 0-1! You provided ", grdt_start_prop)

  if (grdt_clr_prop > 1 &&
    grdt_clr_prop < 0)
    stop("The color gradient proportion must be between 0-1! You provided ", grdt_clr_prop)

  if (grdt_start_prop + grdt_decrease_prop > 1)
    stop("The start and the decrease section must be less or equal to 1: ",
      grdt_start_prop, " + ", grdt_decrease_prop, " = ", grdt_start_prop + grdt_decrease_prop)

  if (getGridVal(width, default.units) < getGridVal(grdt_line_width, default.units)*2)
    stop("Your width of your arrow exceeds the width of the line, seems to be ")

  # Get the pure arrow
  pg <- bezierArrowSmpl(width = width, default.units = default.units,
    clr = clr, align_2_axis = align_2_axis,
    gp = gp, vp = vp, ...)

  if (grdt_start_prop + grdt_decrease_prop == 0){
    warning("You called the gradient function but did not",
            " specify any gradient, why not just use",
            " the bezierArrowSmpl function")
    return(pg)
  }

  # Now to the gradient
  bp <- attr(pg, "center_points")
  end_points <- attr(pg, "end_points")

  end_point <- which(cumsum(bp$lengths) >= sum(bp$lengths)*(grdt_start_prop + grdt_decrease_prop))[1]-2
  start_decrease <- which(cumsum(bp$lengths) >= sum(bp$lengths)*(grdt_start_prop))[1]

  max_gradient_width <- getGridVal(width, default.units) -
    2*getGridVal(grdt_line_width, default.units)

  clr_length <- which(cumsum(bp$lengths[end_point:1])/sum(bp$lengths[1:end_point]) >= grdt_clr_prop)[1]
  g_clrs <- colorRampPalette(colors=c(clr, grdt_clr))(clr_length)

  clipLinesEnd <- function(lines, no_elements_to_remove){
    # Make sure to keep last element in case there is a mismatch,
    # we need to preserve an attachment to the right if this is to work,
    # removing all would cause the pie-piece to become a line
    if (no_elements_to_remove >= length(lines$left$x) &&
      no_elements_to_remove < length(lines$right$x)){
      lines$left$x <- head(lines$left$x, -no_elements_to_remove)
      lines$left$y <- head(lines$left$y, -no_elements_to_remove)
      lines$right$x <- head(lines$right$x, 1)
      lines$right$y <- head(lines$right$y, 1)
    }else if (no_elements_to_remove < length(lines$left$x) &&
      no_elements_to_remove >= length(lines$right$x)){
      lines$left$x <- head(lines$left$x, 1)
      lines$left$y <- head(lines$left$y, 1)
      lines$right$x <- head(lines$right$x, -no_elements_to_remove)
      lines$right$y <- head(lines$right$y, -no_elements_to_remove)
    }else if (no_elements_to_remove >= length(lines$left$x) &&
      no_elements_to_remove >= length(lines$right$x)){
      # There are nothing left to output
      lines$left$x <- NULL
      lines$left$y <- NULL
      lines$right$x <- NULL
      lines$right$y <- NULL
    }else{
      lines$left$x <- head(lines$left$x, -no_elements_to_remove)
      lines$left$y <- head(lines$left$y, -no_elements_to_remove)
      lines$right$x <- head(lines$right$x, -no_elements_to_remove)
      lines$right$y <- head(lines$right$y, -no_elements_to_remove)
    }
    return(lines)
  }

  # Generate a list with all the elements
  inner_gradient <- gList()
  # If the end point and the start decrease points are the same
  # then there is no decrease
  if (end_point != start_decrease){
    getTriangleGrowth <- function(l){
      f <- (1-rev(cumsum(l)/sum(l)))[-1]
      return(f/max(f))
    }
    g_factor <- getTriangleGrowth(bp$lengths[start_decrease:end_point])

    base <- rotateWidthAccVector(x=bp$x[end_point],
      y=bp$y[end_point],
      x_origo=bp$x[end_point-1],
      y_origo=bp$y[end_point-1],
      width=max_gradient_width*g_factor[1],
      default.units=default.units,
      perpendicular=TRUE)

    # Draw the end of the triangle
    gradient_pg <- polygonGrob(y=unit.c(unit(bp$y[end_point], default.units),
        base$left[2],
        base$right[2]),
      x=unit.c(unit(bp$x[end_point], default.units),
        base$left[1],
        base$right[1]),
      gp=gpar(fill=g_clrs[1], col=g_clrs[1]),
      vp = vp)

    inner_gradient <- gList(inner_gradient, gradient_pg)
    for (i in (end_point-2):start_decrease){
      top <- base

      base <- rotateWidthAccVector(x=bp$x[i],
        y=bp$y[i],
        x_origo=bp$x[i-1],
        y_origo=bp$y[i-1],
        width=max_gradient_width*g_factor[end_point - i],
        perpendicular=TRUE,
        default.units=default.units,
        prev_angle=top$angle)

      current_clr <- ifelse(end_point - i < length(g_clrs),
        g_clrs[end_point - i],
        tail(g_clrs, 1))
      gradient_pg <- polygonGrob(y=unit.c(top$left[2],
          top$right[2],
          base$right[2],
          base$left[2]),
        x=unit.c(top$left[1],
          top$right[1],
          base$right[1],
          base$left[1]),
        gp=gpar(fill=current_clr,
          col=current_clr),
        vp = vp)
      inner_gradient <- gList(inner_gradient, gradient_pg)
    }
  }else {
    # In case we didn't have any decrease we still
    # need to initiate the base just so that
    # the rest will work
    if (end_point+1 > length(bp$x)){
      base <- rotateWidthAccVector(x=end_points$end$x,
        y=end_points$end$y,
        x_origo=bp$x[end_point],
        y_origo=bp$y[end_point],
        width=max_gradient_width*g_factor[1],
        default.units=default.units,
        perpendicular=TRUE)
    }else{
      base <- rotateWidthAccVector(x=bp$x[end_point + 1],
        y=bp$y[end_point + 1],
        x_origo=bp$x[end_point],
        y_origo=bp$y[end_point],
        width=max_gradient_width*g_factor[1],
        default.units=default.units,
        perpendicular=TRUE)

    }
  }

  if (start_decrease > 1){
    # Select the beginning
    if (getGridVal(bp$x[1], "mm") < getGridVal(bp$x[2], "mm")){
      x_start_selection <-
        getGridVal(bp$x[1:start_decrease], default.units) >
        getGridVal(end_points$start$x, default.units) + getGridVal(grdt_line_width, default.units)
    }else if (getGridVal(bp$x[1], "mm") > getGridVal(bp$x[2], "mm")){
      x_start_selection <-
        getGridVal(bp$x[1:start_decrease], default.units, axisTo="x") >
        getGridVal(end_points$start$x, default.units) - getGridVal(grdt_line_width, default.units)
    }

    if (getGridVal(bp$y[1], "mm", axisTo="y") < getGridVal(bp$y[2], "mm", axisTo="y")){
      y_start_selection <-
        getGridVal(bp$y[1:start_decrease], default.units) >
        getGridVal(end_points$start$y, default.units) + getGridVal(grdt_line_width, default.units)
    }else{
      y_start_selection <-
        getGridVal(bp$y[1:start_decrease], default.units) >
        getGridVal(end_points$start$y, default.units) - getGridVal(grdt_line_width, default.units)
    }

    # It can be either x or y that is closes to the starting point
    start_selection <- x_start_selection | y_start_selection

    selection <- which(start_selection)[1]:(start_decrease-1)

    # Catch the first turn
    getDirection <- function(vals){
      for (i in 2:length(vals)){
        if (vals[1] != vals[i]){
          if (vals[1] > vals[i]){
            return(-1)
          }else if (vals[1] < vals[i]){
            return(1)
          }
        }
      }
      return(0)
    }

    # Remove those that are lower/higher than the point
    removeVals <- function(vals, point){
      direction <- getDirection(vals)
      for (i in 1:length(vals)){
        if ((vals[i] - point)*direction > 0){
          return(vals[-(1:(i-1))])
        }
      }
      return(c())
    }

    angle <- getVectorAngle(x = bp$x[2],
                            y = bp$y[2],
                            x_origo = bp$x[1],
                            y_origo = bp$y[1])

    w <- getGridVal(grdt_line_width, default.units)
    st_bp <- list(start_x=getGridVal(end_points$start$x, default.units) +
                    w*cos(angle),
                  start_y=getGridVal(end_points$start$y, default.units) +
                    w*sin(angle))


    # Add the remaining points
    st_bp$add_x <- removeVals(vals=getGridVal(bp$x[selection], default.units),
                              point=st_bp$start_x)
    st_bp$add_y <- removeVals(vals=getGridVal(bp$y[selection], default.units),
                              point=st_bp$start_y)

    # The two vectors need to be the same - make the larger smaller
    if (length(st_bp$add_x) < length(st_bp$add_y)){
      st_bp$add_y <- tail(st_bp$add_y, n=length(st_bp$add_x))
    }else if (length(st_bp$add_x) > length(st_bp$add_y)){
      st_bp$add_x <- tail(st_bp$add_x, n=length(st_bp$add_y))
    }

    # Now merge into one x and y
    st_bp$x <- c(st_bp$start_x, st_bp$add_x)
    st_bp$y <- c(st_bp$start_y, st_bp$add_y)

    st_bp <- lapply(st_bp, function(x) unit(x, default.units))

    lines <- getLines(bp=st_bp,
                      end_point=list(x=bp$x[start_decrease],
                                     y=bp$y[start_decrease]),
                      width=max_gradient_width,
                      default.units=default.units,
                      align_2_axis = align_2_axis)

    if (length(g_clrs) > end_point-start_decrease){
      # Continue with gradient polygons if needed
      for (i in 2:(length(g_clrs) - (end_point-start_decrease))){
        top <- base
        base <- list(left = unit.c(tail(lines$left$x, i)[1],
                                   tail(lines$left$y, i)[1]),
                     right = unit.c(tail(lines$right$x, i)[1],
                                    tail(lines$right$y, i)[1]))

        current_clr <- g_clrs[end_point - start_decrease + i]
        gradient_pg <- polygonGrob(y=unit.c(top$left[2],
            top$right[2],
            base$right[2],
            base$left[2]),
          x=unit.c(top$left[1],
            top$right[1],
            base$right[1],
            base$left[1]),
          gp=gpar(fill=current_clr,
            col=current_clr),
          vp = vp)

        inner_gradient <- gList(inner_gradient, gradient_pg)
      }

      lines <- clipLinesEnd(lines, length(g_clrs) - (end_point-start_decrease))
    }

    if (length(lines$left$x) > 0){
      gradient_pg <- polygonGrob(y=unit.c(lines$left$y,
          base$left[2],
          base$right[2],
          rev(lines$right$y)),
        x=unit.c(lines$left$x,
          base$left[1],
          base$right[1],
          rev(lines$right$x)),
        default.units=default.units,
        gp=gpar(fill=grdt_clr, col=grdt_clr),
        vp = vp)

      inner_gradient <- gList(inner_gradient, gradient_pg)
    }
  }


  return (gList(pg, inner_gradient))
}
