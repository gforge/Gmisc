#' A simple bezier arrow
#'
#' This is an alternative to the grid packages \code{\link[grid]{bezierGrob}}
#' with the advantage that it allows you to draw an arrow with a specific
#' unit width. Note, it has only a end-arrow at this point.
#'
#' @inheritParams grid::bezierGrob
#' @param width The width of the arrow, either a numeric single number or a unit. \strong{Note:}
#'  The arrow does not rely on lwd but on actual width.
#' @param clr The color of the arrow.
#' @param arrow This is a list with all the \strong{base} (width) and the desired 
#' \strong{length} for the arrow. \strong{Note:} This differs from the original 
#' \code{\link{bezierGrob}} function.
#' @param align_2_axis Indicates if the arrow should be vertically/horizontally 
#'  aligned. This is useful for instance if the arrow attaches to a box.
#' @param name A character identifier.
#' @return \code{grid::grob} A grob of the class polygonGrob with attributes that 
#'  correspond to the bezier points.
#'
#' @examples
#' library(grid)
#' grid.newpage()
#' arrowGrob <- bezierArrowSmpl(x = c(.1,.3,.6,.9),
#'                              y = c(0.2, 0.2, 0.9, 0.9))
#' grid.draw(arrowGrob)
#'
#' @import grid
#' @export
bezierArrowSmpl <- function(x = c(0.2, .7, .3, .9),
                            y = c(0.2, .2, .9, .9),
                            width = .05,
                            clr = "#000000",
                            default.units = "npc",
                            arrow = list(base = unit(.1, "snpc"),
                                         length = unit(.1, "snpc")),
                            align_2_axis = TRUE,
                            name = NULL,
                            gp = gpar(), vp = NULL){
  if (class(x) != "unit")
    x <- unit(x, default.units)
  if (class(y) != "unit")
    y <- unit(y, default.units)
  if (class(arrow$base) != "unit")
    arrow$base <- unit(arrow$base, default.units)
  if (class(arrow$length) != "unit")
    arrow$length <- unit(arrow$length, default.units)
  if (class(width) != "unit")
    width <- unit(width, default.units)

  if (length(y) != length(x))
    stop("You have provided unequal lengths to y and x - thus uninterpretable:",
         " y=", length(y), " elements",
         " while x=", length(x), " elements")

  ###############################################################################
  # Internally we want to avoid using the "npc" as this is not an absolute      #
  # measure and we therefore switch to "mm" that is consistent among the axes.  #
  # This compromises the portability of the grob but it is a price worth paying #      
  # Note: All values are numeric beyone this point!                             #
  ###############################################################################
  internal.units <- "mm"
  x <- convertX(x, unitTo=internal.units, valueOnly=TRUE)
  y <- convertY(y, unitTo=internal.units, valueOnly=TRUE)
  width <- convertY(width, unitTo=internal.units, valueOnly=TRUE)
  arrow$length <- convertX(arrow$length, unitTo = internal.units, valueOnly = TRUE)
  arrow$base <- convertX(arrow$base, unitTo = internal.units, valueOnly = TRUE)
  

  # According to the original description they're all spline
  # control points but as I want the line to start and end
  # at specific points then this makes sense to me
  end_points <- list(start=list(x=x[1],
                                y=y[1]),
                     end=list(x=tail(x, 1),
                              y=tail(y, 1)))

  spline_ctrl <- list(x=x[2:(length(x)-1)],
                      y=y[2:(length(y)-1)])

  # Get the length of the spline control through sqrt(a^2+b^2)
  spline_ctrl$start$length <- 
    sqrt((spline_ctrl$x[1] - end_points$start$x)^2+
           (spline_ctrl$y[1] - end_points$start$y)^2)
  spline_ctrl$end$length <- 
    sqrt((tail(spline_ctrl$x,1) - end_points$end$x)^2+
           (tail(spline_ctrl$y, 1) - end_points$end$y)^2)

  # TODO: extend to multiple ctrl points as regular bezier curves as they do for instance in Inkscape
  new_bp <- 
    getBezierAdj4Arrw(end_points = end_points, spline_ctrl = spline_ctrl,
                      arrow_length = arrow$length,
                      internal.units = internal.units,
                      vp = vp)

  # Get lengths
  new_bp$lengths <-
    with(new_bp, 
         mapply(x1 = x[-length(x)], 
                y1 = y[-length(y)], 
                x2 = x[-1], 
                y2 = y[-1], 
                function(x1, y1, x2, y2) sqrt((x2-x1)^2 + (y2-y1)^2)))
  
  # Add the arrow length to the last element
  new_bp$lengths[length(new_bp$lengths)] <- 
    tail(new_bp$lengths, 1) +
    arrow$length
  
  lines <- calculateLinesAndArrow(x = new_bp$x, y = new_bp$y,
                                  offset = width/2, 
                                  end_x = end_points$end$x,
                                  end_y = end_points$end$y,
                                  arrow_offset = arrow$base/2)

  # Change evrything to default.units from internal
  lines <- lapply(lines, 
                  function(x) lapply(x, function(xx) unit(xx, internal.units)))
  lines$left$x <- convertX(lines$left$x, unitTo=default.units)
  lines$right$x <- convertX(lines$right$x, unitTo=default.units)

  lines$left$y <- convertY(lines$left$y, unitTo=default.units)
  lines$right$y <- convertY(lines$right$y, unitTo=default.units)

  new_bp <- lapply(new_bp, function(x) unit(x, internal.units))  
  new_bp$x <- convertX(new_bp$x, unitTo=default.units)
  new_bp$y <- convertY(new_bp$y, unitTo=default.units)

  end_points$start$x <- convertX(unit(end_points$start$x, internal.units),
                                 unitTo=default.units)
  end_points$start$y <- convertY(unit(end_points$start$y, internal.units),
                                 unitTo=default.units)
  end_points$end$x <- convertX(unit(end_points$end$x, internal.units),
                                 unitTo=default.units)
  end_points$end$y <- convertY(unit(end_points$end$y, internal.units),
                               unitTo=default.units)

  poly_x <- unit.c(lines$left$x,
                   rev(lines$right$x))
  poly_y <- unit.c(lines$left$y,
                   rev(lines$right$y))
  pg <- polygonGrob(x=poly_x,
                    y=poly_y,
                    gp=gpar(fill=clr, col=clr), 
                    name = name,
                    vp = vp)

  # Add details that are used by the gradient version
  attr(pg, "center_points") <- new_bp
  attr(pg, "left_points") <- lines$left
  attr(pg, "right_points") <- lines$right
  attr(pg, "end_points") <- end_points

  return(pg)
}
