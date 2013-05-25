#' A simple bezier arrow
#' 
#' This is an alternative to the grid packages \code{\link{bezierGrob}}
#' with the advantage that it allows you to draw an arrow with a specific
#' unit width. Note, it has only a end-arrow at this point. 
#'  
#' @param x A numeric vector or unit object specifying x-locations of spline control points. 
#' @param y A numeric vector or unit object specifying y-locations of spline control points.
#' @param width The width of the arrow, either a numeric single number or a unit. \strong{Note:}
#'  The arrow does not rely on lwd but on actual width.
#' @param clr The color of the arrow.
#' @param default.units A string indicating the default units to use if x or y are only given as numeric vectors.
#' @param arrow This is a list with all the \strong{base} (width) and the desired \strong{length} for the arrow. 
#'  \strong{Note:} This differs from the original \code{\link{bezierGrob}} function.   
#' @param align_2_axis Indicates if the arrow should be vertically/horizontally aligned. This
#'  is useful for instance if the arrow attaches to a box.
#' @param name A character identifier.
#' @param gp An object of class gpar, typically the output from a call to the function gpar. 
#'  This is basically a list of graphical parameter settings.
#' @param vp A Grid viewport object (or NULL).
#' @return A grob of the class polygonGrob with attributes that correspond to the bezier points.
#' 
#' @author max
#' @export
bezierArrowSmpl <- function(x = c(0.2, .7, .3, .9), y = c(0.2, .2, .9, .9), 
                            width = .05,
                            clr = "#000000",
                            default.units = "npc", 
                            arrow = list(base=unit(.1, "npc"),
                                         length = unit(.1, "npc")),
                            align_2_axis = TRUE,
                            name = NULL, 
                            gp = gpar(), vp = NULL){
  require(grid)
  if (class(x) == "unit")
    x <- convertX(x, unitTo=default.units, valueOnly=TRUE)
  if (class(y) == "unit")
    y <- convertY(y, unitTo=default.units, valueOnly=TRUE)
  
  if (length(y) != length(x))
    stop("You have provided unequal lengths to y and x - thus uninterpretable:",
      " y=", length(y), " elements",
      " while x=", length(x), " elements")
  
  # According to the original description they're all spline
  # control points but as I want the line to start and end 
  # at specific points then this makes sense to me  
  end_points <- list(start=list(x=x[1],
                                y=y[1]),
                     end=list(x=tail(x, 1),
                              y=tail(y, 1)))
                          
  spline_ctrl <- list(x=x[2:(length(x)-1)],
                      y=y[2:(length(y)-1)])
                          
  spline_ctrl$start$length <- sqrt((spline_ctrl$x[1] - end_points$start$x)^2+
      (spline_ctrl$y[1] - end_points$start$y)^2)
  spline_ctrl$end$length <- sqrt((tail(spline_ctrl$x,1) - end_points$end$x)^2+
      (tail(spline_ctrl$y, 1) - end_points$end$y)^2)
  
  
  bz_grob <- bezierGrob(x=c(end_points$start$x, spline_ctrl$x, end_points$end$x), 
                        y=c(end_points$start$y, spline_ctrl$y, end_points$end$y), 
                        default.units=default.units, vp=vp)
  bp <- bezierPoints(bz_grob)
  # Change to values that we can work with arithmetically
  bp$y <- convertY(bp$y, unitTo=default.units, valueOnly=TRUE)
  bp$x <- convertX(bp$x, unitTo=default.units, valueOnly=TRUE)
  getBzLength <- function(x, y){
    m <- rbind(y, x)
    # Map the change between coordinates
    m <- m[, 2:ncol(m)] - m[, 1:(ncol(m)-1)]
    # Set first element to 0 length
    m <- cbind(c(0,0), m)
    # The old sqrt(a^2+b^2) formula
    return(sqrt(colSums(m^2)))
  }
  bp$lengths <- getBzLength(bp$x, bp$y)
  
  getBestMatchForArrowLengthAlongCurve <- function (bp, arrow_length) {
    if (class(arrow_length) == "unit")
      arrow_length <- convertUnit(arrow_length, unitTo="npc", valueOnly=TRUE)
    no_points <- length(bp$y)
    end_point <- list(x=bp$x[no_points],
      y=bp$y[no_points])
    # Just a guess
    best_point <- round(no_points*.9)
    for(i in 1:no_points){
      lb <- sqrt((bp$x[best_point]-end_point$x)^2+
          (bp$y[best_point]-end_point$y)^2)
      l_plus <- sqrt((bp$x[best_point+1]-end_point$x)^2+
          (bp$y[best_point+1]-end_point$y)^2)
      l_minus <- sqrt((bp$x[best_point-1]-end_point$x)^2+
          (bp$y[best_point-1]-end_point$y)^2)
      best_diff <- abs(lb - arrow_length)
      plus_diff <- abs(l_plus - arrow_length)
      minus_diff <- abs(l_minus - arrow_length)
      
      if (best_diff < plus_diff &&
        best_diff < minus_diff)
        return(best_point)
      else if (plus_diff < minus_diff)
        best_point <- best_point + 1
      else
        best_point <- best_point - 1
      
      # Reached the end without finding an optimal point
      if (best_point == 1 ||
        best_point == no_points){
        break;
      }
    }
    
    warning("Could not find the optimal point along the line",
      " that would correspond to the desired arrow length")
    return(best_point)
  }
  bp$cut_point <- getBestMatchForArrowLengthAlongCurve(bp, arrow$length)
  
  # Set the arrow details according to this new information
  arrow$x <- end_points$end$x - bp$x[bp$cut_point]
  arrow$y <- end_points$end$y - bp$y[bp$cut_point]
  arrow$length <- sqrt(arrow$x^2+arrow$y^2)
  
  getBezierAdjustedForArrow <- function(bp, end_points, spline_ctrl, arrow){
    multiplier <- (spline_ctrl$end$length-arrow$length*1.1)/arrow$length
    # Use the arrow's vector in the opposite direction as the new ctrl point
    spline_ctrl$x[length(spline_ctrl$x)] <- -arrow$x*multiplier + bp$x[bp$cut_point]
    spline_ctrl$y[length(spline_ctrl$y)] <- -arrow$y*multiplier + bp$y[bp$cut_point]
    
    simple_start_adjustment <- 1-arrow$length/sum(bp$lengths)/3
    # Remove a fraction of the distance for the spline controles
    spline_ctrl$x[1] <- end_points$start$x + (spline_ctrl$x[1]-end_points$start$x)*simple_start_adjustment
    spline_ctrl$y[1] <- end_points$start$y + (spline_ctrl$y[1]-end_points$start$y)*simple_start_adjustment
    
    return(bezierGrob(x=c(end_points$start$x, spline_ctrl$x, bp$x[bp$cut_point]), 
                      y=c(end_points$start$y, spline_ctrl$y, bp$y[bp$cut_point]), 
                      default.units=default.units,
                      vp=vp))
  }
  
  new_bz_grob <- getBezierAdjustedForArrow(bp, end_points, spline_ctrl, arrow)
  
  # Get the bezier points that are adjusted for the arrow
  new_bp <- bezierPoints(new_bz_grob)
  new_bp$y <- convertY(new_bp$y, unitTo=default.units, valueOnly=TRUE)
  new_bp$x <- convertX(new_bp$x, unitTo=default.units, valueOnly=TRUE)
  new_bp$lengths <- getBzLength(new_bp$x, new_bp$y)
  # Add the arrow length to the last element
  new_bp$lengths[length(new_bp$lengths)] <- new_bp$lengths[length(new_bp$lengths)] + 
    arrow$length
  lines <- getLinesWithArrow(bp = new_bp, arrow = arrow, 
    width = width, end_points = end_points,
    default.units = default.units,
    align_2_axis = align_2_axis)
  
  pg <- polygonGrob(x=unit.c(lines$left$x,
                             rev(lines$right$x)),
                    y=unit.c(lines$left$y,
                             rev(lines$right$y)),
                    gp=gpar(col=NA, fill=clr),
                    name = name,
                    vp = vp)
  
  attr(pg, "center_points") <- new_bp
  attr(pg, "upper_points") <- lines$left
  attr(pg, "lower_points") <- lines$right
  attr(pg, "end_points") <- end_points
  
  return(pg)
}
