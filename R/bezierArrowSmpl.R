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
#' @example examples/transitionPlot_example.R
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
  
  # TODO: extend to multiple ctrl points as regular bezier curves as they do for instance in Inkscape 
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
  
  getBestMatchForArrowLengthAlongCurve <- function (bp, arrow_length) {

    arrow_length <- getGridVal(arrow_length, "npc")
    
    dist2end <- sqrt((bp$x-tail(bp$x, 1))^2+
            (bp$y-tail(bp$y, 1))^2)
    best_point <- tail(which(dist2end > arrow_length), 1)
    
    return(best_point)
  }
  bp$cut_point <- getBestMatchForArrowLengthAlongCurve(bp, arrow$length)
    
  # Set the arrow details according to this new information
  arrow$x <- end_points$end$x - bp$x[bp$cut_point]
  arrow$y <- end_points$end$y - bp$y[bp$cut_point]
  #arrow$length <- sqrt(arrow$x^2+arrow$y^2)
  
  getBezierAdjustedForArrow <- function(bp, end_points, spline_ctrl, arrow){
    a_l <- getGridVal(arrow$length, "npc")
    multiplier <- (spline_ctrl$end$length-a_l*1.1)/a_l
    # Use the arrow's vector in the opposite direction as the new ctrl point
    spline_ctrl$x[length(spline_ctrl$x)] <- -arrow$x*multiplier + bp$x[bp$cut_point]
    spline_ctrl$y[length(spline_ctrl$y)] <- -arrow$y*multiplier + bp$y[bp$cut_point]
    
    # Relate to full length
    tot_line_length <- sum(getBzLength(x = bp$x, y= bp$y))
    simple_start_adjustment <- 1-a_l/tot_line_length/3
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

  extendBp2MatchArrowLength <- function (bp, end, arrow_length){
    arrow_length <- getGridVal(arrow_length, "npc")
    bp_last_x <- tail(bp$x, 1)
    bp_last_y <- tail(bp$y, 1)
    dist2end <- sqrt((bp_last_x-end$x)^2+
            (bp_last_y-end$y)^2)
    
    if (dist2end != arrow_length){
      partial_distance <- 1-arrow_length/dist2end
      add_x <- bp_last_x + 
          (end$x-bp_last_x)*partial_distance
      add_y <- bp_last_y + 
          (end$y-bp_last_y)*partial_distance
      # Insert new point
      bp$x <- c(bp$x, add_x)
      bp$y <- c(bp$y, add_y)
    }
    
    return (bp)
  }
  new_bp <- extendBp2MatchArrowLength(new_bp, 
      end = end_points$end, 
      arrow_length = arrow$length)
  
  # Get lengths
  new_bp$lengths <- getBzLength(new_bp$x, new_bp$y)
  
  # Add the arrow length to the last element
  new_bp$lengths[length(new_bp$lengths)] <- tail(new_bp$lengths, 1) + 
    getGridVal(arrow$length, "npc")
  lines <- getLinesWithArrow(bp = new_bp, 
      arrow = arrow, 
      width = width, 
      end_points = end_points,
      default.units = default.units,
      align_2_axis = align_2_axis)
  
  pg <- polygonGrob(x=unit.c(lines$left$x,
                             rev(lines$right$x)),
                    y=unit.c(lines$left$y,
                             rev(lines$right$y)),
                    gp=gpar(fill=clr, col=clr), # col=NA, - messes up the anti-aliasing
                    name = name,
                    vp = vp)
  
  # Add details that are used by the gradient version
  attr(pg, "center_points") <- new_bp
  attr(pg, "upper_points") <- lines$left
  attr(pg, "lower_points") <- lines$right
  attr(pg, "end_points") <- end_points
  
  return(pg)
}
