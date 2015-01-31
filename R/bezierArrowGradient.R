#' A bezier arrow with gradient
#'
#' This is an experimental addition to the original \code{\link{bezierArrowSmpl}}
#' with the addition of a gradient in the center of the arrow that fades.
#'
#' @section Note:
#' The triangle section of the arrow is not currently included in the gradient.
#'
#' @inheritParams grid::bezierGrob
#' @inheritParams bezierArrowSmpl
#' @param clr The color of the arrow. This is the main color of the arrow and not the gradient color.
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
#' @return \code{grid::gList()} A grob of \code{\link[grid]{gList}}-type
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
  grdt_type = c("triangle", "rectangle"),
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

  if (is.na(grdt_line_width)){
    grdt_line_width <- getGridVal(width, default.units)*.2
  }

  if (!inherits(grdt_line_width, "unit")){
    grdt_line_width <- unit(grdt_line_width, default.units)
  }

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

  internal.units <- "mm"
  end_points <- lapply(end_points, function(pt){
    list(x = convertX(pt$x, unitTo=internal.units),
         y = convertY(pt$y, unitTo=internal.units))
  })
  bp$x <- convertX(bp$x, unitTo = internal.units, valueOnly = TRUE)
  bp$y <- convertY(bp$y, unitTo = internal.units, valueOnly = TRUE)
  # Extend to the full line
  bp$x <- c(bp$x, end_points$end$x)
  bp$y <- c(bp$y, end_points$end$y)

  grdt_line_width <- convertX(grdt_line_width, unitTo = internal.units, valueOnly = TRUE)

  # Calculate the lengths
  bp$lengths <-
    c(0,
      with(bp,
         mapply(x1 = x[-length(x)],
                y1 = y[-length(y)],
                x2 = x[-1],
                y2 = y[-1],
                function(x1, y1, x2, y2) sqrt((x2-x1)^2 + (y2-y1)^2))))
  bp$cumlen <- cumsum(bp$lengths)

  grdt_length <- sum(bp$lengths)*(grdt_start_prop + grdt_decrease_prop)
  end_point <-
    (bp$cumlen - grdt_length) %>%
    abs %>%
    which.min

  # Adjust last point if discrepancy > 10%
  if (abs(bp$cumlen[end_point] - grdt_length) > bp$length[end_point]/10){
    dx <- bp$x[end_point] - bp$x[end_point - 1]
    dy <- bp$y[end_point] - bp$y[end_point - 1]

    # Desired length vs actual length
    adj_prop <- (grdt_length - bp$cumlen[end_point])/bp$length[end_point]
    bp$x[end_point] <- bp$x[end_point] +
      dx * adj_prop
    bp$y[end_point] <- bp$y[end_point] +
      dy * adj_prop
    # Adjust the lengths
    bp$lengths[end_point] <- abs(bp$cumlen[end_point] - grdt_length)
    bp$cumlen[end_point] <- bp$cumlen[end_point] + grdt_length
  }

  start_decrease <-
    (bp$cumlen -
       sum(bp$lengths)*(grdt_start_prop)) %>%
    abs %>%
    which.min

  # Add start margin
  line_start <-
    (bp$cumlen -
       grdt_line_width/2) %>%
    abs %>%
    which.min

  if (bp$cumlen[line_start + 1] - grdt_line_width/2 < 0){
    line_start <- line_start - 1
  }

  clr_start <-
    (bp$cumlen[line_start:end_point] -
       bp$cumlen[end_point] * grdt_clr_prop) %>%
    abs %>%
    which.min %>%
    + 1

  # Remove start
  if (line_start > 1){
    bp <- lapply(bp, function(x) x[-1*(1:line_start - 1)])
  }
  end_point <- end_point - line_start + 1
  start_decrease <- start_decrease - line_start + 1

  if (abs(bp$cumlen[2] - grdt_line_width/2) > grdt_line_width/10^3){
    dx <- bp$x[2] - bp$x[1]
    dy <- bp$y[2] - bp$y[1]

    adj_prop <- (bp$cumlen[2] - grdt_line_width/2)/bp$length[2]
    bp$x[1] <- bp$x[1] +
      dx * adj_prop
    bp$y[1] <- bp$y[1] +
      dy * adj_prop
  }

  # Shorten to the end
  bp <- lapply(bp, function(x) head(x, end_point))

  # The offset decrease should be relative
  offset <- rep(grdt_line_width/2, length(bp$x))
  if (end_point > start_decrease &&
        grdt_type == "triangle"){
    decr_lengths <- tail(bp$lengths, end_point - start_decrease)
    offset[start_decrease:end_point] <-
      grdt_line_width/2 *
      (1 - c(0, cumsum(decr_lengths))/sum(decr_lengths))
  }

  grdnt_lines <- calculateLinesAndArrow(x = bp$x, y = bp$y, offset = offset)
  if (attr(pg, "axis") != FALSE){
    grdnt_lines <- align2Axis(bp = bp,
                              lines = grdnt_lines,
                              width = grdt_line_width,
                              internal.units = internal.units,
                              axis = attr(pg, "axis"))
  }

  #   max_gradient_width <- getGridVal(width, default.units) -
  #     2*getGridVal(grdt_line_width, default.units)
  convGenPolyGrob <- function(x, y, d.u, i.u, gp){
    x <- convertX(unit(x, units = i.u), unitTo = d.u)
    y <- convertY(unit(y, units = i.u), unitTo = d.u)
    polygonGrob(x = x,
                y = y,
                gp = gp)
  }

  inner_gradient <- gList()
  if (clr_start > 2){
    reg_size <-
      lapply(grdnt_lines,
             function(side) lapply(side,
                                   function(x) head(x, clr_start - 1)))
    gradient_pg <-
      with(reg_size,
           convGenPolyGrob(x = c(left$x, rev(right$x)),
                           y = c(left$y, rev(right$y)),
                           d.u = default.units,
                           i.u = internal.units,
                           gp = gpar(fill = grdt_clr, col = grdt_clr, lwd = .01)))

    inner_gradient <- gList(inner_gradient, gradient_pg)

    # Remove the already plotted
    grdnt_lines <-
      lapply(grdnt_lines,
             function(side) lapply(side,
                                   function(x) (x[-1*(1:(clr_start - 2))])))
  }

  # Plot the gradient color - these need to be plotted as
  # one color per polygon
  if (length(bp$x) >= clr_start){
    g_clrs <- colorRampPalette(colors=c(grdt_clr, clr))(length(grdnt_lines$right$x))
    for (i in 1:(length(grdnt_lines$right$x) - 1)){
      gradient_pg <-
        with(grdnt_lines,
             convGenPolyGrob(x = c(left$x[i:(i+1)], right$x[(i+1):i]),
                             y = c(left$y[i:(i+1)], right$y[(i+1):i]),
                             d.u = default.units,
                             i.u = internal.units,
                             gp = gpar(fill = g_clrs[i], col = g_clrs[i], lwd = .01)))

      inner_gradient <- gList(inner_gradient, gradient_pg)
    }
  }

  return (gList(pg, inner_gradient))
}
