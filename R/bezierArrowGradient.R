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
#' @param grdt_prop The proportion of the full length that should be a the gradient. The gradient
#'  consists of three things:
#'  (1) the central band,
#'  (2) the slimming of the central band,
#'  (3) the color shift into the arrow color.
#'  \emph{Note} that the the slimming and color proportions can be overlapping.
#' @param grdt_decrease_prop The proportion of the gradient that should be decreasing,
#'  i.e. narrowing according to the \code{grdt_type} argument.
#' @param grdt_clr_prop The proportion of the gradient that should be converging to the arrow color.
#' @param grdt_line_width The width of the border line. If not specified it defaults to 5 \% of
#'  the original width, note the gradient's width is thus 90 \%.
#' @param grdt_clr The color of the gradient.
#' @param ... Passed on to \code{\link{bezierArrowSmpl}}
#' @return A grob of \code{\link[grid:grid.grob]{gList}}-type
#'
#' @inheritParams calculateLinesAndArrow
#' @examples
#' library(grid)
#' grid.newpage()
#' arrowGrob <- bezierArrowGradient(
#'   x = c(.1, .3, .6, .9),
#'   y = c(0.2, 0.2, 0.9, 0.9)
#' )
#' grid.draw(arrowGrob)
#' @importFrom utils head tail
#' @export
bezierArrowGradient <- function(x = c(0.2, .7, .3, .9),
                                y = c(0.2, .2, .9, .9),
                                width = .05,
                                clr = "#000000",
                                default.units = "npc",
                                align_2_axis = TRUE,
                                grdt_type = c("triangle", "rectangle"),
                                grdt_prop = .8,
                                grdt_decrease_prop = .5,
                                grdt_clr_prop = .7,
                                grdt_line_width,
                                grdt_clr = "#2F4F2F",
                                vp = NULL,
                                gp = gpar(),
                                rm_intersect = 3L,
                                ...) {
  if ("grdt_start_prop" %in% names(list(...))) {
    stop(
      "The grdt_start_prop has been deprecated. Switch to grdt_prop instead.",
      "\n**Note** The grdt_prop is a total of how large proportion of the arrow",
      " should be covered with a gradient. This is different from before where",
      " the grdt_start_prop was added to the grdt_decrease_prop in order to",
      " generate this value."
    )
  }

  # Get initial values
  grdt_type <- match.arg(grdt_type)

  # Adapt line width to the direction if default.units == "npc"
  width <- getAbsoluteWidth(
    w = width,
    default.units = default.units,
    x = x,
    y = y
  )

  if (missing(grdt_line_width)) {
    grdt_line_width <- unit(getGridVal(width, "mm") * .5, units = "mm")
  }

  grdt_line_width <- getAbsoluteWidth(
    w = grdt_line_width,
    default.units = default.units,
    x = x,
    y = y
  )

  if (!inherits(grdt_line_width, "unit")) {
    grdt_line_width <- unit(grdt_line_width, default.units)
  }

  # Sanity check for input parameters
  if (grdt_prop > 1 &&
    grdt_prop < 0) {
    stop("The gradient proportion (grdt_prop) must be between 0-1! You provided ", grdt_prop)
  }

  if (grdt_decrease_prop > 1 &&
    grdt_decrease_prop < 0) {
    stop("The decrease proportion must be between 0-1! You provided ", grdt_decrease_prop)
  }

  if (grdt_clr_prop > 1 &&
    grdt_clr_prop < 0) {
    stop("The color gradient proportion must be between 0-1! You provided ", grdt_clr_prop)
  }

  if (getGridVal(width, default.units) < getGridVal(grdt_line_width, default.units)) {
    stop("Your gradient width exceeds the width of the arrow")
  }

  # Get the pure arrow
  pg <- bezierArrowSmpl(
    x = x, y = y,
    width = width,
    default.units = default.units,
    clr = clr, align_2_axis = align_2_axis,
    gp = gp, vp = vp,
    rm_intersect = rm_intersect,
    ...
  )

  # Now to the gradient
  bp <- attr(pg, "center_points")
  end_points <- attr(pg, "end_points")

  internal.units <- "mm"
  end_points <- lapply(end_points, function(pt) {
    list(
      x = convertX(pt$x, unitTo = internal.units),
      y = convertY(pt$y, unitTo = internal.units)
    )
  })
  bp$x <- convertX(bp$x, unitTo = internal.units, valueOnly = TRUE)
  bp$y <- convertY(bp$y, unitTo = internal.units, valueOnly = TRUE)
  # Extend to the full line
  bp$x <- c(bp$x, end_points$end$x)
  bp$y <- c(bp$y, end_points$end$y)

  grdt_line_width <- convertX(grdt_line_width, unitTo = internal.units, valueOnly = TRUE)

  # Calculate the lengths
  lenCalc <- function(bp) {
    bp$lengths <-
      c(
        0,
        with(
          bp,
          mapply(
            x1 = x[-length(x)],
            y1 = y[-length(y)],
            x2 = x[-1],
            y2 = y[-1],
            function(x1, y1, x2, y2) sqrt((x2 - x1)^2 + (y2 - y1)^2)
          )
        )
      )
    bp$cumlen <- cumsum(bp$lengths)
    return(bp)
  }
  bp <- lenCalc(bp)

  grdt_length <- sum(bp$lengths) * (grdt_prop)
  end_point <-
    (bp$cumlen - grdt_length) %>%
    abs() %>%
    which.min()

  # Adjust last point if discrepancy > 10%
  if (abs(bp$cumlen[end_point] - grdt_length) > bp$length[end_point] / 10) {
    if (bp$cumlen[end_point] - grdt_length < 0 &&
      end_point <= length(bp$x)) {
      dx <- bp$x[end_point + 1] - bp$x[end_point]
      dy <- bp$y[end_point + 1] - bp$y[end_point]
      adj_prop <- (grdt_length - bp$cumlen[end_point]) / bp$length[end_point + 1]
    } else {
      dx <- bp$x[end_point] - bp$x[end_point - 1]
      dy <- bp$y[end_point] - bp$y[end_point - 1]
      adj_prop <- (grdt_length - bp$cumlen[end_point]) / bp$length[end_point]
    }

    # Desired length vs actual length
    bp$x[end_point] <- bp$x[end_point] +
      dx * adj_prop
    bp$y[end_point] <- bp$y[end_point] +
      dy * adj_prop
    # Adjust the lengths
    bp$lengths[end_point] <- abs(bp$cumlen[end_point] - grdt_length)
    bp$cumlen[end_point] <- grdt_length

    # Shorten to the end
    bp <- lapply(bp, function(x) head(x, n = end_point))

    # If the line was extended with a length beyond the regular
    # length we should split the last element into sub-elements
    avg_len <- mean(bp$lengths[-end_point])
    if (tail(bp$lengths, 1L) > avg_len * 2) {
      no_splits <- round(tail(bp$lengths, 1L) / avg_len)
      last_2_pt <- lapply(bp, tail, n = 2L)
      bp <- lapply(bp, head, n = length(bp$x) - 2)

      spltVals <- function(vals, tail_vals, no_splits) {
        c(
          vals,
          seq(
            from = tail_vals[1],
            to = tail(tail_vals, 1),
            length.out = no_splits
          )
        )
      }
      bp$x <- spltVals(bp$x, last_2_pt$x, no_splits)
      bp$y <- spltVals(bp$y, last_2_pt$y, no_splits)
      bp <- lenCalc(bp)
    }
  } else {
    # Shorten to the end
    bp <- lapply(bp, function(x) head(x, end_point))
  }

  # Add start margin
  margin <- grdt_line_width / 2
  line_start <-
    (bp$cumlen - margin) %>%
    abs() %>%
    which.min()

  if (bp$cumlen[line_start] - margin > 0) {
    line_start <- line_start - 1
  }

  # Remove start
  if (line_start > 1) {
    bp <- lapply(bp, function(x) x[-1 * (1:line_start - 1)])
    if (bp$cumlen[1] != margin) {
      dx <- bp$x[2] - bp$x[1]
      dy <- bp$y[2] - bp$y[1]

      adj_prop <- (bp$cumlen[2] - margin) / bp$length[2]

      bp$x[1] <- bp$x[1] +
        dx * adj_prop
      bp$y[1] <- bp$y[1] +
        dy * adj_prop
    }

    bp <- lenCalc(bp)
  }


  ########################
  # Get the decrease and #
  # color start points   #
  ########################
  start_decrease <-
    (bp$cumlen - tail(bp$cumlen, 1) * (1 - grdt_decrease_prop)) %>%
    abs() %>%
    which.min()

  clr_start <-
    (bp$cumlen - tail(bp$cumlen, 1) * (1 - grdt_clr_prop)) %>%
    abs() %>%
    which.min() %>%
    +1


  # The offset decrease should be relative
  offset <- rep(grdt_line_width / 2, length(bp$x))
  if (grdt_type == "triangle") {
    decr_lengths <- tail(bp$lengths, length(offset) - start_decrease)
    offset[start_decrease:length(offset)] <-
      offset[start_decrease:length(offset)] *
        (1 - c(0, cumsum(decr_lengths)) / sum(decr_lengths))
  }

  grdnt_lines <- calculateLinesAndArrow(x = bp$x, y = bp$y, offset = offset, rm_intersect = rm_intersect)
  if (attr(pg, "axis") != FALSE) {
    grdnt_lines <- align2Axis(
      bp = bp,
      lines = grdnt_lines,
      width = grdt_line_width,
      internal.units = internal.units,
      axis = attr(pg, "axis")
    )
  }

  # Prepare for the correct size
  grdnt_lines$left$x <- convertX(unit(grdnt_lines$left$x, units = internal.units), unitTo = default.units)
  grdnt_lines$right$x <- convertX(unit(grdnt_lines$right$x, units = internal.units), unitTo = default.units)
  grdnt_lines$left$y <- convertY(unit(grdnt_lines$left$y, units = internal.units), unitTo = default.units)
  grdnt_lines$right$y <- convertY(unit(grdnt_lines$right$y, units = internal.units), unitTo = default.units)

  inner_gradient <- gList()
  if (clr_start > 2) {
    reg_size <-
      lapply(
        grdnt_lines,
        function(side) {
          lapply(
            side,
            function(x) head(x, clr_start - 1)
          )
        }
      )
    gradient_pg <-
      with(
        reg_size,
        polygonGrob(
          x = c(left$x, rev(right$x)),
          y = c(left$y, rev(right$y)),
          gp = gpar(fill = grdt_clr, col = grdt_clr)
        )
      )

    inner_gradient <- gList(inner_gradient, gradient_pg)
  }

  # Plot the gradient color - these need to be plotted as
  # one color per polygon
  if (length(bp$x) >= clr_start) {
    elmnts <- (clr_start - 1):(length(grdnt_lines$right$x) - 1)
    g_clrs <- colorRampPalette(colors = c(grdt_clr, clr))(length(elmnts))
    # Speeds signifacantly the process
    decr_polygons <- list()
    for (i in elmnts) {
      col <- g_clrs[i - clr_start + 2]
      x <- with(grdnt_lines, c(left$x[i:(i + 1)], right$x[(i + 1):i]))
      y <- with(grdnt_lines, c(left$y[i:(i + 1)], right$y[(i + 1):i]))
      decr_polygons[[i - clr_start + 2]] <-
        polygonGrob(
          x = x,
          y = y,
          gp = gpar(fill = col, col = col)
        )
    }
    inner_gradient <-
      gList(inner_gradient, fastDoCall(gList, decr_polygons))
  }

  return(gList(pg, inner_gradient))
}
