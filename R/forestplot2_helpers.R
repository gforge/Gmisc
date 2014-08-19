#' Draw standard confidence intervals
#'
#' A function that is used to draw the different
#' confidence intervals for the non-summary lines.
#' Use the \code{fpDrawNormalCI} function as a
#' template if you want to make your own funky line + marker.
#'
#' @param lower_limit The lower limit of the confidence line.
#'  A native numeric variable that can actually be
#'  outside the boundaries. If you want to see if it
#'  is outside then convert it to 'npc' and see if the
#'  value ends up more than 1 or less than 0. Here's how
#'  you do the conversion:
#'  \code{convertX(unit(upper_limit, "native"), "npc", valueOnly = TRUE)}
#'  and the \code{\link[grid]{convertX}} together with \code{\link[grid]{unit}}
#'  is needed to get the right values while you need to provide the valueOnly
#'  as you cannot compare a unit object.
#' @param estimate The estimate indicating the placement
#'  of the actual box. Note, this can also be outside bounds
#'  and is provided in a numeric format the same way as the
#'  \code{lower_limit}.
#' @param upper_limit The upper limit of the confidence line. See
#'  lower_limit for details.
#' @param size The actual size of the box/diamond/marker.
#'  This provided in the 'snpc' format to generate a perfect
#'  marker. Although you can provide it alternative units as well,
#'  this is useful for the legends to work nicely.
#' @param y.offset If you have multiple lines they need an offset in
#'  the y-direction.
#' @param clr.line The color of the line.
#' @param clr.marker The color of the estimate marker
#' @param lwd Line width
#' @param ... Allows additional parameters for sibling functions
#' @return \code{void} The function outputs the line using grid compatible
#'  functions and does not return anything.
#'
#' @example inst/examples/forestplot2_alt_ci_example.R
#' @rdname fpDrawCI
#' @export
#' @family forestplot functions
fpDrawNormalCI <- function(lower_limit,
                           estimate,
                           upper_limit,
                           size,
                           y.offset = 0.5,
                           clr.line, clr.marker,
                           lwd,
                           ...) {

  # Funciton for drawing the confidence line
  prFpDrawLine(lower_limit = lower_limit,
               upper_limit = upper_limit,
               clr.line = clr.line,
               lwd = lwd,
               y.offset = y.offset)

  # If the box is outside the plot the it shouldn't be plotted
  box <- convertX(unit(estimate, "native"), "npc", valueOnly = TRUE)
  skipbox <- box < 0 || box > 1

  # Lastly draw the box if it is still there
  if (!skipbox){
    # Convert size into 'snpc'
    if(!is.unit(size)){
      size <- unit(size, "snpc")
    }

    # Draw the actual box
    grid.rect(x = unit(estimate, "native"),
              y = y.offset,
              width = size,
              height = size,
              gp = gpar(fill = clr.marker,
                        col = clr.marker))
  }
}

#' Draws a straight line
#'
#' If the line is outside the boundaries the line
#' is clipped with an arrow at the limit indicating
#' that it continues. If the lower limit is not below
#' the upper limit the line is not drawn.
#'
#' @inheritParams fpDrawNormalCI
#' @return \code{void}
prFpDrawLine <- function (lower_limit, upper_limit, clr.line, lwd, y.offset) {
  # Draw the lines if the lower limit is
  # actually below the upper limit
  if (lower_limit >= upper_limit)
    return()

  # If the limit is outside the 0-1 range in npc-units
  # then that part is outside the box and it should
  # be clipped (this function adds an arrow to the end
  # of the line)
  clipupper <-
    convertX(unit(upper_limit, "native"),
             "npc",
             valueOnly = TRUE) > 1
  cliplower <-
    convertX(unit(lower_limit, "native"),
             "npc",
             valueOnly = TRUE) < 0

  gp_list <- list(col = clr.line)
  if (!missing(lwd))
    gp_list$lwd <- lwd

  grid_line_args <- list(y = y.offset,
                         gp = fastDoCall(gpar, gp_list))
  if (clipupper || cliplower) {
    # A version where arrows are added to the part outside
    # the limits of the graph
    ends <- "both"
    lims <- unit(c(0, 1), c("npc", "npc"))
    if (!clipupper) {
      ends <- "first"
      lims <- unit(c(0, upper_limit), c("npc", "native"))
    }
    if (!cliplower) {
      ends <- "last"
      lims <- unit(c(lower_limit, 1), c("native", "npc"))
    }
    grid_line_args$x <- lims
    grid_line_args$arrow <-
      arrow(ends = ends,
            length = unit(0.05, "inches"))
  } else {
    grid_line_args$x <- unit(c(lower_limit, upper_limit), "native")
  }

  fastDoCall(grid.lines, grid_line_args)
}

#' @rdname fpDrawCI
#' @export
fpDrawDiamondCI <- function(lower_limit,
                            estimate,
                            upper_limit,
                            size,
                            y.offset = 0.5,
                            clr.line, clr.marker,
                            lwd,
                            ...) {
  # Funciton for drawing the confidence line
  prFpDrawLine(lower_limit = lower_limit,
               upper_limit = upper_limit,
               clr.line = clr.line,
               lwd = lwd,
               y.offset = y.offset)

  # If the box is outside the plot the it shouldn't be plotted
  box <- convertX(unit(estimate, "native"), "npc", valueOnly = TRUE)
  if (box >= 0 &&
        box <= 1){

    # Convert size if needed
    default.size.unit = "snpc"
    if(is.unit(size)){
      size <- convertUnit(size, unitTo="mm", valueOnly=TRUE)
      default.size.unit = "mm"
    }

    grid.polygon(x = unit(estimate, "native") +
                   unit(c(-size/2, 0, +size/2, 0), default.size.unit),
                 y = unit(y.offset, "npc") +
                   unit(c(0, size/2, 0, -size/2), default.size.unit),
                 gp = gpar(fill = clr.marker,
                           col = clr.marker))

  }
}

#' @rdname fpDrawCI
#' @export
fpDrawCircleCI <- function(lower_limit,
                           estimate,
                           upper_limit,
                           size,
                           y.offset = 0.5,
                           clr.line, clr.marker,
                           lwd,
                           ...) {
  # Funciton for drawing the confidence line
  prFpDrawLine(lower_limit = lower_limit,
               upper_limit = upper_limit,
               clr.line = clr.line,
               lwd = lwd,
               y.offset = y.offset)

  # If the box is outside the plot the it shouldn't be plotted
  box <- convertX(unit(estimate, "native"), "npc", valueOnly = TRUE)

  if (box >= 0 &&
        box <= 1){
    # Convert size into 'mm' and switch to radius
    if(is.unit(size)){
      size <- convertUnit(size, unitTo="mm", valueOnly=TRUE)
      size <- unit(size/2, "mm")
    }else{
      size <- unit(size/2, "snpc")
    }

    grid.circle(x = unit(estimate, "native"),
                y = unit(y.offset, "npc"),
                r = size,
                gp = gpar(fill = clr.marker,
                          col = clr.marker))
  }
}

#' @rdname fpDrawCI
#' @param pch Type of point see \code{\link[grid]{grid.points}} for details
#' @export
fpDrawPointCI <- function(lower_limit,
                          estimate,
                          upper_limit,
                          size,
                          y.offset = 0.5,
                          clr.line, clr.marker,
                          lwd,
                          pch = 1,
                          ...) {
  # Funciton for drawing the confidence line
  prFpDrawLine(lower_limit = lower_limit,
               upper_limit = upper_limit,
               clr.line = clr.line,
               lwd = lwd,
               y.offset = y.offset)

  # If the box is outside the plot the it shouldn't be plotted
  box <- convertX(unit(estimate, "native"), "npc", valueOnly = TRUE)

  if (box >= 0 &&
        box <= 1){
    # Convert size into 'snpc' if not given
    if(!is.unit(size)){
      size <- unit(size, "snpc")
    }

    grid.points(x = unit(estimate, "native"),
                y = unit(y.offset, "npc"),
                size = size,
                pch = pch,
                gp = gpar(fill = clr.marker,
                          col = clr.marker))

  }
}

#' @rdname fpDrawCI
#' @param col The color of the summary diamond.
#' @export
fpDrawSummaryCI <- function(lower_limit, estimate, upper_limit,
                            size, col, y.offset = 0.5, ...) {
  # Convert size into 'npc' value only if
  # it is provided as a unit() object
  size <- ifelse(is.unit(size),
      convertUnit(size, unitTo="npc", valueOnly=TRUE),
      size)
  grid.polygon(x = unit(c(lower_limit, estimate, upper_limit, estimate), "native"),
               y = unit(y.offset +
                          c(0, 0.5 * size, 0, -0.5 * size), "npc"),
               gp = gpar(fill = col,
                         col = col))
}

#' A function for the color elements used in forestplot2()
#'
#' This function encapsulates all the colors that are used in the
#' \code{\link{forestplot2}} function. As there are plenty of color
#' options this function gathers them all in one place.
#'
#' If you have several values per row in a forestplot you can set
#' a color to a vector where the first value represents the first
#' line/box, second the second line/box etc. The vectors are only
#' valid for the \code{box} & \code{lines} options.
#'
#' This function is a copy of the \code{\link[rmeta]{meta.colors}}
#' function in the \pkg{rmeta} package.
#'
#' @param all.elements A color for all the elements. If set to NULL then
#'  it's set to the par("fg") color
#' @param box The color of the box indicating the estimate
#' @param lines The color of the confidence lines
#' @param summary The color of the summary
#' @param zero The color of the zero line
#' @param text The color of the text
#' @param axes The color of the x-axis at the bottom
#' @return list A list with the elements:
#' \item{box}{the color of the box/marker}
#' \item{lines}{the color of the lines}
#' \item{summary}{the color of the summary}
#' \item{zero}{the color of the zero vertical line}
#' \item{text}{the color of the text}
#' \item{axes}{the color of the axes}
#'
#' @author Max Gordon, Thomas Lumley
#' @importFrom grDevices colorRampPalette
#'
#' @export
#' @family forestplot functions
fpColors <- function (all.elements,
                      box        = "black",
                      lines      = "gray",
                      summary    = "black",
                      zero       = "lightgray",
                      text       = "black",
                      axes       = "black")
{
  if (missing(all.elements)) {
    # Make sure the color lengths match
    # if nott then add a slightly lighter/darker shade
    if (length(box) > length(lines)){
      nl <- length(lines)
      for (n in (nl+1):length(box))
        lines <- append(lines,
                        colorRampPalette(c(box[n], par("bg")))(10)[2])
    }else if (length(box) < length(lines)){
      nl <- length(box)
      for (n in (nl+1):length(lines))
        box <- append(box,
                        colorRampPalette(c(lines[n], par("fg")))(10)[2])
    }

    if (length(summary) < length(box))
      summary <- rep(summary, length.out = length(box))
    return(list(box = box, lines = lines, summary = summary,
                zero = zero, text = text, axes = axes))
  }

  if (is.null(all.elements))
    all.elements <- par("fg")

  return(list(box = all.elements,
              lines = all.elements,
              summary = all.elements,
              zero = all.elements,
              text = all.elements,
              axes = all.elements))
}

#' A function for the legend used in forestplot2()
#'
#' This function encapsulates all the legend options that are used in the
#' \code{\link{forestplot2}} function. This is in order to limit the crowding
#' among the arguments for the \code{\link{forestplot2}} call.
#'
#' @param pos The position of the legend, either at the "top" or the "right" unlesss
#'  positioned inside the plot. If you want the legend to be positioned inside the plot
#'  then you have to provide a list with the same x & y qualities as \code{\link[graphics]{legend}}.
#'  For instance if you want the legend to be positioned at the top right corner then
#'  use \code{pos = list("topright")} - this is equivalent to \code{pos = list(x=1, y=1)}.
#'  If you want to have a distance from the edge of the graph then add a inset to the list,
#'  e.g. \code{pos = list("topright", "inset"=.1)} - the inset should be either a \code{\link[grid]{unit}}
#'  element or a value between 0 and 1. The default is to have the boxes aligned vertical, if
#'  you want them to be in a line then you can specify the "align" option, e.g.
#'  \code{pos = list("topright", "inset"=.1, "align"="horizontal")}
#' @param cex The cex size of the legend, defaults to 80 \% procent of cex parameter.
#' @param gp The \code{\link[grid]{gpar}} options for the legend. If you want
#'  the background color to be light grey then use \code{gp = gpar(fill = "lightgrey")}.
#'  If you want a border then set the col argument: \code{gp = gpar(fill = "lightgrey", col="black")}.
#'  You can also use the lwd and lty argument as usual, \code{gp = gpar(lwd=2, lty=1)}, will result
#'  in a black border box of line type 1 and line width 2.
#' @param r The box can have rounded edges, check out \code{\link[grid]{grid.roundrect}}. The
#'  r option should be a \code{\link[grid]{unit}} object. This is by default \code{unit(0, "snpc")}
#'  but you can choose any value that you want. The \code{"snpc"} unit is the preferred option.
#' @param padding The padding for the legend box, only used if box is drawn. This is
#'  the distance from the border to the text/boxes of the legend.
#' @param title The title of the legend if any
#' @return \code{list} Returns a list with all the elements
#' @export
#' @family forestplot functions
fpLegend <- function(pos           = "top",
                     cex           = .8,
                     gp            = NULL,
                     r             = unit(0, "snpc"),
                     padding       = unit(ifelse(!is.null(gp), 3, 0), "mm"),
                     title         = NULL){
  return(list(pos = pos,
              cex = cex,
              gp = gp,
              r = r,
              padding = padding,
              title = title))
}

#' Get a function list
#'
#' This function helps the \code{\link{forestplot2}}
#' to deal with multiple drawing functions for the
#' confidence intervals.
#'
#' @param fn The function list/matrix. If a list it
#'  should be in the format [[row]][[col]], the function
#'  tries to handle this but in cases where the columns
#'  and rows are the same it will not know what is a column
#'  and what is a row.
#' @param no_rows Number of rows
#' @param no_cols Number of columns
#' @return \code{list} The function returns a list that has
#' the format [[row]][[col]] where each element contains the
#' function that you need to call using the \code{\link[base]{as.call}}
#' and \code{\link[base]{eval}} functions: \code{eval(as.call(list(fn[[row]][[col]], arg_1=1, arg_2=2)))}
#'
#'
#' @keywords internal
prFpGetConfintFnList <- function(fn, no_rows, no_cols){
  # Return a list that has
  # a two dim structure of [[row]][[col]]
  # if you have a matrix provided but if you
  # have only a vector with only 1 column then you
  # get the [[row]] by default
  # If the fn is a character or a matrix then
  ret <- list()
  if (is.function(fn)){
    if (no_cols == 1){
      for (i in 1:no_rows){
        ret[[i]] <- fn
      }
    }else{
      for (i in 1:no_rows){
        ret[[i]] <- list()
        for (ii in 1:no_cols){
          ret[[i]][[ii]] <- fn
        }
      }
    }
  }else if (typeof(fn) == "character"){
    if (is.matrix(fn)){
      if (ncol(fn) != no_cols)
        stop("Your columns do not add upp for your",
             " confidence interval funcitons, ",
             ncol(fn), " != ", no_cols)
      if (nrow(fn) != no_rows)
        stop("Your rows do not add upp for your",
             " confidence interval funcitons, ",
             nrow(fn), " != ", no_rows)

    }else if (length(fn) %in% c(1, no_rows)){
      fn <- matrix(fn, nrow=no_rows, ncol=no_cols)
    }else if (length(fn) == no_cols){
      fn <- matrix(fn, nrow=no_rows, ncol=no_cols, byrow=TRUE)
    }else{
      stop("You have not provided the expected",
           " number of funciton names: ",
           length(fn), " is not 1, ", no_cols, ", or ", no_rows)

    }

    # Convert into function format
    for (i in 1:no_rows){
      if (no_cols == 1){
        ret[[i]] <- get(fn[i, 1])
      }else{
        ret[[i]] <- list()
        for (ii in 1:no_cols){
          ## Go by row for the fn
          ret[[i]][[ii]] <- get(fn[i, ii])
        }
      }
    }

  }else if (is.list(fn)){
    if (no_cols == 1){
      # Actually correct if the lengths add up
      if (length(fn) != no_rows)
        stop("You do not have the same number of ",
             "confidence interval functions as you have ",
             "number of rows: ", length(fn), "!=", no_rows,
             " You should provide the same number.")
      ret <- fn
    }else{
      # Populate a new fn list
      if (length(fn) == no_rows){
        # One dim-list provided
        # now generate a two-dim list
        if (!is.list(fn[[1]])){
          for (i in 1:no_rows){
            ret[[i]] <- list()
            for (ii in 1:no_cols){
              ## Go by row for the fn
              ret[[i]][[ii]] <- fn[[i]]
            }
          }
        }else{
          # Verify that the list structure
          # is provided as a valid matrix
          # with the correct size
          n <- sapply(fn, length)
          if (any(n != no_cols)){
            stop("You need to provide a 'square' list (of dim. n x m)",
                 " of the same dimension as the number of lines",
                 " in order for this function to work. Currently your",
                 " confidence interval function has the format",
                 " ", no_rows , " x ", paste(n, collapse="/"),
                 " where you want all of the second argument to be",
                 " equal to ", no_cols)
          }

          ret <- fn
        }
      }else if (length(fn) == no_cols){
        # One dim-list provided
        # now generate a two-dim list
        if (!is.list(fn[[1]])){
          for (i in 1:no_rows){
            ret[[i]] <- list()
            for (ii in 1:no_cols){
              ## Go by row for the fn
              ret[[i]][[ii]] <- fn[[ii]]
            }
          }
        }else{
          # Verify that the list structure
          # is provided as a matrix
          n <- sapply(fn, length)
          if (any(n != no_rows)){
            stop("You need to provide a 'square' list (of dim. n x m)",
                 " of the same dimension as the number of lines",
                 " in order for this function to work. Currently your",
                 " confidence interval function has the format",
                 " ", no_rows , " x ", paste(n, collapse="/"),
                 " where you want all of the second argument to be",
                 " equal to ", no_cols)
          }

          # Change to the [[row]][[col]] format
          for (i in 1:no_rows){
            ret[[i]] <- list()
            for (ii in 1:no_cols){
              ## Go by row for the fn
              ret[[i]][[ii]] <- fn[[ii]][[i]]
            }
          }
        }
      }else{
        stop("The number of provided confidence intervals",
             " functions, ", length(fn), ", ",
             " does not seem to match up with either",
             " number of rows, ", no_rows,
             " or number of cols, ", no_cols)
      }
    }
  }else{
    stop("You have provided something else than",
         " a function, list or function name: ",
         class(fn))
  }

  return(ret)
}

#' A helper function to forestplot2
#'
#' Gets the x-label and zero-bar details
#'
#' @param x_range The range that the values from the different confidence
#'  interval span
#' @param nc Number of columns
#' @param mean The original means, either matrix or vector
#' @return \code{list} Returns a list with axis_vp, axisGrob, labGrob, zero and clip
#'
#'
#' @inheritParams forestplot2
#' @keywords internal
prFpGetGraphTicksAndClips <- function(xticks,
                                      xticks.digits,
                                      xlog,
                                      xlab,
                                      lwd.xaxis,
                                      col,
                                      cex,
                                      cex.axis,
                                      clip,
                                      zero,
                                      x_range,
                                      nc,
                                      mean){

  # Active rows are all excluding the top ones with NA in the mean value
  if (is.matrix(mean)){
    for (from in 1:nrow(mean))
      if (!all(is.na(mean[from, ])))
        break;
    to <- nrow(mean)
  }else{
    for (from in 1:length(mean))
      if (!is.na(mean[from]))
        break;
    to <- length(mean)
  }

  if (xlog) {
    clip[clip < 0] <- 0
    clip <- log(clip)
    zero <- log(zero)

    if (missing(xticks)) {
      ticks <- getTicks(exp(x_range),
                        clip=clip,
                        exp=xlog,
                        digits=xticks.digits)

      # Add the endpoint ticks to the tick list if
      # it's not already there
      if (is.infinite(clip[1]) == FALSE &&
            min(ticks, na.rm = TRUE) < clip[1])
        ticks <- unique(c(exp(clip[1]), ticks))

      if (is.infinite(clip[2]) == FALSE &&
            max(ticks, na.rm = TRUE) > clip[2])
        ticks <- unique(c(ticks, exp(clip[2])))

      # Update the range so that it includes the ticks
      if (min(x_range) > log(min(ticks)))
        x_range[which.min(x_range)] <- log(min(ticks))
      if (max(x_range) < max(ticks))
        x_range[which.max(x_range)] <- log(max(ticks))

    } else {
      ticks <- xticks
    }

    axis_vp <- viewport(layout.pos.col = 2 * nc + 1,
                        layout.pos.row = from:to,
                        xscale         = x_range,
                        name           = "axis")



    # Draw the x-axis if there are any ticks
    if (length(ticks)) {

      # Decide on the number of digits, if below zero then there should
      # be by default one more digit
      ticklabels <- ifelse(ticks < 1 | abs(floor(ticks*10)-ticks*10) > 0,
                           format(ticks, digits = 2, nsmall = 2),
                           format(ticks, digits = 1, nsmall = 1))
      ticks <- log(ticks)
    }else{
      ticks <- NULL
      ticklabels <- FALSE
    }


  } else {
    if (missing(xticks)){
      ticks <- getTicks(x_range,
                        clip=clip,
                        exp=xlog,
                        digits=xticks.digits)

      # Add the endpoint ticks to the tick list if
      # it's not already there
      if (is.infinite(clip[1]) == FALSE &&
            min(ticks, na.rm = TRUE) < clip[1])
        ticks <- unique(c(clip[1], ticks))

      if (is.infinite(clip[2]) == FALSE &&
            max(ticks, na.rm = TRUE) > clip[2])
        ticks <- unique(c(ticks, clip[2]))

      ticklabels <- TRUE

      # Update the range so that it includes the ticks
      if (min(x_range) > min(ticks))
        x_range[which.min(x_range)] <- min(ticks)
      if (max(x_range) < max(ticks))
        x_range[which.max(x_range)] <- max(ticks)

    } else{
      ticks <- xticks
      ticklabels <- TRUE
    }

    axis_vp <- viewport(layout.pos.col = 2 * nc + 1,
                        layout.pos.row = from:to,
                        xscale         = x_range,
                        name           = "axis")

  }

  if (length(ticks) != 1 || ticks != 0){
    gp_list <- list(cex = cex.axis,
                    col = col$axes)
    if(!missing(lwd.xaxis))
      gp_list$lwd <- lwd.xaxis

    dg <- xaxisGrob(at    = ticks,
                    label = ticklabels,
                    gp    = fastDoCall(gpar, gp_list))
  }else{
    dg <- FALSE
  }

  if (length(xlab) == 1 && nchar(xlab) > 0){
    # Write the label for the x-axis
    labGrob <- textGrob(xlab,
                        gp = gpar(col = col$axes, cex=cex))

  }else{
    labGrob <- FALSE
  }


  return(list(axis_vp = axis_vp,
              axisGrob = dg,
              labGrob = labGrob,
              zero = zero,
              clip = clip,
              x_range = x_range))
}

#' Plots the x-axis for forestplot2
#'
#' A helper function to the \code{\link{forestplot2}}
#' function.
#'
#' @param axisList The list from \code{\link{prFpGetGraphTicksAndClips}}
#' @return void
#'
#' @inheritParams forestplot2
#' @keywords internal
prFpPrintXaxis <- function(axisList,
                           col,
                           lwd.zero){
  # Now plot the axis inkluding the horizontal bar
  pushViewport(axisList$axis_vp)

  # Plot the vertical "zero" axis
  gp_list <- list(col = col$zero)
  if (!missing(lwd.zero))
    gp_list$lwd <- lwd.zero

  grid.lines(x  = unit(axisList$zero, "native"),
             y  = 0:1,
             gp = fastDoCall(gpar, gp_list))

  lab_y <- unit(0, "mm")
  lab_grob_height <- unit(-2, "mm")
  # Omit the axis if specified as 0
  if (is.grob(axisList$axisGrob)){
    # Plot the actual x-axis
    grid.draw(axisList$axisGrob)
    lab_grob_height <- grobHeight(axisList$axisGrob)
    lab_y <- lab_y - lab_grob_height
  }

  if (is.grob(axisList$labGrob)){
    # Add some padding between text and ticks proportional to the ticks height
    padding <-
      unit(convertY(lab_grob_height, "lines", valueOnly=TRUE)*0.1,
           "lines")

    # The text is strangely messy
    # and needs its own viewport
    pushViewport(viewport(height=grobHeight(axisList$labGrob),
                          y=lab_y - padding, just="top"))
    grid.draw(axisList$labGrob)
    upViewport()
  }
  upViewport()
}


#' Plots the labels
#'
#' This is a helper function to the \code{\link{forestplot2}}
#' function.
#'
#' @param labels A list to the labels
#' @param nc Number of columns
#' @param nr Number of rows
#' @return \code{void}
#'
#' @keywords internal
prFpPrintLabels <- function(labels, nc, nr){
  # Output the labels
  # The column
  for (j in 1:nc) {
    # The row
    for (i in 1:nr) {
      if (!is.null(labels[[j]][[i]])) {
        # The column position is 2 * j - 1 due to the column gap
        vp <- viewport(layout.pos.row = i,
                       layout.pos.col = 2 * j - 1,
                       name           = sprintf("Label_vp_%d_%d", i, 2*j-1))
        pushViewport(vp)
        grid.draw(labels[[j]][[i]])
        upViewport()
      }
    }
  }
}

#' Gets the forestplot legend grobs
#'
#' @return \code{list} A "Legend" class that derives from a
#'  list with all the different legends. The list also contains
#'  attributes such as height, width, max_height,
#'  max_width, line_height_and_spacing. The title of the
#'  legend is saved inside \code{attr("title")}
#'
#' @inheritParams forestplot2
#' @inheritParams fpLegend
#' @keywords internal
prFpGetLegendGrobs <- function(legend,
                               cex,
                               title){
  lGrobs <- list()
  max_width <- 0
  max_height <- 0
  for (n in 1:length(legend)){
    lGrobs[[n]] <- textGrob(legend[n], x=0, just="left",
                            gp=gpar(cex=cex))

    gw <- convertUnit(grobWidth(lGrobs[[n]]), "mm", valueOnly=TRUE)
    gh <- convertUnit(grobHeight(lGrobs[[n]]), "mm", valueOnly=TRUE)
    if (gw > max_width)
      max_width <- gw
    if (gh > max_height)
      max_height <- gh

    attr(lGrobs[[n]], "width") <- unit(gw, "mm")
    attr(lGrobs[[n]], "height") <- unit(gh, "mm")
  }
  attr(lGrobs, "max_height") <- unit(max_height, "mm")
  attr(lGrobs, "max_width") <- unit(max_width, "mm")
  attr(lGrobs, "line_height_and_spacing") <- unit.c(attr(lGrobs, "max_height"),
      unit(.5, "lines"))

  # Do title stuff if present
  if (is.character(title)){
    title <- textGrob(title, x=0, just="left",
        gp=gpar(fontface = "bold",
        cex = cex*1.1))
    attr(lGrobs, "title") <- title

    attr(lGrobs, "titleHeight") <- grobHeight(title)
    attr(lGrobs, "titleWidth") <- grobHeight(title)
    if (convertUnit(attr(lGrobs, "titleWidth"), unitTo="npc", valueOnly=TRUE) >
      convertUnit(attr(lGrobs, "max_width"), unitTo="npc", valueOnly=TRUE))
      attr(lGrobs, "max_width") <- attr(lGrobs, "titleWidth")
  }
  class(lGrobs) <- c("Legend", class(lGrobs))
  return(lGrobs)
}

#' Draw the forestplot legend
#'
#' Takes the grobs and outputs the legend
#' inside the current viewport.
#'
#' @param lGrobs A list with all the grobs, see \code{\link{prFpGetLegendGrobs}}
#' @param col The colors of the legends.
#' @param colgap The gap between the box and the text
#' @param legendMarkerFn The function for drawing the marker
#' @param ... Passed to the legend \code{legendMarkerFn}
#' @return \code{void}
#'
#' @inheritParams forestplot2
#' @inheritParams fpLegend
#'
#' @keywords internal
prFpDrawLegend <- function (lGrobs,
                            col,
                            colgap,
                            pos,
                            gp,
                            r,
                            padding,
                            legendMarkerFn,
                            ...) {
  if (!inherits(lGrobs, "Legend"))
    stop("The lGrobs object should be created by the internal Gmisc:::prFpGetLegendGrobs and be of class 'Legend'.")

  # Draw the rounded rectangle at first
  # if there is a gpar specified.
  if (length(gp) > 0){
    grid.roundrect(gp = gp, r=r)
    inner_vp <- viewport(width=unit(1, "npc") - padding - padding,
      height=unit(1, "npc") - padding - padding)
    pushViewport(inner_vp)
  }
  legend_width <- 0
  legend_height <- 0
  if (!is.list(pos) && pos == "top" ||
    is.list(pos) && "align" %in% names(pos) && pos[["align"]] == "horizontal"){
    orientation <- "horizontal"
  }else{
    orientation <- "vertical"
  }

  boxSize <- attr(lGrobs, "max_height")

  drawBox <- function(vp, i, col, lGrobs){
    pushViewport(vp)

    call_list <-
      list(legendMarkerFn[[i]],
           lower_limit=0,
           estimate=.5,
           upper_limit=1,
           size=attr(lGrobs, "max_height"),
           y.offset = .5,
           clr.marker = col$box[i],
           clr.line = col$lines[i],
           lwd=1,
           ... = ...)

    # Do the actual drawing of the object
    eval(as.call(call_list))

    upViewport()
  }

  if (orientation == "horizontal"){
    # Output the horizontal boxes and texts
    widths <- NULL
    for (n in 1:length(lGrobs)){
      if (length(widths) == 0)
        widths <- unit.c(boxSize, colgap, attr(lGrobs[[n]], "width"))
      else
        widths <- unit.c(widths, colgap, boxSize, colgap, attr(lGrobs[[n]], "width"))
    }
    heights <- attr(lGrobs, "max_height")
    # Add title height if any
    if (!is.null(attr(lGrobs, "title"))) heights <- unit.c(attr(lGrobs, "titleHeight"),
          attr(lGrobs, "line_height_and_spacing")[2],
          heights)

    l_layout <- grid.layout(nrow=length(heights),
                            heights = heights,
                            ncol=length(widths),
                            widths=widths)
    lvp <- viewport(layout = l_layout,
                    name = "legend_details")
    pushViewport(lvp)
    row <- 1
    # Output title
    if (!is.null(attr(lGrobs, "title"))){
      vp <- viewport(layout.pos.row = 1)
      pushViewport(vp)
      pushViewport(viewport(width=attr(lGrobs, "titleWidth")))
      grid.draw(attr(lGrobs, "title"))
      upViewport(2)
      row <- 3
    }
    for (i in 1:length(lGrobs)){
      offset <- 4*(i-1)
      vp <- viewport(layout.pos.row = row,
                     layout.pos.col = 1 + offset,
                     xscale=c(0, 1))
      drawBox(vp, i, col, lGrobs)
      vp <- viewport(layout.pos.row = row,
                     layout.pos.col = 3 + offset)
      pushViewport(vp)
      grid.draw(lGrobs[[i]])
      upViewport()
    }
    upViewport()

  }else{
    # Output the vertical boxes and texts
    widths <- unit.c(boxSize, colgap, attr(lGrobs, "max_width"))

    # Remove bottom line
    heights <- attr(lGrobs, "line_height_and_spacing")[rep(1:2, length.out=length(lGrobs)*2-1)]
    #heights <- unit(convertUnit(heights, unitTo="npc", valueOnly=TRUE)/sum(convertUnit(heights, unitTo="npc", valueOnly=TRUE), "npc")
    # Add title height if any
    if (!is.null(attr(lGrobs, "title"))) heights <- unit.c(attr(lGrobs, "titleHeight"),
        attr(lGrobs, "line_height_and_spacing")[2],
        heights)

    l_layout <- grid.layout(ncol=length(widths),
                            nrow=length(heights),
                            widths=widths,
                            heights=heights)

    lvp <- viewport(layout = l_layout, just="left", x=0,
                    name="legend")
    pushViewport(lvp)
    row_start <- 1
    # Output title
    if (!is.null(attr(lGrobs, "title"))){
      vp <- viewport(layout.pos.row = 1)
      pushViewport(vp)
      grid.draw(attr(lGrobs, "title"))
      upViewport()
      row_start <- 3
    }

    for (i in 1:length(lGrobs)){
      vp <- viewport(layout.pos.row = row_start + (i-1)*2,
                     layout.pos.col = 1,
                     xscale=c(0,1))
      drawBox(vp, i, col, lGrobs)

      vp <- viewport(layout.pos.row = row_start + (i-1)*2,
                     layout.pos.col = 3)
      pushViewport(vp)
      grid.draw(lGrobs[[i]])
      upViewport()
    }
    upViewport()
  }

  if (length(gp) > 0){
    upViewport()
  }
}


#' Gets the x-axis range
#'
#' If the borders are smaller than the upper/lower limits
#' then clip the graph. The line will have arrows indicating
#' that it continues beyond the graph The zero bar has to
#' be on the chart though!
#'
#' @return \code{vector} Contains a min and max value
#' @inheritParams forestplot2
#'
#' @keywords internal
prFpXrange <- function(upper, lower, clip, zero, xticks, xlog){
  top <- min(max(upper, na.rm = TRUE), clip[2])
  bottom <- max(min(lower, na.rm = TRUE), clip[1])
  # Although perhops not entirely intuitive
  # I've decided that the function should
  # extend the range to include the clip
  # endpoints unless there are prespecified
  # ticks indicating that the end-points aren't
  # included in the x-axis
  if (missing(xticks)){
    ret <- c(
        min(
            zero,
            bottom
        ),
        max(
            zero,
            top
        )
    )

  }else{
    ret <- c(
        min(
            c(zero, bottom, xticks)
        ),
        max(
            c(zero, top, xticks)
        )
    )
  }

  if (xlog){
    return(log(ret))
  }else{
    return(ret)
  }
}

#' Gets the forestplot labels
#'
#' A function that gets all the labels
#'
#' @param label_type The type of text labels
#' @param align Alignment, should be equal to \code{length(nc}
#' @param nc Number of columns
#' @param nr Number of rows
#' @return \code{list} A list with \code{length(nc)} where each element contains
#'  a list of \code{length(nr)} elements with attributes width/height for each
#'  element and max_width/max_height for the total
#'
#' @inheritParams forestplot2
#' @keywords internal
prFpGetLabels <- function(label_type, labeltext, align,
                          nc, nr,
                          is.summary,
                          fontfamily.summary, fontfamily.labelrow,
                          col, cex){
  labels <- vector("list", nc)

  max_height <- NULL
  max_width <- NULL
  # Walk through the labeltext
  # Creates a list matrix with
  # The column part
  for (j in 1:nc) {
    labels[[j]] <- vector("list", nr)

    # The row part
    for (i in 1:nr) {
      txt_out <- prFpFetchRowLabel(label_type, labeltext, i, j)
      # If it's a call created by bquote or similar it
      # needs evaluating
      if (is.call(txt_out))
        txt_out <- eval(txt_out)

      if (is.expression(txt_out) || is.character(txt_out) || is.numeric(txt_out)){
        x <- switch(align[j], l = 0, r = 1, c = 0.5)

        just <- switch(align[j],
          l = "left",
          r = "right",
          c = "center")

        # Bold the text if this is a summary
        if (is.summary[i]){
          if (is.expression(txt_out)){
            x <- 0.5
          }else{
            x <- switch(align[j], l = 0, r = 1, c = 0.5)
          }

          gp_list <- list(fontface = "bold",
                          cex = cex*1.1,
                          col = rep(col$text, length = nr)[i])
          if (!missing(fontfamily.summary))
            gp_list$fontfamily <- fontfamily.summary

          # Create a textGrob for the summary
          labels[[j]][[i]] <-
            textGrob(txt_out, x = x,
                     just = just,
                     gp = fastDoCall(gpar, gp_list))
        }else{
          gp_list <- list(fontface = "plain",
                          cex = cex,
                          col = rep(col$text, length = nr)[i])
          if (!missing(fontfamily.labelrow))
            gp_list$fontfamily <- fontfamily.labelrow

          # Create a textGrob with the current row-cell for the label
          labels[[j]][[i]] <-
            textGrob(txt_out, x = x,
                     just = just,
                     gp = fastDoCall(gpar, gp_list))
        }

        attr(labels[[j]][[i]], "height") <- grobHeight(labels[[j]][[i]])
        attr(labels[[j]][[i]], "width") <- grobWidth(labels[[j]][[i]])
        if (is.null(max_height)){
          max_height <- attr(labels[[j]][[i]], "height")
          max_width <- attr(labels[[j]][[i]], "width")
        }else{
          max_height <- max(max_height, attr(labels[[j]][[i]], "height"))
          max_width <- max(max_width, attr(labels[[j]][[i]], "width"))
        }
      }
    }
  }
  attr(labels, "max_height") <- max_height
  attr(labels, "max_width") <- max_width
  attr(labels, "cex") <- ifelse(any(is.summary), cex*1.1, cex)
  return(labels)
}

#' Get the label
#'
#' A function used for fetching the text or
#' expression from the supplied labeltext.
#'
#' @param label_type The type of label
#' @param i The row
#' @param j The column
#' @return An expression or a text
#'
#' @inheritParams forestplot2
#' @keywords internal
prFpFetchRowLabel <- function(label_type, labeltext, i, j){
  if (label_type=="expression"){
    # Haven't figured out it this is possible with
    # a multilevel expression
    row_column_text <- labeltext[[i]]
  }
  else if(label_type=="list"){
    # I get annoying warnings with this
    #if (!is.expression(labeltext[[j]][[i]]) && is.na(labeltext[[j]][[i]]))
    #    return(FALSE)
    row_column_text <- labeltext[[j]][[i]]
  }
  else{
    if (is.na(labeltext[i, j]))
      return(FALSE)
    row_column_text <- labeltext[i, j]
  }
  return(row_column_text)
}

#' Get the main foresplot
#'
#' The layout makes space for a legend if needed
#'
#' @param labels The labels
#' @param nr Number of rows
#' @param legend_layout A legend layout object if applicable
#' @return \code{viewport} Returns the viewport needed
#'
#' @inheritParams forestplot2
#' @keywords internal
prFpGetLayoutVP <- function (lineheight, labels, nr, legend_layout = NULL) {
  if (!is.unit(lineheight)){
    if (lineheight == "auto"){
      lvp_height <- unit(1, "npc")
    }else if (lineheight == "lines"){
      lvp_height <- unit(nr*attr(labels, "cex")*1.5, "lines")
    }else{
      stop("The lineheight option '", lineheight, "'is yet not implemented")
    }
  }else{
    lvp_height <- unit(convertY(lineheight,
                                unitTo="lines",
                                valueOnly=TRUE)*nr,
                       "lines")
  }

  # If there is a legend on top then the size should be adjusted
  if (!is.null(legend_layout) &&
        legend_layout$nrow == 3 &&
        convertY(lvp_height, "npc", valueOnly=TRUE) < 1){
    lvp_height <- sum(lvp_height, legend_layout$heights[1:2])
  }

  lvp <- viewport(height=lvp_height,
                  layout = legend_layout,
                  name = ifelse(is.null(legend_layout), "main", "main_and_legend"))
  return (lvp)
}

#' Validate the forestplot label list
#'
#' Checks that all list elements have equal
#' length, i.e. there is a m x n relation
#'
#' @param labelList The list of labels
#' @return \code{boolean} TRUE or FALSE
#'
#'
#' @keywords internal
prFpValidateLabelList <- function(labelList){
  l = length(labelList[[1]])
  if (length(labelList) == 1)
    return(TRUE)

  for(i in 2:length(labelList)){
    # All elements should have the same length
    if (l != length(labelList[[i]]))
      return(FALSE)
  }

  return(TRUE)
}

#' Finds the widest grob in the current list of grobs
#'
#' @param grob.list A list of grobs
#' @param return_unit A valid \code{\link[grid]{unit}} specifier
#' @return \code{grid::unit} Returns the width \code{\link[grid]{unit}}
#'  for the widest grob
#' @keywords internal
prFpFindWidestGrob <- function (grob.list, return_unit="mm"){
  len <- c()
  for (i in seq(along.with=grob.list)){
    if (is.object(grob.list[[i]])){
      # There is a tendency of underestemating grob size
      # when there are expressions
      grob_width <- convertWidth(grobWidth(grob.list[[i]]), return_unit, valueOnly=TRUE)
      len <- append(len, grob_width)
    }else{
      len <- append(len, 0)
    }
  }

  return(unit(max(len), return_unit))
}

#' Converts legend position to a standard position
#'
#' Used for the forestplot legend box.
#'
#' @return \code{list} Returns the \code{pos} list with
#'  the correct x/y/adjust values
#'
#' @inheritParams fpLegend
#' @keywords internal
prFpGetLegendBoxPosition <- function (pos) {
  valid_txt_pos <- c("bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center")
  if (!all(c("x", "y") %in% names(pos)) &&
        !(("x" %in% pos &&
             any(pos[["x"]] == valid_txt_pos)) ||
            any(pos[[1]] == valid_txt_pos)))
    stop("If you want to specify the legend position in a certain corner",
         " within the main plot then you need to have list names x and y specified,",
         " or you should have the first list element to be '", paste(valid_txt_pos, collapse="'/'"), "',",
         " if you don't specify the first element then it can be the 'x' element")

  # Convert to the x & y format to make things easier
  if (!all(c("x", "y") %in% names(pos))){
    if ("x" %in% names(pos))
      txt_pos <- pos[["x"]]
    else
      txt_pos <- pos[[1]]

    # The inset offsets the position
    if (!"inset" %in% names(pos)){
      pos[["inset"]] <- unit(0, "npc")
    }else if (!is.unit(pos[["inset"]])){
      if (pos[["inset"]] > 1 || pos[["inset"]] < 0)
        stop("If you have not specified the unit of the pos inset then it should be between 0 and 1")
      pos[["inset"]] <- unit(pos[["inset"]], "npc")
    }else{
      if (convertUnit(pos[["inset"]], unitTo="npc", valueOnly=TRUE) > 1)
        stop("You have provided a value outside the possible range ('npc' bigger than 1)")
    }

    if (txt_pos == "bottomright"){
      pos[["x"]] <- unit(1, "npc") - pos[["inset"]]
      pos[["y"]] <- unit(0, "npc") + pos[["inset"]]
      pos[["just"]] <- c("right", "bottom")
    }else if(txt_pos == "bottom"){
      pos[["x"]] <- unit(0.5, "npc")
      pos[["y"]] <- unit(0, "npc") + pos[["inset"]]
      pos[["just"]] <- c("center", "bottom")
    }else if (txt_pos == "bottomleft"){
      pos[["x"]] <- unit(0, "npc") + pos[["inset"]]
      pos[["y"]] <- unit(0, "npc") + pos[["inset"]]
      pos[["just"]] <- c("left", "bottom")
    }else if (txt_pos == "left"){
      pos[["x"]] <- unit(0, "npc") + pos[["inset"]]
      pos[["y"]] <- unit(.5, "npc")
      pos[["just"]] <- c("left", "center")
    }else if (txt_pos == "topleft"){
      pos[["x"]] <- unit(0, "npc") + pos[["inset"]]
      pos[["y"]] <- unit(1, "npc") - pos[["inset"]]
      pos[["just"]] <- c("left", "top")
    }else if (txt_pos == "top"){
      pos[["x"]] <- unit(0.5, "npc")
      pos[["y"]] <- unit(1, "npc") - pos[["inset"]]
      pos[["just"]] <- c("center", "top")
    }else if (txt_pos == "topright"){
      pos[["x"]] <- unit(1, "npc") - pos[["inset"]]
      pos[["y"]] <- unit(1, "npc") - pos[["inset"]]
      pos[["just"]] <- c("right", "top")
    }else if (txt_pos == "right"){
      pos[["x"]] <- unit(1, "npc") - pos[["inset"]]
      pos[["y"]] <- unit(.5, "npc")
      pos[["just"]] <- c("right", "center")
    }else if (txt_pos == "center" || txt_pos == "centre"){
      pos[["x"]] <- unit(.5, "npc")
      pos[["y"]] <- unit(.5, "npc")
      pos[["just"]] <- c("center", "center")
    }else{
      stop("Position '", pos[["x"]], "'not yet implemented")
    }
  }else if(!"just" %in% names(pos)){
    pos[["just"]] <- c("center", "center")
  }
  return (pos)
}

#' Prepares the legend marker function
#'
#' @param legendMarkerFn The unknown parameter
#' @param col_no The number of columns
#' @param confintNormalFn The original confintNormalFn input
#' @return \code{list}
#'
#' @keywords internal
prFpPrepareLegendMarker <- function (legendMarkerFn, col_no, confintNormalFn) {
  if (!missing(legendMarkerFn)){
    if (is.function(legendMarkerFn)){
      return(lapply(1:col_no, function(x) legendMarkerFn))
    }
    if(is.character(legendMarkerFn)){
      if (length(legendMarkerFn) == 1){
        legendMarkerFn <- rep(legendMarkerFn, times=col_no)
      }else if (length(legendMarkerFn) != col_no){
        stop("The number of legend markers, ", length(legendMarkerFn),
             ", should be the same as the number of columns for the mean, ", col_no)
      }

      tmp <- list()
      for (i in 1:length(legendMarkerFn)){
        tmp[[i]] <- get(legendMarkerFn[i])
      }

      return(tmp)
    }

    if(is.list(legendMarkerFn)){
      if(length(legendMarkerFn) != col_no){
        stop("The number of legend markers, ", length(legendMarkerFn), ",",
             " should be the same as the number of columns for the mean, ", col_no)
      }else if(!all(sapply(legendMarkerFn, function(x) is.function(x)))){
        stop("If you provide a list for legendMarkerFn then each element should be a function")
      }

      return(legendMarkerFn)
    }

    stop("The legend marked function designated by the legendMarkerFn",
         " is neither a character or a function")
  }

  if (length(confintNormalFn) == col_no){
    return(prFpGetConfintFnList(fn = confintNormalFn,
                                no_rows = NROW(mean),
                                no_cols = col_no)[[1]])
  }

  # Not sure what to do if the number don't match the number of legends
  # and it ain't 1 and it therefore defaults to the normal confidence
  # interval marker
  if (length(confintNormalFn) != 1)
    confintNormalFn <- fpDrawNormalCI

  return(lapply(1:col_no, function(x) confintNormalFn))
}

#' Converts a 2D or 3D array to mean, lower, upper
#'
#' @param x The array to convert
#' @return \code{list(mean = mean, lower = lower, upper = upper)}
#' @keywords internal
prFpConvertMultidimArray <- function(x){
  switch(as.character(length(dim(x))),
         "2" = {
           # Loop through the different rows as a row with only a label may have NA in it
           lower_cnr <- NULL
           upper_cnr <- NULL
           for (d1 in dim(x)[1]){
             if (length(unique(x[d1,])) < 3)
               next;

             lower_cnr <- which.min(x[d1,])
             upper_cnr <- which.max(x[d1,])
             if (length(lower_cnr) == 1 &&
                   length(upper_cnr) == 1){
               break;
             }
           }
           if (length(lower_cnr) != 1 ||
                 length(upper_cnr) != 1)
             stop("Sorry did not manage to automatically identify",
                  " the upper/lower boundaries.")

           lower <- x[,lower_cnr,drop=TRUE]
           upper <- x[,upper_cnr,drop=TRUE]
           mean <- x[,-c(upper_cnr, lower_cnr),drop=TRUE]},
         "3" = {
           # Loop through the different rows as a row with only a label may have NA in it
           # this is a little complicated as we're doing a 3D loop and exiting
           # as soon as the vars have been identified
           lower_cnr <- NULL
           upper_cnr <- NULL
           for (d3 in 1:dim(x)[3]){
             for (d1 in 1:dim(x)[1]){
               if (length(unique(x[d1,,d3])) < 3)
                 next;

               lower_cnr <- which.min(x[d1,,d3])
               upper_cnr <- which.max(x[d1,,d3])

               if (length(lower_cnr) == 1 &&
                     length(upper_cnr) == 1)
                 break;
             }
             if (length(lower_cnr) == 1 &&
                   length(upper_cnr) == 1)
               break;

           }
           if (length(lower_cnr) != 1 ||
                 length(upper_cnr) != 1)
             stop("Sorry did not manage to automatically identify",
                  " the upper/lower boundaries.")
           lower_cnr <- which.min(x[1,,1])
           upper_cnr <- which.max(x[1,,1])
           lower <- x[,lower_cnr,,drop=TRUE]
           upper <- x[,upper_cnr,,drop=TRUE]
           mean <- x[,-c(upper_cnr, lower_cnr),,drop=TRUE]},{
             stop("Invalid number of dimensions of the mean argument,",
                  " should be either 2 or 3 - you have '", length(dim(mean)),"'")
           })
  return(list(mean = mean, lower = lower, upper = upper))
}