#' A copy of rmeta meta.colors. 
#' 
#' If you have several values per row in a
#' plot then you can set the values to a vector where the first value 
#' represents the first line/box, second the second line/box etc. The 
#' vectors are only valid for the box & lines. 
#' 
#' This function is a copy of the meta.colors function in the
#' rmeta package.
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
#' \item{box}{the color of the box}
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
      
    return(list(box = box, lines = lines, summary = summary, 
                zero = zero, text = text, axes = axes))
  }
  
  if (is.null(all.elements)) 
    all.elements <- par("fg")
  
  return(list(box = all.elements, lines = all.elements, summary = all.elements, 
              zero = all.elements, text = all.elements, 
              axes = all.elements))
}

#' A helper function to forestplot2
#' 
#' Gets the x-label and zero-bar details
#' 
#' @param xticks The xticks 
#' @param xticks.digits Number of digits for the xticks
#' @param xlog If the axis should be log()
#' @param xlab The x-axis label
#' @param lwd.xaxis The line width of the x-axis
#' @param col The color object
#' @param cex The text size
#' @param clip The clip margins
#' @param zero The zero effect
#' @param x_range The range that values span
#' @param nc Number of columns
#' @return list Returns a list with axis_vp, axisGrob, labGrob, zero and clip
#' 
#' @author Max
prFpGetGraphTicksAndClips <- function(xticks, 
                                      xticks.digits, 
                                      xlog, 
                                      xlab, 
                                      lwd.xaxis, 
                                      col,
                                      cex,
                                      clip, 
                                      zero, 
                                      x_range, 
                                      nc){
  if (xlog) {
    clip[clip < 0] <- 0
    clip <- log(clip)
    zero <- log(zero)
    axis_vp <- viewport(layout.pos.col = 2 * nc + 1, 
                        xscale         = x_range,
                        name           = "axis")
    
    if (is.null(xticks)) {
      ticks <- getTicks(exp(x_range), clip=clip, exp=TRUE, digits=xticks.digits)
      
      # Add the endpoint ticks to the tick list if 
      # it's not already there
      if (is.infinite(clip[1]) == FALSE &&
            min(ticks, na.rm = TRUE) < clip[1]) 
        ticks <- unique(c(exp(clip[1]), ticks))
      
      if (is.infinite(clip[2]) == FALSE &&
            max(ticks, na.rm = TRUE) > clip[2]) 
        ticks <- unique(c(ticks, exp(clip[2])))
    } else {
      ticks <- xticks
    }
    
    # Draw the x-axis if there are any ticks
    if (length(ticks)) {
      
      # Decide on the number of digits, if below zero then there should
      # be by default one more digit
      ticklabels <- ifelse(ticks < 1 | abs(floor(ticks*10)-ticks*10) > 0, 
                           formatC(ticks, digits = 2, format="f", drop0trailing=FALSE), 
                           format(ticks, digits = 1, drop0trailing=TRUE))
      xticks <- log(ticks)
    }else{
      xticks <- NULL
      ticklabels <- FALSE
    }
    
    
  } else {
    axis_vp <- viewport(layout.pos.col = 2 * nc + 1, 
                        xscale         = x_range,
                        name           = "axis")
    
    if (is.null(xticks)){
      ticks <- getTicks(exp(x_range), clip=clip, digits=xticks.digits)
      
      # Add the endpoint ticks to the tick list if 
      # it's not already there
      if (is.infinite(clip[1]) == FALSE &&
            min(ticks, na.rm = TRUE) < clip[1]) 
        ticks <- unique(c(exp(clip[1]), ticks))
      
      if (is.infinite(clip[2]) == FALSE &&
            max(ticks, na.rm = TRUE) > clip[2]) 
        ticks <- unique(c(ticks, exp(clip[2])))
      
      ticklabels <- TRUE
      
    } else{
      ticklabels <- TRUE
    }
    
  }
  
  if (length(xticks) != 1 || xticks != 0){
    dg <- xaxisGrob(at    = xticks, 
                    label = ticklabels,
                    gp    = gpar(cex = cex*0.6, col = col$axes, lwd=lwd.xaxis))
    dg_height <- convertUnit(grobHeight(textGrob("I", gp=gpar(cex=cex*0.6))), "npc") + unit(1, "lines")
  }else{
    dg <- FALSE
    dg_height <- 0 
  }

  if (length(xlab) == 1 && nchar(xlab) > 0){
    # Write the label for the x-axis
    labGrob <- textGrob(xlab, 
                        y = unit(-2, "lines"), 
                        gp = gpar(col = col$axes, cex=cex))
    
  }else
    labGrob <- FALSE
  
  return(list(axis_vp = axis_vp,
              axisGrob = dg,
              axisHeight = dg_height,
              labGrob = labGrob,
              zero = zero,
              clip = clip))
}

#' Plots the x-axis for forestplot2
#' 
#' A helper function to the \code{\link{forestplot2}}
#' function.
#' 
#' @param axisList The list from \code{\link{prFpGetGraphTicksAndClips}} 
#' @param col The colors list
#' @param lwd.zero The zero line's line width
#' @return void 
#' 
#' @author Max
prFpPrintXaxis <- function(axisList, 
                           col, 
                           lwd.zero){
  # Now plot the axis inkluding the horizontal bar
  pushViewport(axisList$axis_vp)
  
  # Plot the vertical "zero" axis
  grid.lines(x  = unit(axisList$zero, "native"), 
             y  = 0:1, 
             gp = gpar(col = col$zero, lwd=lwd.zero))
  
  # Omit the axis if specified as 0
  if (is.grob(axisList$axisGrob)){
    # Plot the actual x-axis
    grid.draw(axisList$axisGrob)
  }
  
  if (is.grob(axisList$labGrob)){
    grid.draw(axisList$labGrob)
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
#' @return void
#' 
#' @author Max
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

prFpGetLegendGrobs <- function(legend, legend.cex){
  lGrobs <- list()
  max_width <- 0
  max_height <- 0
  for (n in 1:length(legend)){
    lGrobs[[n]] <- textGrob(legend[n], x=0, just="left", 
                            gp=gpar(cex=legend.cex))
    
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
  class(lGrobs) <- c("Legend", class(lGrobs))
  return(lGrobs)
}

prFpDrawLegend <- function (lGrobs, legend.pos, 
                            col, 
                            colgap) {
  legend_width <- 0
  legend_height <- 0

  if (!inherits(lGrobs, "Legend"))
    stop("The lGrobs object should be created by the internal Gmisc:::prFpGetLegendGrobs and be of class legend.")

  boxSize <- attr(lGrobs, "max_height")

  drawBox <- function(vp, i, col, lGrobs){
    pushViewport(vp)
    grid.rect(gp=gpar(fill = col$box[i], 
                      col = col$lines[i]),
              width=attr(lGrobs, "max_height"),
              height=attr(lGrobs, "max_height"))
    upViewport()
  }
  if (legend.pos == "top"){
    widths <- NULL
    for (n in 1:length(lGrobs)){
      if (length(widths) == 0)
        widths <- unit.c(boxSize, colgap, attr(lGrobs[[n]], "width"))
      else
        widths <- unit.c(widths, colgap, boxSize, colgap, attr(lGrobs[[n]], "width"))
    }
    
    l_layout <- grid.layout(nrow=1, 
                            ncol=length(widths), 
                            widths=widths)
    lvp <- viewport(layout = l_layout,
                    name = "legend_details")
    pushViewport(lvp)
    for (i in 1:length(lGrobs)){
      offset <- 4*(i-1)
      vp <- viewport(layout.pos.row = 1, 
                     layout.pos.col = 1 + offset)
      drawBox(vp, i, col, lGrobs)
      vp <- viewport(layout.pos.row = 1, 
                     layout.pos.col = 3 + offset)
      pushViewport(vp)
      grid.draw(lGrobs[[i]])
      upViewport()
    }
    upViewport()
    
  }else{
    widths <- unit.c(boxSize, colgap, attr(lGrobs, "max_width"))
    line_and_adj_height <- convertUnit(unit.c(boxSize, 
                                              unit(.5, "lines")), 
                                       "npc", valueOnly=TRUE)
    
    heights <- unit(rep(line_and_adj_height,
                        times=length(lGrobs)), "npc")
    
    l_layout <- grid.layout(ncol=length(widths), 
                            nrow=length(heights), 
                            widths=widths,
                            heights=heights)
    
    lvp <- viewport(layout = l_layout, just="left", x=0,
                    name="legend")
    pushViewport(lvp)
    for (i in 1:length(lGrobs)){
      vp <- viewport(layout.pos.row = 1 + (i-1)*2, 
                     layout.pos.col = 1)
      drawBox(vp, i, col, lGrobs)
      
      vp <- viewport(layout.pos.row = 1 + (i-1)*2, 
                     layout.pos.col = 3)
      pushViewport(vp)
      grid.draw(lGrobs[[i]])
      upViewport()
    }
    upViewport()
  }
}
