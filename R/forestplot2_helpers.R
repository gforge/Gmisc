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
  attr(lGrobs, "line_height_and_spacing") <- unit.c(attr(lGrobs, "max_height"), 
      unit(.5, "lines"))
  class(lGrobs) <- c("Legend", class(lGrobs))
  return(lGrobs)
}

prFpDrawLegend <- function (lGrobs, legend.pos, 
                            col, 
                            colgap) {
  legend_width <- 0
  legend_height <- 0
  if (!is.list(legend.pos) && legend.pos == "top" ||
    is.list(legend.pos) && "align" %in% names(legend.pos) && legend.pos[["align"]] == "horizontal"){
    orientation <- "horizontal"
  }else{
    orientation <- "vertical"
  }

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
  
  if (orientation == "horizontal"){
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
    line_and_adj_height <- attr(lGrobs, "line_height_and_spacing") 
    
    # Remove bottom line
    heights <- rep(convertUnit(line_and_adj_height, unitTo="npc", valueOnly=TRUE),
      times=length(lGrobs))[1:(length(lGrobs)*2-1)]
    heights <- unit(heights/sum(heights), "npc")
    
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


#' Gets the x-axis range
#' 
#' If the borders are smaller than the upper/lower limits  
#' then clip the graph. The line will have arrows indicating  
#' that it continues beyond the graph The zero bar has to 
#' be on the chart though!
#' 
#' @param upper Upper confidence intervals 
#' @param lower Lower confidence intervals
#' @param clip The clip argument
#' @param zero The zero effect line position
#' @param xticks The xticks if any
#' @param xlog A TRUE or FALSE for if the axis is log()
#' @return \code{vector} Contains a min and max value
#' 
#' @author Max
prFpXrange <- function(upper, lower, clip, zero, xticks, xlog){
  top <- min(max(upper, na.rm = TRUE), clip[2])
  bottom <- max(min(lower, na.rm = TRUE), clip[1])
  # Although perhops not entirely intuitive 
  # I've decided that the function should
  # extend the range to include the clip
  # endpoints unless there are prespecified 
  # ticks indicating that the end-points aren't
  # included in the x-axis
  if (is.null(xticks)){
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
#' @param labeltext The text labels
#' @param align Alignment, should be equal to \code{length(nc}
#' @param nc Number of columns
#' @param nr Number of rows
#' @param is.summary If the row is a summary
#' @param fontfamily.summary The summary fontfamily
#' @param fontfamily.labelrow The regular fontfamily (non-summary)
#' @param col A list with the colors
#' @param cex The font size adjustment
#' @return \code{list} A list with \code{length(nc)} where each element contains
#'  a list of \code{length(nr)} elements with attributes width/height for each
#'  element and max_width/max_height for the total
#' 
#' @author max
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
          
          # Create a textGrob for the summary
          labels[[j]][[i]] <- textGrob(txt_out, x = x,
            just = just,
            gp = gpar(fontface = "bold",
              fontfamily=fontfamily.summary,  
              cex = cex*1.1, 
              col = rep(col$text, length = nr)[i]))
        }else{
          # Create a textGrob with the current row-cell for the label
          labels[[j]][[i]] <- textGrob(txt_out, x = x,
            just = just, 
            gp = gpar(fontface = "plain",
              cex = cex,
              fontfamily=fontfamily.labelrow, 
              col = rep(col$text, length = nr)[i]))
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
  
  return(labels)
}

#' Get the label
#' 
#' A function used for fetching the text or 
#' expression from the supplied labeltext.
#' 
#' @param label_type The type of label 
#' @param labeltext The text
#' @param i The row
#' @param j The column
#' @return An expression or a text
#' 
#' @author max
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
#' @param lineheight The line height 
#' @param marList The margin list
#' @param labels The labels
#' @param nr Number of rows
#' @param legend_layout A legend layout object if applicable
#' @return \code{viewport} Returns the viewport needed 
#' 
#' @author max
prFpGetLayoutVP <- function (lineheight, marList, labels, nr, legend_layout = NULL) {
  if (!is.unit(lineheight)){
    if (lineheight == "auto"){
      lvp_height <- unit(1, "npc")-marList$bottom-marList$top
    }else if (lineheight == "lines"){
      # Use the height of a grob + 50 %
      lvp_height <- unit(convertUnit(attr(labels, "max_height"), 
          unitTo="mm", 
          valueOnly=TRUE)*(nr+.5)*1.5, "mm")
    }else{
      stop("The lineheight option '", lineheight, "'is yet not implemented")
    }
  }else{
    lvp_height <- unit(convertUnit(lineheight, unitTo="npc", valueOnly=TRUE)*(nr+.5), "npc")
  }
  
  lvp <- viewport(x = unit(.5, "npc") - marList$x_adjust, 
    y = unit(.5, "npc") - marList$y_adjust,
    width=unit(1, "npc")-marList$left-marList$right,
    height=lvp_height,
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
#' @author max
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
#' @return \code{\link[grid]{unit}}
#' 
#' @author max
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
