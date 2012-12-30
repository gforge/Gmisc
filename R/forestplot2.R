#' Create a forestplot
#' 
#' forestplot2 is based on the rmeta 2.16 forestplot function. 
#' The original rmeta package is requiered for the \code{\link{meta.colors}()} 
#' function. This function resolves some limitations of the original
#' functions such as adding expressions and using multiple confidence
#' bands for the same label.
#' 
#' Using multiple bands for the same label is judged to be interesting when 
#' one wants to compare different outcomes. 
#' 
#' @param labeltext A list, matrix, vector or expression with the names of each 
#'   row. The list should be wrapped in m x n number to resemble a matrix:
#'   \code{list(list("rowname 1 col 1", "rowname 2 col 1"), list("r1c2", expression(beta))}
#'   You can also provide a matrix although this cannot have expressions by design:
#'   \code{matrix(c("rowname 1 col 1", "rowname 2 col 1", "r1c2", "beta"), ncol=2)}
#'   Use NA:s for blank spaces and if you provide a full column with NA then
#'   that column is a empty column that adds some space.
#' @param mean A vector or a matrix with the averages
#' @param lower The lower bound of the confidence interval for the forestplot, needs 
#'   to be the same format as the mean, i.e. matrix/vector of equal columns & length
#' @param upper The upper bound of the confidence interval for the forestplot, needs 
#'   to be the same format as the mean, i.e. matrix/vector of equal columns & length
#' @param align Vector giving alignment (l,r,c) for columns of table
#' @param is.summary A vector indicating by TRUE/FALSE if the value is a summary
#'   value which means that it will have a different font-style.
#' @param fontfamily.summary The fontfamily of the summary
#' @param fontfamily.labelrow The fontfamily of a regular row
#' @param clip Lower and upper limits for clipping confidence intervals to arrows
#' @param xlab x-axis label
#' @param zero x-axis coordinate for zero line
#' @param graphwidth Width of confidence interval graph, see \code{\link{unit}} for
#'   details on how to utilize mm etc.
#' @param col See \code{\link{meta.colors}}
#' @param xlog If TRUE, x-axis tick marks are exponentiated
#' @param xticks Optional user-specified x-axis tick marks. Specify NULL to use 
#'   the defaults, numeric(0) to omit the x-axis.
#' @param xticks.digits The number of digits to allow in the x-axis if this
#'   is created by default.
#' @param lineHeight.inc If you plot multiple bars for each line you
#'   often want to increase the line height.
#' @param lwd.xaxis lwd for the xaxis
#' @param lwd.zero  lwd for the vertical line that gives the no-effect line
#' @param lwd.ci lwd for the confidence bands
#' @param cex The font adjustment
#' @param boxsize Override the default box size based on precision
#' @param ... Not used 
#' @return void
#' 
#' @import grid
#' 
#' @author Max Gordon, Thomas Lumley
#' 
#' @example examples/forestplot2_example.R
#' @export
forestplot2 <- function (labeltext, 
  mean, lower, upper, 
  align                = NULL, 
  is.summary           = FALSE,
  fontfamily.summary   = NULL, 
  fontfamily.labelrow  = NULL, 
  clip                 = c(-Inf, Inf), 
  xlab                 = "", 
  zero                 = 0, 
  graphwidth           = unit(.3, "npc"), 
  col                  = meta.colors(), 
  xlog                 = FALSE, 
  xticks               = NULL,
  xticks.digits        = 2,
  lineHeight.inc       = 1.2,
  lwd.xaxis            = NULL,
  lwd.zero             = NULL,
  lwd.ci               = NULL,
  cex                  = 1,
  boxsize              = NULL, 
  ...) 
{
  require("grid") || stop("`grid' package not found")
  
  if (NCOL(mean) != NCOL(lower) ||
      NCOL(lower) != NCOL(upper) ||
      NCOL(mean) == 0)
      stop('Mean, lower and upper contain invalid number of columns')
    
  # Save the original values since the function due to it's inheritance
  # from the original forestplot needs some changing to the parameters
  if (xlog){
    # Change all the values along the log scale
    org_mean <- log(mean)
    org_lower <- log(lower)
    org_upper <- log(upper)
  }else{
    org_mean <- mean
    org_lower <- lower
    org_upper <- upper
  }
  
  
  # For border calculations etc it's
  # convenient to have the matrix as a
  # vector
  if (NCOL(mean) > 1){
    mean <- as.vector(mean)
    lower <- as.vector(lower)
    upper <- as.vector(upper)
  }   
  
  # A function that is used to draw the different confidence intervals
  drawNormalCI <- function(LL, OR, UL, size, y.offset = 0.5, clr.line, clr.box) {
    size = 0.75 * size
    
    # Check if line/box outside graph
    clipupper <- convertX(unit(UL, "native"), "npc", valueOnly = TRUE) > 
      1
    cliplower <- convertX(unit(LL, "native"), "npc", valueOnly = TRUE) < 
      0
    box <- convertX(unit(OR, "native"), "npc", valueOnly = TRUE)
    skipbox <- box < 0 || box > 1
    
    # A version where arrows are added to the part outside 
    # the limits of the graph
    if (clipupper || cliplower) {
      ends <- "both"
      lims <- unit(c(0, 1), c("npc", "npc"))
      if (!clipupper) {
        ends <- "first"
        lims <- unit(c(0, UL), c("npc", "native"))
      }
      if (!cliplower) {
        ends <- "last"
        lims <- unit(c(LL, 1), c("native", "npc"))
      }
      grid.lines(x = lims, y = y.offset, arrow = arrow(ends = ends, 
          length = unit(0.05, "inches")), gp = gpar(col = clr.line, lwd=lwd.ci))
      if (!skipbox) 
        grid.rect(x = unit(OR, "native"), y=y.offset, width = unit(size, 
            "snpc"), height = unit(size, "snpc"), gp = gpar(fill = clr.box, 
            col = clr.box))
    } else {
      # Don't draw the line if it's no line to draw
      if (LL != UL)
        grid.lines(x = unit(c(LL, UL), "native"), y = y.offset, 
          gp = gpar(col = clr.line, lwd=lwd.ci))
      grid.rect(x = unit(OR, "native"), y=y.offset, width = unit(size, 
          "snpc"), height = unit(size, "snpc"), gp = gpar(fill = clr.box, 
          col = clr.box))
    }
  }
  drawSummaryCI <- function(LL, OR, UL, size) {
    grid.polygon(x = unit(c(LL, OR, UL, OR), "native"), y = unit(0.5 + 
          c(0, 0.5 * size, 0, -0.5 * size), "npc"), 
      gp = gpar(fill = col$summary, 
        col = col$summary))
  }
  
  # A function used for fetching the text or expression
  # from the supplied labeltext.
  fetchRowLabel <- function(label_type, labeltext, i, j){
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
  
  validateLabelList <- function(labelList){
    l = length(labelList[[1]])
    if (length(labelList) == 1)
      return(TRUE)
    
    for(i in 1:length(labelList)){
      # All elements should have the same length
      if (l != length(labelList[[i]]))
        return(FALSE)
    }
    
    return(TRUE)
  }
  
  # The previous algorithm failed when I added the expressions
  findWidestGrob <- function (grob.list){
    len <- c()
    for (i in seq(along.with=grob.list)){
      if (is.object(grob.list[[i]])){
        # There is a tendency of underestemating grob size
        # when there are expressions
        mm <- convertWidth(grobWidth(grob.list[[i]]), "mm")
        len <- append(len, mm)
      }else{
        len <- append(len, 0)
      }
    }
    
    return(unit(max(len), "mm"))
  }
  
  # Get the number of columns (nc) and number of rows (nr)
  # if any columns are to be spacers the widthcolumn variable
  if (is.expression(labeltext)){
    widthcolumn <- c(TRUE)
    # Can't figure out multiple levels of expressions
    nc <- 1
    nr <- length(labeltext)
    label_type = "expression"
  } else if (is.list(labeltext)){
    if (validateLabelList(labeltext) == FALSE)
      stop("Invalid labellist, it has to be formed as a matrix m x n elements")
    
    # Can't figure out multiple levels of expressions
    nc <- length(labeltext)
    
    widthcolumn = c()
    # Should mark the columns that don't contain
    # epressions, text or numbers as widthcolumns
    for(col.no in seq(along=labeltext)){
      empty_row <- TRUE
      for (row.no in seq(along=labeltext[[col.no]])){
        if (is.expression(labeltext[[col.no]][[row.no]]) ||
          !is.na(labeltext[[col.no]][[row.no]])){
          empty_row <- FALSE
          break
        }
      }
      widthcolumn <- append(widthcolumn, empty_row)
    }
    
    nr <- length(labeltext[[1]])
    label_type = "list"
  } else if (is.vector(labeltext)){
    widthcolumn <- c(FALSE)
    nc = 1
    nr = length(labeltext)
    
    labeltext <- matrix(labeltext, ncol=1)
    label_type = "matrix"
  } else {
    # Original code for matrixes
    widthcolumn <- !apply(is.na(labeltext), 1, any)
    nc <- NCOL(labeltext)
    nr <- NROW(labeltext)
    label_type = "matrix"
  }
  
  # Prepare the summary and align variables
  if (is.null(align)){
    align <- c("l", rep("r", nc - 1))
  } else {
    align <- rep(align, length = nc)
  } 
  
  is.summary <- rep(is.summary, length = nr)
  
  getLabels <- function(){
    labels <- vector("list", nc)
    
    # Walk through the labeltext
    # Creates a list matrix with 
    # The column part
    for (j in 1:nc) {
      labels[[j]] <- vector("list", nr)
      
      # The row part
      for (i in 1:nr) {
        txt_out <- fetchRowLabel(label_type, labeltext, i, j)
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
              x <- 0
            }
            # Create a textGrob for the summary
            labels[[j]][[i]] <- textGrob(txt_out, x = x,
              just = just,
              hjust = 0,
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
        }
      }
    }
    return(labels)
  }
  labels <- getLabels()
  
  # Set the gap between columns
  colgap <- unit(6, "mm")
  
  # There is always at least one column so grab the widest one
  # and have that as the base for the column widths
  colwidths <- unit.c(findWidestGrob(labels[[1]]), colgap)
  
  # If multiple row label columns, add the other column widths
  if (nc > 1) {
    for (i in 2:nc){
      colwidths <- unit.c(colwidths, 
        findWidestGrob(labels[[i]]), 
        colgap)  
    } 
  }
  
  # Add the base grapwh width to the total column width
  # default is 2 inches
  colwidths <- unit.c(colwidths, graphwidth)
  
  # Create the canvas for the plot
  plot.new()
  
  # The base viewport, set the increase.line_height paremeter if it seems a little
  # crowded between the lines that might happen when having multiple comparisons
  main_grid_layout <- grid.layout(nrow   = nr + 1, 
                                  ncol   = length(colwidths),
                                  widths = colwidths,
                                  heights = unit(c(rep(1, nr), 0.5)*lineHeight.inc, "lines"))
  pushViewport(viewport(layout = main_grid_layout,
                        name="Base"))
  
  # If the borders are smaller than the upper/lower limits 
  # then clip the graph. The line will have arrows indicating
  # that it continues beyond the graph
  # The zero bar has to be on the chart though!
  xrange <- function(){
    top <- max(upper, na.rm = TRUE)
    bottom <- min(lower, na.rm = TRUE)
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
          ifelse(is.infinite(clip[1]),
            bottom,
            clip[1])
        ), 
        max(
          zero,
          ifelse(is.infinite(clip[2]),
            top,
            clip[2])
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
  
  # Create the fourth argument 4 the drawNormalCI() function 
  if (!is.null(boxsize)){
    # If matrix is provided this will convert it
    # to a vector but it doesn't matter in this case
    info <- rep(boxsize, length = length(mean))
  }else{
    # Get width of the lines
    cwidth <- (upper - lower)
    # Set cwidth to min value if the value is invalid
    # this can be the case for reference points
    cwidth[cwidth <= 0 | is.na(cwidth)] <- min(cwidth[cwidth > 0])
    info <- 1/cwidth
    info <- info/max(info[!is.summary], na.rm = TRUE)
    info[is.summary] <- 1
  }
  
  printLabels <- function(){
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
  printLabels()
  getXTicks <- function(){
    if (is.null(xticks) && xlog){
      xticks <- getTicks(exp(xrange()), exp=TRUE)
    }
  }
  printXaxis <- function(){
    # Print x-axis and change everything to log scale if needed
    if (xlog) {
      clip[clip < 0] <- 0
      clip <- log(clip)
      zero <- log(zero)
      
      axis_vp <- viewport(layout.pos.col = 2 * nc + 1, 
                          xscale         = xrange(),
                          name           = "axis")
      # Print y-axis - the vertical "zero" axis
      grid.lines(x = unit(zero, "native"), 
        y = 0:1, 
        gp = gpar(col = col$zero, lwd=lwd.zero))
      
      if (is.null(xticks)) {
        # ticks <- pretty(exp(xrange()))
        # ticks <- ticks[ticks > 0]
        ticks <- getTicks(exp(xrange()), clip=clip, exp=TRUE, digits=xticks.digits)
        
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
          formatC(ticks, digits = 2, format="f", drop0trailing=FALSE), format(ticks, digits = 1))
        xticks <- log(ticks)
      }else{
        xticks <- NULL
        ticklabels <- FALSE
      }

      
    } else {
      axis_vp <- viewport(layout.pos.col = 2 * nc + 1, 
                          xscale         = xrange(),
                          name           = "axis")
      
      if (is.null(xticks)){
        ticks <- getTicks(exp(xrange()), clip=clip, digits=xticks.digits)
        
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

    # Now plot the axis inkluding the horizontal bar
    pushViewport(axis_vp)
    
    # Plot the vertical "zero" axis
    grid.lines(x  = unit(zero, "native"), 
               y  = 0:1, 
               gp = gpar(col = col$zero, lwd=lwd.zero))

    # Omit the axis if specified as 0
    if(length(xticks) != 1 || xticks == 0){
      # Plot the actual x-axis
      grid.xaxis(at    = xticks, 
        label = ticklabels,
        gp    = gpar(cex = cex*0.6, col = col$axes, lwd=lwd.xaxis))
    }
    
    # Write the label for the x-axis
    grid.text(xlab, y = unit(-2, "lines"), gp = gpar(col = col$axes, cex=cex))
    upViewport()
  }
  printXaxis()
  
  # Output the different confidence intervals
  for (i in 1:nr) {
    if (is.na(mean[i])) 
      next
    
    if (is.matrix(org_mean)){
      low_values <- org_lower[i,] 
      mean_values <- org_mean[i,] 
      up_values <- org_upper[i,]
      info_values <- matrix(info, ncol=length(low_values))[i, ]
    }else{
      low_values <- org_lower[i] 
      mean_values <- org_mean[i] 
      up_values <- org_upper[i]
      info_values <- info[i]
    }
    
    # The line and box colors may vary
    clr.line <- rep(col$line, length=length(low_values))
    clr.box <- rep(col$box, length=length(low_values))

    line_vp <- viewport(layout.pos.row = i,
                        layout.pos.col = 2 * nc + 1, 
                        xscale = xrange(),
                        name = sprintf("Line_%d_%d", i, 2 * nc + 1))
    pushViewport(line_vp)
    
    if (is.summary[i]){
      drawSummaryCI(low_values, mean_values, up_values, info_values)
    }else{
      # Draw multiple confidence intervals
      if (length(low_values) > 1){
        y.offset_base <- 0.2
        y.offset_increase <- (1-y.offset_base*2)/length(low_values)
        
        for(j in 1:length(low_values)){
          drawNormalCI(low_values[j], 
            mean_values[j], 
            up_values[j], 
            info_values[j], 
            y.offset = y.offset_base + (j-1)*y.offset_increase,
            clr.line = clr.line[j],
            clr.box = clr.box[j])
        }
      }else{
        drawNormalCI(low_values, mean_values, up_values, info_values,
          clr.line = clr.line, clr.box = clr.box)
      }
    }
    
    
    upViewport()
  }
  upViewport()
}

#' A copy of rmeta meta.colors. If you have several values per row in a
#' plot then you can set the values to a vector where the first value 
#' represents the first line/box, second the second line/box etc. The 
#' vectors are only valid for the box & lines. 
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
#' 
#' @export
meta.colors <- function (all.elements, 
  box        = "black", 
  lines      = "gray", 
  summary    = "black", 
  zero       = "lightgray", 
  text       = "black", 
  axes       = "black") 
{
  if (missing(all.elements)) {
    return(list(box = box, lines = lines, summary = summary, 
        zero = zero, text = text, axes = axes))
  }
  
  if (is.null(all.elements)) 
    all.elements <- par("fg")
  
  return(list(box = all.elements, lines = all.elements, summary = all.elements, 
      zero = all.elements, text = all.elements, 
      axes = all.elements))
}