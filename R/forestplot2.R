#' Create a forestplot
#' 
#' forestplot2 is based on the rmeta 2.16 forestplot function. This 
#' function resolves some limitations of the original
#' functions such as:
#' \itemize{
#'   \item{Adding expressions: }{Allows use of expressions, e.g. \code{expression(beta)}}
#'   \item{Multiple bands: }{Using multiple confidence bands for the same label}
#'   \item{Autosize: }{Adapts to windows size}
#' }
#' 
#' Using multiple bands for the same label can be interesting when 
#' one wants to compare different outcomer. It can also be an alternative
#' when you want to show both crude and adjusted estimates.
#' 
#' Known issues: the x-axis does not entirely respect the margin.
#' Autosizing boxes is not always the best option, try to set these
#' manually as much as possible.
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
#'   details on how to utilize mm etc. The default is \code{auto}, that is it uses up whatever
#'   space that is left after adjusting for text size and legend.
#' @param lineheight Height of the graph. By default this is \code{auto} and adjustes to the
#'   space that is left after adjusting for x-axis size and legend. Sometimes
#'   it might be desireable to set the line height to a certain height, for
#'   instance if you have several forestplots you may want to standardize their
#'   line height, then you set this variable to a certain height, note this should 
#'   be provided as a \code{\link[grid]{unit}} object. A good option
#'   is to set the line height to \code{unit(2, "cm")}. A third option
#'   is to set line height to "lines" and then you get 50 % more than what the
#'   text height is as your line height.
#' @param col See \code{\link{fpColors}}
#' @param xlog If TRUE, x-axis tick marks are exponentiated
#' @param xticks Optional user-specified x-axis tick marks. Specify NULL to use 
#'   the defaults, numeric(0) to omit the x-axis.
#' @param xticks.digits The number of digits to allow in the x-axis if this
#'   is created by default.
#' @param lwd.xaxis lwd for the xaxis
#' @param lwd.zero  lwd for the vertical line that gives the no-effect line
#' @param lwd.ci lwd for the confidence bands
#' @param cex The font adjustment
#' @param boxsize Override the default box size based on precision
#' @param mar A numerical vector of the form c(bottom, left, top, right) of the type \code{unit()}
#' @param legend Legen corresponding to the number of bars.
#' @param legend.pos The position of the legend, either at the "top" or the "right"
#' @param legend.cex The cex size of the legend, by default \code{cex*0.8}
#' @param new_page If you want the plot to appear on a new blank page then set this to \code{TRUE}, by
#'  default it is \code{FALSE}.
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
                         graphwidth           = "auto", 
                         lineheight           = "auto",
                         col                  = fpColors(), 
                         xlog                 = FALSE, 
                         xticks               = NULL,
                         xticks.digits        = 2,
                         lwd.xaxis            = NULL,
                         lwd.zero             = NULL,
                         lwd.ci               = NULL,
                         cex                  = 1,
                         boxsize              = NULL, 
                         mar                  = unit(rep(.05, times=4), "npc"),
                         legend               = NULL,
                         legend.pos           = c("top", "right"),
                         legend.cex           = cex*.8,
                         new_page             = FALSE,
                         ...) 
{

  if (NCOL(mean) != NCOL(lower) ||
        NCOL(lower) != NCOL(upper) ||
        NCOL(mean) == 0)
    stop('Mean, lower and upper contain invalid number of columns',
         " Mean columns:", ncol(mean),
         " Lower bound columns:", ncol(lower),
         " Upper bound columns:", ncol(upper))
  
     
  if (!is.unit(lineheight) && !lineheight %in% c("auto", "lines"))
    stop("The argument lineheight must either be of type unit or set to 'auto',",
      " you have provided a '", class(lineheight), "' class")
  
  if (length(legend) > 0){
    if (length(legend) != ncol(mean))
      stop("If you want a legend you need to provide the same number of",
           " legend descriptors as you have boxes per line, currently you have ", 
           ncol(mean), " boxes and ",
           length(legend), " legends.")
    
    legend.pos = match.arg(legend.pos)
  }
  
  # Fix if data.frames were provided in the arguments
  if (is.data.frame(mean))
    mean <- as.matrix(mean)
  if (is.data.frame(lower))
    lower<- as.matrix(lower)
  if (is.data.frame(upper))
    upper <- as.matrix(upper)
  
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
      grid.lines(x = lims, 
                 y = y.offset, 
                 arrow = arrow(ends = ends, 
                               length = unit(0.05, "inches")), 
                 gp = gpar(col = clr.line, lwd=lwd.ci))
      if (!skipbox)
        grid.rect(x = unit(OR, "native"), 
                  y = y.offset, 
                  width = unit(size, 
                               "snpc"), 
                  height = unit(size, "snpc"), 
                  gp = gpar(fill = clr.box, 
                            col = clr.box))
    } else {
      # Don't draw the line if it's no line to draw
      if (LL != UL)
        grid.lines(x = unit(c(LL, UL), "native"), y = y.offset, 
                   gp = gpar(col = clr.line, lwd=lwd.ci))
      grid.rect(x = unit(OR, "native"), y=y.offset, 
                width = unit(size, 
                             "snpc"), 
                height = unit(size, "snpc"), 
                gp = gpar(fill = clr.box, 
                          col = clr.box))
    }
  }
  drawSummaryCI <- function(LL, OR, UL, size) {
    grid.polygon(x = unit(c(LL, OR, UL, OR), "native"), y = unit(0.5 + 
                                                                   c(0, 0.5 * size, 0, -0.5 * size), "npc"), 
                 gp = gpar(fill = col$summary, 
                           col = col$summary))
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
  findWidestGrob <- function (grob.list, return_unit="mm"){
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
  

  labels <- prFpGetLabels(label_type = label_type,
    labeltext = labeltext, align = align,
    nc = nc, nr = nr,
    is.summary = is.summary,
    fontfamily.summary = fontfamily.summary,
    fontfamily.labelrow = fontfamily.labelrow,
    col = col, cex = cex)
  
  # Set the gap between columns, I set it to
  # 6 mm but for relative widths we switch it
  # to npc that adapts to the window size
  colgap <- convertUnit(unit(6, "mm"), "npc", valueOnly=TRUE)
  if (colgap < .1)
    colgap <- unit(.05, "npc")
  else
    colgap <- unit(colgap, "npc")
  
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
  
  
  ###########################################
  # Normalize the widths to cover the whole #
  # width of the graph space.               #
  ###########################################
  if (is.unit(graphwidth)){
    # Add the base grapwh width to the total column width
    # default is 2 inches
    colwidths <- unit.c(colwidths, graphwidth)
    
    npc_colwidths <- convertUnit(colwidths, "npc", valueOnly=TRUE)
    colwidths <- unit(npc_colwidths/sum(npc_colwidths), "npc")
  }else if(graphwidth=="auto"){
    # If graph width is not provided as a unit the autosize it to the
    # rest of the space available
    npc_colwidths <- convertUnit(colwidths, "npc", valueOnly=TRUE)
    graphwidth <- unit(1 - sum(npc_colwidths), "npc")
    colwidths <- unit.c(colwidths, graphwidth)
  }else{
    stop("You have to provide graph width either as a unit() object or as auto.",
         " Auto sizes the graph to maximally use the available space.",
         " If you want to have exact mm width then use graphwidth = unit(34, 'mm').")
  }
  
  axisList <- prFpGetGraphTicksAndClips(xticks=xticks, 
                                        xticks.digits=xticks.digits, 
                                        xlog=xlog,
                                        xlab=xlab,
                                        lwd.xaxis=lwd.xaxis, 
                                        cex=cex,
                                        col=col,
                                        clip=clip, zero=zero, 
                                        x_range=prFpXrange(upper = upper, 
                                            lower = lower, 
                                            clip = clip, 
                                            zero = zero, 
                                            xticks = xticks, 
                                            xlog = xlog),
                                        nc = nc)
  clip <- axisList$clip
  
  ###################
  # Create the plot #
  ###################
  if (new_page) grid.newpage()
  
  # Adjust for the margins and the x-axis + label
  mar <- convertUnit(mar, unitTo="npc", valueOnly=TRUE)
  marList <- list()
  marList$x_adjust <- unit(mar[2]/2 - mar[4]/2, "npc")
  marList$y_adjust <- unit(mar[1]/2 - mar[3]/2, "npc")
  
  # This breaks without separate variables
  marList$bottom <- unit(mar[1], "npc")
  marList$left <- unit(mar[2], "npc")
  marList$top <- unit(mar[3], "npc")
  marList$right <- unit(mar[4], "npc")
  
  if (is.grob(axisList$axisGrob)){
    marList$bottom <- marList$bottom + axisList$axisHeight
    marList$y_adjust <- marList$y_adjust - 
      unit(convertUnit(axisList$axisHeight, "npc", valueOnly=TRUE)/2, "npc")
  }
  
  if (is.grob(axisList$labGrob)){
    marList$bottom <- marList$bottom + 
      grobHeight(axisList$labGrob) + 
      unit(1*cex, "lines")
    
    marList$y_adjust <- marList$y_adjust - 
      unit(.5, "grobheight", data=axisList$labGrob) - 
      unit(.5*cex, "lines")
  }
  
  
  if (length(legend) > 0){
    lGrobs <- prFpGetLegendGrobs(legend, legend.cex)
    if (legend.pos == "top"){
      legend_layout <- grid.layout(nrow=3, ncol=1, 
                                   heights=unit.c(attr(lGrobs, "max_height"),
                                                 colgap+colgap,
                                                 unit(1, "npc")-
                                                   attr(lGrobs, "max_height")-colgap))
      
      legend_pos <- list(row = 1,
                         col = 1)
      main_pos <- list(row = 3,
                       col = 1)
    }else{
      legend_width <- unit.c(attr(lGrobs, "max_height"),
                             colgap + colgap,
                             attr(lGrobs, "max_width"))
      
      legend_layout <- grid.layout(nrow=1, ncol=3, 
                                   widths = unit.c(unit(1, "npc") - 
                                                    colgap - sum(legend_width),
                                                  colgap,
                                                  sum(legend_width)))
      legend_pos <- list(row = 1,
                         col = 3)
      main_pos <- list(row = 1,
                       col = 1)
    }
    
    
    pushViewport(prFpGetLayoutVP(lineheight=lineheight, marList=marList, 
        labels = labels,
        nr=nr, legend_layout=legend_layout))
    vp <- viewport(layout.pos.row = legend_pos$row, 
                   layout.pos.col = legend_pos$col,
                   name = "legend")
    pushViewport(vp)
    
    # Draw the legend
    prFpDrawLegend(lGrobs=lGrobs, 
                   legend.pos=legend.pos, 
                   col=col, 
                   colgap=convertUnit(colgap, unitTo="mm"))
    upViewport()

    # Reset to the main plot
    vp <- viewport(layout.pos.row = main_pos$row, 
                   layout.pos.col = main_pos$col,
                   name="main")
    pushViewport(vp)
  }else{
    pushViewport(prFpGetLayoutVP(lineheight=lineheight, marList=marList, 
        labels = labels, nr=nr))
  }
  
  # The base viewport, set the increase.line_height paremeter if it seems a little
  # crowded between the lines that might happen when having multiple comparisons
  main_grid_layout <- grid.layout(nrow   = nr + 1, 
                                  ncol   = length(colwidths),
                                  widths = colwidths,
                                  heights = unit(c(rep(1, nr), .5)/(nr+.5), "npc"),
                                  respect = TRUE)
  pushViewport(viewport(layout = main_grid_layout,
                        name="BaseGrid"))
  
  
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
    textHeight <- convertUnit(grobHeight(textGrob("A", gp=gpar(cex=cex))),
                              unitTo="npc", 
                              valueOnly=TRUE)
    info <- 1/cwidth
    info <- info/max(info[!is.summary], na.rm = TRUE)
    # Adjust the dots as it gets ridiculous with small text and huge dots
    if (any(textHeight*(nr+.5) * 1.5 < info))
      info <- textHeight*(nr+.5) * 1.5 * info/max(info) + textHeight*(nr+.5)*1.5/4
    info[is.summary] <- 1
  }
  
  prFpPrintLabels(labels=labels,
                  nc=nc, 
                  nr=nr)
  
  prFpPrintXaxis(axisList=axisList, 
                 col=col, lwd.zero=lwd.zero)
  
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
                        xscale = prFpXrange(upper = upper, 
                            lower = lower, 
                            clip = clip, 
                            zero = zero, 
                            xticks = xticks, 
                            xlog = xlog),
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
  upViewport(2)
}
