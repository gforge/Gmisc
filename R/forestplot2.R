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
#' @param cex.axis The font adjustment for the x-xaxis, defaults to 60 \%
#'   of the cex parameter.
#' @param boxsize Override the default box size based on precision
#' @param mar A numerical vector of the form c(bottom, left, top, right) of the type \code{unit()}
#' @param main The title of the plot if any, default \code{NULL}
#' @param legend Legen corresponding to the number of bars.
#' @param legend.pos The position of the legend, either at the "top" or the "right" unlesss
#'  positioned inside the plot. If you want the legend to be positioned inside the plot
#'  then you have to provide a list with the same x & y qualities as \code{\link[graphics]{legend}}.
#'  For instance if you want the legend to be positioned at the top right corner then
#'  use \code{legend.pos = list("topright")} - this is equivalent to \code{legend.pos = list(x=1, y=1)}.
#'  If you want to have a distance from the edge of the graph then add a inset to the list,
#'  e.g. \code{legend.pos = list("topright", "inset"=.1)} - the inset should be either a \code{\link[grid]{unit}}
#'  element or a value between 0 and 1. The default is to have the boxes aligned vertical, if
#'  you want them to be in a line then you can specify the "align" option, e.g. 
#'  \code{legend.pos = list("topright", "inset"=.1, "align"="horizontal")} 
#' @param legend.cex The cex size of the legend, defaults to 80 \% procent of cex parameter.
#' @param legend.gp The \code{\link[grid]{gpar}} options for the legend. If you want
#'  the background color to be light grey then use \code{legend.gp = gpar(fill = "lightgrey")}.
#'  If you want a border then set the col argument: \code{legend.gp = gpar(fill = "lightgrey", col="black")}.
#'  You can also use the lwd and lty argument as usual, \code{legend.gp = gpar(lwd=2, lty=1)}, will result
#'  in a black border box of line type 1 and line width 2.
#' @param legend.r The box can have rounded edges, check out \code{\link[grid]{grid.roundrect}}. The 
#'  r option should be a \code{\link[grid]{unit}} object. This is by default \code{unit(0, "snpc")}
#'  but you can choose any value that you want. The \code{"snpc"} unit is the preferred option.
#' @param legend.padding The padding for the legend box, only used if box is drawn. This is 
#'  the distance from the border to the text/boxes of the legend.
#' @param legend.title The title of the legend if any, default to NULL 
#' @param new_page If you want the plot to appear on a new blank page then set this to \code{TRUE}, by
#'  default it is \code{FALSE}.
#' @param confintNormalFn You can specify exactly how the line with the box is 
#'  drawn for the normal (i.e. non-summary) confidence interval by changing this 
#'  parameter to your own function or some of the alternatives provided in the package.
#'  It defaults to the box function \code{\link{fpDrawNormalCI}}.
#' @param confintSummaryFn Same as previous argument but for the summary outputs
#'  and it defaults to \code{\link{fpDrawSummaryCI}}.
#' @param legendMarkerFn What type of function should be used for drawing the
#'  legends, this can be a list if you want different functions. It defaults to 
#'  a box if you have anything else than a single function or the number of columns 
#'  in the \code{mean} argument.
#' @param ... Passed on to the \code{confintNormalFn} and
#'  \code{confintSummaryFn} arguments
#' @return void 
#' 
#' @import grid
#' 
#' @author Max Gordon, Thomas Lumley
#' 
#' @example inst/examples/forestplot2_example.R
#' @family forestplot functions
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
                         cex.axis             = cex * 0.6,
                         boxsize              = NULL, 
                         mar                  = unit(rep(5, times=4), "mm"),
                         main                 = NULL,
                         legend               = NULL,
                         legend.pos           = "top",
                         legend.cex           = cex*.8,
                         legend.gp            = NULL,
                         legend.r             = unit(0, "snpc"),
                         legend.padding       = unit(ifelse(length(legend.gp) > 0, 3, 0), "mm"),
                         legend.title         = NULL,
                         new_page             = FALSE,
                         confintNormalFn      = fpDrawNormalCI,
                         confintSummaryFn     = fpDrawSummaryCI,
                         legendMarkerFn      = NULL,
                         ...) 
{

  if (NCOL(mean) != NCOL(lower) ||
        NCOL(lower) != NCOL(upper) ||
        NCOL(mean) == 0)
    stop('Mean, lower and upper contain invalid number of columns',
         " Mean columns:", ncol(mean),
         " Lower bound columns:", ncol(lower),
         " Upper bound columns:", ncol(upper))

  # Prepare the legend marker
  if (!is.null(legend)){
    legendMarkerFn <- prFpPrepareLegendMarker(legendMarkerFn = legendMarkerFn, 
                                              col_no = NCOL(mean), 
                                              confintNormalFn = confintNormalFn)
  }
  
  confintNormalFn <- 
    prFpGetConfintFnList(fn = confintNormalFn, 
                         no_rows = NROW(mean), 
                         no_cols = NCOL(mean))
  confintSummaryFn <- 
    prFpGetConfintFnList(fn = confintSummaryFn, 
                         no_rows = NROW(mean), 
                         no_cols = NCOL(mean))
  
  if (!is.unit(lineheight) && !lineheight %in% c("auto", "lines"))
    stop("The argument lineheight must either be of type unit or set to 'auto',",
      " you have provided a '", class(lineheight), "' class")
  
  if (length(legend) > 0){
    if (length(legend) != ncol(mean))
      stop("If you want a legend you need to provide the same number of",
           " legend descriptors as you have boxes per line, currently you have ", 
           ncol(mean), " boxes and ",
           length(legend), " legends.")
    if (is.list(legend.pos)){
      legend.pos <- prFpGetLegendBoxPosition(legend.pos)
    }else if (!legend.pos %in% c("top", "right")){
      stop("The legend is either a list positioning it inside the main plot or at the 'top' or 'right' side,",
        " the position '", legend.pos, "' is not valid.")
    }
    
    if (inherits(legend.gp, "gpar")){
      # Remove default border if no color
      # unless there is a line width or type specified
      if (!"col" %in% names(legend.gp)){
        if (any(c("lwd", "lwd") %in% names(legend.gp))){
          legend.gp[["col"]] = "black"
        }else{
          legend.gp[["col"]] = NA
        }
      } 
    }
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
  
  # Get the number of columns (nc) and number of rows (nr)
  # if any columns are to be spacers the widthcolumn variable
  if (is.expression(labeltext)){
    widthcolumn <- c(TRUE)
    # Can't figure out multiple levels of expressions
    nc <- 1
    nr <- length(labeltext)
    label_type = "expression"
  } else if (is.list(labeltext)){
    if (!prFpValidateLabelList(labeltext))
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
  colgap <- convertUnit(colgap, "mm")
  
  # There is always at least one column so grab the widest one
  # and have that as the base for the column widths
  colwidths <- unit.c(prFpFindWidestGrob(labels[[1]]), colgap)
  
  # If multiple row label columns, add the other column widths
  if (nc > 1) {
    for (i in 2:nc){
      colwidths <- unit.c(colwidths, 
                          prFpFindWidestGrob(labels[[i]]), 
                          colgap)  
    } 
  }
  
  
  axisList <- prFpGetGraphTicksAndClips(xticks=xticks, 
                                        xticks.digits=xticks.digits, 
                                        xlog=xlog,
                                        xlab=xlab,
                                        lwd.xaxis=lwd.xaxis, 
                                        cex=cex,
                                        cex.axis=cex.axis,
                                        col=col,
                                        clip=clip, zero=zero, 
                                        x_range=prFpXrange(upper = upper, 
                                                           lower = lower, 
                                                           clip = clip, 
                                                           zero = zero, 
                                                           xticks = xticks, 
                                                           xlog = xlog),
                                        nc = nc,
                                        mean = org_mean)
  clip <- axisList$clip
  
  ###################
  # Create the plot #
  ###################
  if (new_page) grid.newpage()
  
  # Adjust for the margins and the x-axis + label
  marList <- list()
  
  # This breaks without separate variables
  marList$bottom <- convertY(mar[1], "npc")
  marList$left <- convertX(mar[2], "npc")
  marList$top <- convertY(mar[3], "npc")
  marList$right <- convertX(mar[4], "npc")
  
  prPushMarginViewport(bottom = marList$bottom,
    left = marList$left,
    top = marList$top,
    right = marList$right,
    name="forestplot_margins")
  
  if (is.character(main)){
    prGridPlotTitle(title=main, base_cex = cex)
  }
  
  # Initiate the legend
  if (length(legend) > 0){
    lGrobs <- prFpGetLegendGrobs(legend = legend, 
      legend.cex = legend.cex, 
      legend.title = legend.title)
    legend_horizontal_height <- sum(legend.padding,
      attr(lGrobs, "max_height"), 
      legend.padding)
    if (!is.null(attr(lGrobs, "title"))){
      legend_horizontal_height <- sum(attr(lGrobs, "titleHeight"),
        attr(lGrobs, "line_height_and_spacing")[2],
        legend_horizontal_height)
    }
    legend_vertical_width <- sum(unit.c(legend.padding,
      attr(lGrobs, "max_height"),
      colgap,
      attr(lGrobs, "max_width"),
      legend.padding))
    
    # Prepare the viewports if the legend is not
    # positioned inside the forestplot, i.e. on the top or right side
    if ((!is.list(legend.pos) && legend.pos == "top") ||
      ("align" %in% names(legend.pos) && legend.pos[["align"]] == "horizontal")){
      legend_layout <- grid.layout(nrow=3, ncol=1, 
                                   heights=unit.c(legend_horizontal_height,
                                                 colgap+colgap,
                                                 unit(1, "npc")-
                                                   legend_horizontal_height-
                                                   colgap-colgap))
      
      legend_pos <- list(row = 1,
                         col = 1)
      main_pos <- list(row = 3,
                       col = 1)
    }else{
      legend_layout <- grid.layout(nrow=1, ncol=3, 
                                   widths = unit.c(unit(1, "npc") - 
                                                     colgap - 
                                                     legend_vertical_width,
                                                   colgap,
                                                   legend_vertical_width))
      legend_pos <- list(row = 1,
                         col = 3)
      main_pos <- list(row = 1,
                       col = 1)
    }
  }
    
  # If the legend should be positioned within the plot then wait
  # until after the plot has been drawn
  if (length(legend) > 0 && !is.list(legend.pos)){
    pushViewport(prFpGetLayoutVP(lineheight=lineheight,  
        labels = labels,
        nr=nr, 
        legend_layout=legend_layout))
    vp <- viewport(layout.pos.row = legend_pos$row, 
                   layout.pos.col = legend_pos$col,
                   name = "legend")
    pushViewport(vp)
    
    # Draw the legend
    prFpDrawLegend(lGrobs = lGrobs, 
                   legend.pos = legend.pos, 
                   col = col, 
                   colgap = convertUnit(colgap, unitTo="mm"),
                   legend.gp = legend.gp,
                   legend.r = legend.r,
                   legend.padding = legend.padding,
                   legendMarkerFn = legendMarkerFn,
                   ...)
    upViewport()

    # Reset to the main plot
    vp <- viewport(layout.pos.row = main_pos$row, 
                   layout.pos.col = main_pos$col,
                   name="main")
    pushViewport(vp)
  }else{
    pushViewport(prFpGetLayoutVP(lineheight=lineheight,  
        labels = labels, nr=nr))
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
  
  # Add space for the axis and the label
  axis_height <- unit(0, "npc")
  if (is.grob(axisList$axisGrob))
    axis_height <- axis_height  + grobHeight(axisList$axisGrob)
  if (is.grob(axisList$labGrob)){
    gp_lab_cex <- prGetTextGrobCex(axisList$labGrob)
    
    # The lab grob y actually includes the axis (note negative)
    axis_height <-  axis_height + 
      unit(gp_lab_cex+.5, "line")
  }
  
  axis_layout <- grid.layout(nrow=2,
                             ncol=1,
                             heights=unit.c(unit(1, "npc") - axis_height,
                                            axis_height))
  pushViewport(viewport(layout=axis_layout, 
                        name="axis_margin"))
  pushViewport(viewport(layout.pos.row=1, layout.pos.col=1))

  # The base viewport, set the increase.line_height paremeter if it seems a little
  # crowded between the lines that might happen when having multiple comparisons
  main_grid_layout <- grid.layout(nrow   = nr, 
                                  ncol   = length(colwidths),
                                  widths = colwidths,
                                  heights = unit(rep(1/nr, nr), "npc"),
                                  respect = TRUE)
  pushViewport(viewport(layout = main_grid_layout,
                        name="BaseGrid"))
  
  # Create the fourth argument 4 the fpDrawNormalCI() function 
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
    info <- 1/cwidth*0.75
    info <- info/max(info[!is.summary], na.rm = TRUE)
    # Adjust the dots as it gets ridiculous with small text and huge dots
    if (any(textHeight*(nr+.5) * 1.5 < info))
      info <- textHeight*(nr+.5) * 1.5 * info/max(info, na.rm=TRUE) + textHeight*(nr+.5)*1.5/4
    
    # Set summary to maximum size
    info[is.summary] <- 1/NCOL(org_mean)
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
    clr.line <- rep(col$line, length.out=length(low_values))
    clr.marker <- rep(col$box, length.out=length(low_values))
    clr.summary <- rep(col$summary, length.out=length(low_values))
    
    line_vp <- viewport(layout.pos.row = i,
                        layout.pos.col = length(colwidths), 
                        xscale = axisList$x_range,
                        name = sprintf("Line_%d_%d", i, 2 * nc + 1))
    pushViewport(line_vp)
    
    # Draw multiple confidence intervals
    if (length(low_values) > 1){
      b_height <- max(info_values)
      if (is.unit(b_height))
        b_height <- convertUnit(b_height, unitTo="npc", valueOnly=TRUE)
      
      # Add some space so that the boxes 
      # between lines don't blend
      margin <- 0.05 
      y.offset_base <- b_height/2 + margin 
      y.offset_increase <- (1 - margin - y.offset_base*2)/(length(low_values)-1)
      
      for(j in length(low_values):1){
        # Start from the bottom and plot up
        # the one on top should always be
        # above the one below
        current_y.offset <- y.offset_base + (length(low_values)-j)*y.offset_increase
        if (is.summary[i]){
          call_list <- 
            list(confintSummaryFn[[i]][[j]],
                 lower_limit=low_values[j], 
                 estimate=mean_values[j], 
                 upper_limit=up_values[j], 
                 size=info_values[j],
                 col = clr.summary[j],
                 y.offset = current_y.offset)
        }else{
          call_list <- 
            list(confintNormalFn[[i]][[j]],
                 lower_limit=low_values[j], 
                 estimate=mean_values[j], 
                 upper_limit=up_values[j], 
                 size=info_values[j], 
                 y.offset = current_y.offset,
                 clr.line = clr.line[j],
                 clr.marker = clr.marker[j],
                 lwd=lwd.ci)
        }

        # Add additional arguments that are passed on
        # from the original parameters
        if (length(list(...)) > 0){
          ll <- list(...)
          for (name in names(ll)){
            call_list[[name]] <- ll[[name]]
          }
        }

        # Do the actual drawing of the object
        eval(as.call(call_list))
      }
    }else{
      if (is.summary[i]){
        call_list <- 
          list(confintSummaryFn[[i]],
               lower_limit=low_values, 
               estimate=mean_values, 
               upper_limit=up_values, 
               size=info_values,
               col=clr.summary)
      }else{
        call_list <- 
          list(confintNormalFn[[i]],
               lower_limit=low_values, 
               estimate=mean_values, 
               upper_limit=up_values, 
               size=info_values,
               clr.line = clr.line, 
               clr.marker = clr.marker,
               lwd=lwd.ci)
      }
      
      # Add additional arguments that are passed on
      # from the original parameters
      if (length(list(...)) > 0){
        ll <- list(...)
        for (name in names(ll)){
          call_list[[name]] <- ll[[name]]
        }
      }
      
      # Do the actual drawing of the object
      eval(as.call(call_list))
      
    }
    
    upViewport()
  }
  
  # Output the legend if it is inside the main plot
  if (length(legend) > 0 && is.list(legend.pos)){
    plot_vp <- viewport(layout.pos.row = 1:nr,
      layout.pos.col = 2 * nc + 1, 
      name = "main_plot_area")
    pushViewport(plot_vp)
    
    if ("align" %in% names(legend.pos) && legend.pos[["align"]] == "horizontal"){
      # Calculated with padding above
      height <- legend_horizontal_height
      # Calculate the horizontal width by iterating througha all elements
      # as each element may have a different width
      width <- 0
      for (i in 1:length(lGrobs)){
        if (width > 0){
          width <- width + convertUnit(colgap, unitTo="npc", valueOnly=TRUE)
        }
        width <- width + convertUnit(attr(lGrobs, "max_height") + colgap + attr(lGrobs[[i]], "width"), unitTo="npc", valueOnly=TRUE) 
      }
      # Add the padding
      width <- unit(width + convertUnit(legend.padding, unitTo="npc", valueOnly=TRUE)*2, "npc")
    }else{
      legend_height <- attr(lGrobs, "line_height_and_spacing")[rep(1:2, length.out=length(legend)*2-1)]
      if (!is.null(attr(lGrobs, "title"))){
        legend_height <- unit.c(attr(lGrobs, "titleHeight"),
          attr(lGrobs, "line_height_and_spacing")[2], legend_height)
      }
      
      height <- sum(legend.padding, legend_height, legend.padding)
      width <- legend_vertical_width
    }
    pushViewport(viewport(x=legend.pos[["x"]],
                          y=legend.pos[["y"]],
                          width=width,
                          height=height, 
                          just=legend.pos[["just"]]))
    # Draw the legend
    prFpDrawLegend(lGrobs = lGrobs, 
                   legend.pos = legend.pos, 
                   col = col, 
                   colgap = colgap,
                   legend.gp = legend.gp,
                   legend.r = legend.r,
                   legend.padding = legend.padding,
                   legendMarkerFn = legendMarkerFn,
                   ...)
    upViewport(2)
  }
  
  # Go back to the original viewport
  seekViewport("forestplot_margins")
  upViewport(2)
}
