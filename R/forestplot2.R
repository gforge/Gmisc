#' Draws a forest plot
#'
#' The \emph{forestplot2} is based on the \pkg{rmeta} 2.16
#' \code{\link[rmeta]{forestplot}} function. This
#' function resolves some limitations of the original
#' functions such as:
#' \itemize{
#'   \item{Adding expressions: }{Allows use of expressions, e.g. \code{expression(beta)}}
#'   \item{Multiple bands: }{Using multiple confidence bands for the same label}
#'   \item{Autosize: }{Adapts to viewport (graph) size}
#' }
#'
#' @section Multiple bands:
#'
#' Using multiple bands, i.e. multiple lines, per variable can be interesting when
#' you want to compare different outcomes. E.g. if you want to compare survival
#' specific to heart disease to overall survival for smoking it may be useful to
#' have two bands on top of eachother. Another useful implementation is to show
#' crude and adjusted estimates as separate bands.
#'
#' @section Known issues:
#'
#' The x-axis does not entirely respect the margin. Autosizing boxes is not
#' always the best option, try to set these manually as much as possible.
#'
#' @param labeltext A list, matrix, vector or expression with the names of each
#'  row. The list should be wrapped in m x n number to resemble a matrix:
#'  \code{list(list("rowname 1 col 1", "rowname 2 col 1"), list("r1c2", expression(beta))}
#'  You can also provide a matrix although this cannot have expressions by design:
#'  \code{matrix(c("rowname 1 col 1", "rowname 2 col 1", "r1c2", "beta"), ncol=2)}
#'  Use NA:s for blank spaces and if you provide a full column with NA then
#'  that column is a empty column that adds some space. \emph{Note:} If you do not
#'  provide the mean/lower/upper arguments the function expects the label text
#'  to be a matrix containing the labeltext in the rownames and then columns for
#'  mean, lower, and upper .
#' @param mean A vector or a matrix with the averages. You can also provide a 2D/3D
#'  matrix that is automatically converted to the lower/upper parameters.
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
#' @param zero x-axis coordinate for zero line. If you provide a vector of length 2 it
#'   will print a rectangle instead of just a line.
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
#' @param main The title of the plot if any
#' @param legend Legend corresponding to the number of bars.
#' @param legend_args The legend arguments as returned by the \code{\link{fpLegend}} function.
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
                         align,
                         is.summary           = FALSE,
                         fontfamily.summary,
                         fontfamily.labelrow,
                         clip                 = c(-Inf, Inf),
                         xlab                 = "",
                         zero                 = 0,
                         graphwidth           = "auto",
                         lineheight           = "auto",
                         col                  = fpColors(),
                         xlog                 = FALSE,
                         xticks,
                         xticks.digits        = 2,
                         lwd.xaxis,
                         lwd.zero,
                         lwd.ci,
                         cex                  = 1,
                         cex.axis             = cex * 0.6,
                         boxsize,
                         mar                  = unit(rep(5, times=4), "mm"),
                         main,
                         legend,
                         legend_args          = fpLegend(),
                         new_page             = FALSE,
                         confintNormalFn      = fpDrawNormalCI,
                         confintSummaryFn     = fpDrawSummaryCI,
                         legendMarkerFn,
                         ...)
{
  dot_args <- list(...)
  if (any(grepl("^legend\\.", names(dot_args)))){
    l_args <- names(dot_args)[grep("^legend\\.", names(dot_args))]
    warning("The legend arguments have been grouped into legend_args, see documentation.",
            " You have provided the following legend arguments: '", paste(l_args, collapse="', '"), "'",
            " these will overwrite any arguments generated by the real legend_args",
            "  in order to maintain backwards compatibility")
    for (n in l_args){
      short_name <- gsub("^legend\\.", "", n)
      legend_args[[short_name]] <- dot_args[[n]]
      dot_args[[n]] <- NULL
    }
  }

  if (missing(lower) &&
        missing(upper) &&
        missing(mean)){
    if(missing(labeltext))
      stop("You need to provide the labeltext or",
           " the mean/lower/upper arguments")

    if (NCOL(labeltext) != 3)
      stop("If you only provide the function the labeltext and",
           " not the mean/lower/upper then you need to have",
           " a 3-column labeltext where the columns correspond to",
           " the mean, lower, and upper columns")

    mean <- labeltext
    labeltext <- rownames(mean)
  }

  if (length(zero) > 2)
    stop("The zero marker can only be 1 or 2 values, you have provided: '", length(zero), "' values")

  if (missing(labeltext))
    labeltext <- rownames(mean)

  if (is.null(labeltext))
    stop("You must provide labeltext either in the direct form as an argument",
         " or as rownames for the mean argument.")

  # Assume that lower and upper are contained within
  # the mean variable
  if (missing(lower) &&
        missing(upper)){
    if (NCOL(mean) != 3)
      stop("If you do not provide lower/upper arguments your mean needs to have 3 columns")

    # If the mean can in this case be eithe 2D-matrix
    # that generates a regular forest plot or
    # it can be a 3D-array where the 3:rd level
    # constitutes the different bands
    all <- prFpConvertMultidimArray(mean)
    mean <- all$mean
    lower <- all$lower
    upper <- all$upper
  }

  if (NCOL(mean) != NCOL(lower) ||
        NCOL(lower) != NCOL(upper) ||
        NCOL(mean) == 0)
    stop('Mean, lower and upper contain invalid number of columns',
         " Mean columns:", ncol(mean),
         " Lower bound columns:", ncol(lower),
         " Upper bound columns:", ncol(upper))

  # Prepare the legend marker
  if (!missing(legend)){
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

  if (!missing(legend)){
    if (length(legend) != ncol(mean))
      stop("If you want a legend you need to provide the same number of",
           " legend descriptors as you have boxes per line, currently you have ",
           ncol(mean), " boxes and ",
           length(legend), " legends.")
    if (is.list(legend_args$pos)){
      legend_args$pos <- prFpGetLegendBoxPosition(legend_args$pos)
    }else if (!legend_args$pos %in% c("top", "right")){
      stop("The legend is either a list positioning it inside the main plot or at the 'top' or 'right' side,",
        " the position '", legend_args$pos, "' is not valid.")
    }

    if (inherits(legend_args$gp, "gpar")){
      # Remove default border if no color
      # unless there is a line width or type specified
      if (!"col" %in% names(legend_args$gp)){
        if (any(c("lwd", "lwd") %in% names(legend_args$gp))){
          legend_args$gp[["col"]] = "black"
        }else{
          legend_args$gp[["col"]] = NA
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
  if (missing(align)){
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

  if (!missing(main)){
    prGridPlotTitle(title=main, base_cex = cex)
  }

  # Initiate the legend
  if (!missing(legend)){
    lGrobs <- prFpGetLegendGrobs(legend = legend,
                                 cex = legend_args$cex,
                                 title = legend_args$title)
    legend_colgap <- colgap
    if (convertUnit(legend_colgap, unitTo = "mm", valueOnly = TRUE) >
          convertUnit(attr(lGrobs, "max_height"), unitTo = "mm", valueOnly = TRUE)){
      legend_colgap <- attr(lGrobs, "max_height")
    }

    legend_horizontal_height <-
      sum(legend_args$padding,
          attr(lGrobs, "max_height"),
          legend_args$padding)
    if (!is.null(attr(lGrobs, "title"))){
      legend_horizontal_height <-
        sum(attr(lGrobs, "titleHeight"),
            attr(lGrobs, "line_height_and_spacing")[2],
            legend_horizontal_height)
    }
    legend_vertical_width <-
      sum(unit.c(legend_args$padding,
                 attr(lGrobs, "max_height"),
                 legend_colgap,
                 attr(lGrobs, "max_width"),
                 legend_args$padding))



    # Prepare the viewports if the legend is not
    # positioned inside the forestplot, i.e. on the top or right side
    if ((!is.list(legend_args$pos) && legend_args$pos == "top") ||
      ("align" %in% names(legend_args$pos) && legend_args$pos[["align"]] == "horizontal")){
      legend_layout <- grid.layout(nrow=3, ncol=1,
                                   heights=unit.c(legend_horizontal_height,
                                                 legend_colgap+legend_colgap,
                                                 unit(1, "npc")-
                                                   legend_horizontal_height-
                                                   legend_colgap-legend_colgap))

      legend_pos <- list(row = 1,
                         col = 1)
      main_pos <- list(row = 3,
                       col = 1)
    }else{
      legend_layout <- grid.layout(nrow=1, ncol=3,
                                   widths = unit.c(unit(1, "npc") -
                                                     legend_colgap -
                                                     legend_vertical_width,
                                                   legend_colgap,
                                                   legend_vertical_width))
      legend_pos <- list(row = 1,
                         col = 3)
      main_pos <- list(row = 1,
                       col = 1)
    }
  }

  # If the legend should be positioned within the plot then wait
  # until after the plot has been drawn
  if (!missing(legend) > 0 &&
        !is.list(legend_args$pos)){
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
                   col = col,
                   colgap = convertUnit(legend_colgap, unitTo="mm"),
                   pos = legend_args$pos,
                   gp = legend_args$gp,
                   r = legend_args$r,
                   padding = legend_args$padding,
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
  if (!missing(boxsize)){
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
                 clr.marker = clr.marker[j])
          if (!missing(lwd.ci))
            call_list$lwd <- lwd.ci
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
               clr.marker = clr.marker)
        if (!missing(lwd.ci))
          call_list$lwd <- lwd.ci
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
  if (!missing(legend) &&
        is.list(legend_args$pos)){
    plot_vp <- viewport(layout.pos.row = 1:nr,
      layout.pos.col = 2 * nc + 1,
      name = "main_plot_area")
    pushViewport(plot_vp)

    if ("align" %in% names(legend_args$pos) && legend_args$pos[["align"]] == "horizontal"){
      # Calculated with padding above
      height <- legend_horizontal_height
      # Calculate the horizontal width by iterating througha all elements
      # as each element may have a different width
      width <- 0
      for (i in 1:length(lGrobs)){
        if (width > 0){
          width <- width + convertUnit(legend_colgap, unitTo="npc", valueOnly=TRUE)
        }
        width <- width + convertUnit(attr(lGrobs, "max_height") + legend_colgap + attr(lGrobs[[i]], "width"), unitTo="npc", valueOnly=TRUE)
      }
      # Add the padding
      width <- unit(width + convertUnit(legend_args$padding, unitTo="npc", valueOnly=TRUE)*2, "npc")
    }else{
      legend_height <- attr(lGrobs, "line_height_and_spacing")[rep(1:2, length.out=length(legend)*2-1)]
      if (!is.null(attr(lGrobs, "title"))){
        legend_height <- unit.c(attr(lGrobs, "titleHeight"),
          attr(lGrobs, "line_height_and_spacing")[2], legend_height)
      }

      height <- sum(legend_args$padding, legend_height, legend_args$padding)
      width <- legend_vertical_width
    }
    pushViewport(viewport(x=legend_args$pos[["x"]],
                          y=legend_args$pos[["y"]],
                          width=width,
                          height=height,
                          just=legend_args$pos[["just"]]))
    # Draw the legend
    prFpDrawLegend(lGrobs = lGrobs,
                   col = col,
                   colgap = legend_colgap,
                   pos = legend_args$pos,
                   gp = legend_args$gp,
                   r = legend_args$r,
                   padding = legend_args$padding,
                   legendMarkerFn = legendMarkerFn,
                   ...)
    upViewport(2)
  }

  # Go back to the original viewport
  seekViewport("forestplot_margins")
  upViewport(2)
}
