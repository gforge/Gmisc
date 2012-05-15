#' Create a forestplot
#' 
#' forestplot2 is based on the rmeta 2.16 forestplot function. 
#' The original rmeta package is requiered for the meta.colors() 
#' function. This function resolves some limitations of the original
#' functions such as adding expressions and using multiple confidence
#' bands for the same label.
#' 
#' Using multiple bands for the same label is judged to be interesting when 
#' one wants to compare different outcomes. 
#' 
#' @param labeltext A list with the names of each row. This has to have the same
#'   amount of rows that the means but it can have more if you want to have more 
#'   complex rowlabels where you might want to have the estimates in plain text. 
#'   Use NA:s for blank spaces
#' @param mean A vector with te averages
#' @param lower The lower bound of the confidence interval for the forestplot
#' @param upper The upper bound of the confidence interval for the forestplot
#' @param align Vector giving alignment (l,r,c) for columns of table
#' @param is.summary A vector indicating by TRUE/FALSE if the value is a summary
#'   value which means that it will have a different font-style.
#' @param fontfamily.summary The fontfamily of the summary
#' @param fontfamily.labelrow The fontfamily of a regular row
#' @param clip Lower and upper limits for clipping confidence intervals to arrows
#' @param xlab x-axis label
#' @param zero x-axis coordinate for zero line
#' @param graphwidth Width of confidence interval graph
#' @param col See meta.colors
#' @param xlog If TRUE, x-axis tick marks are exponentiated
#' @param xticks Optional user-specified x-axis tick marks. Specify NULL to use 
#'   the defaults,numeric(0) to omit the x-axis.
#' @param increase.line_height If you plot multiple bars for each line you
#'   often want to increase the line height.
#' @param lwd.xaxis lwd for the xaxis
#' @param lwd.zero  lwd for the vertical line that gives the no-effect line
#' @param lwd.forestlines lwd for the confidence bands
#' @param cex The font adjustment
#' @param boxsize Override the default box size based on precision
#' @param ... Not used 
#' @return void
#' 
#' @import rmeta
#' 
#' @author max
#' @example examples/forestplot2.R
#' @export
forestplot2 <- function (labeltext, 
        mean, lower, upper, 
        align = NULL, 
        is.summary = FALSE,
        fontfamily.summary = NULL, 
        fontfamily.labelrow = NULL, 
        clip = c(-Inf, Inf), 
        xlab = "", zero = 0, 
        graphwidth = unit(51, "mm"), 
        col = meta.colors(), 
        xlog = FALSE, xticks = NULL,
        increase.line_height = 1.2,
        lwd.xaxis=NULL,
        lwd.zero=NULL,
        lwd.forestlines=NULL,
        cex = 1,
        boxsize = NULL, ...) 
{
    require("grid") || stop("`grid' package not found")
    require("rmeta") || stop("`rmeta' package not found")
    
    # Addon: if two-dimentional mean, lower & upper 
    # the two are printed next to eachother
    if (NCOL(mean) == NCOL(lower) && NCOL(lower) == NCOL(upper) && NCOL(mean) > 0){
        org_mean <- mean
        org_lower <- lower
        org_upper <- upper
        if (NCOL(mean) > 1){
            mean <- as.vector(mean)
            lower <- as.vector(lower)
            upper <- as.vector(upper)
        }   
    }else{
        stop('Mean, lower and upper contain invalid number of columns')
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
        clipbox <- box < 0 || box > 1
        
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
                            length = unit(0.05, "inches")), gp = gpar(col = clr.line, lwd=lwd.forestlines))
            if (!clipbox) 
                grid.rect(x = unit(OR, "native"), y=y.offset, width = unit(size, 
                                "snpc"), height = unit(size, "snpc"), gp = gpar(fill = clr.box, 
                                col = clr.box))
        }
        else {
            grid.lines(x = unit(c(LL, UL), "native"), y = y.offset, 
                    gp = gpar(col = clr.line, lwd=lwd.forestlines))
            grid.rect(x = unit(OR, "native"), y=y.offset, width = unit(size, 
                            "snpc"), height = unit(size, "snpc"), gp = gpar(fill = clr.box, 
                            col = clr.box))
            if ((convertX(unit(OR, "native") + unit(0.5 * size, 
                                        "lines"), "native", valueOnly = TRUE) > UL) && 
                    (convertX(unit(OR, "native") - unit(0.5 * size, 
                                        "lines"), "native", valueOnly = TRUE) < LL)) 
                grid.lines(x = unit(c(LL, UL), "native"), y = 0.5, 
                        gp = gpar(col = clr.line, lwd=lwd.forestlines))
        }
    }
    drawSummaryCI <- function(LL, OR, UL, size) {
        grid.polygon(x = unit(c(LL, OR, UL, OR), "native"), y = unit(0.5 + 
                                c(0, 0.5 * size, 0, -0.5 * size), "npc"), gp = gpar(fill = col$summary, 
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
        for (i in 1:(length(grob.list))){
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
    
    # Workaround for expressions
    if (is.expression(labeltext)){
        widthcolumn <- c(TRUE)
        # Can't figure out multiple levels of expressions
        nc <- 1
        nr <- length(labeltext)
        label_type = "expression"
    }else if(is.list(labeltext)){
        if (validateLabelList(labeltext) == FALSE)
            stop("Invalid labellist, it has to be formed as a matrix m x n elements")
        
        # Can't figure out multiple levels of expressions
        nc <- length(labeltext)
        
        # Works but probably not as orig. intended
        widthcolumn <- rep(TRUE, nc)
        
        nr <- length(labeltext[[1]])
        label_type = "list"
    }else{
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
                    findWidestGrob(labels[[i]][widthcolumn]), 
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
    pushViewport(viewport(layout = grid.layout(nr + 1, length(colwidths), 
                            widths = colwidths,
                            heights = unit(c(rep(1, nr), 0.5)*increase.line_height, "lines"))))
    
    # Get width of the lines
    cwidth <- (upper - lower)
    
    # If the borders are smaller than the upper/lower limits 
    # then clip the graph. The line will have arrows indicating
    # that it continues beyond the graph
    # The zero bar has to be on the chart though!
    xrange <- c(
            min(zero, 
                    max(
                            min(lower, na.rm = TRUE), 
                            clip[1])
            ), 
            max(
                    zero,
                    min(
                            max(upper, na.rm = TRUE), 
                            clip[2])
            )
    )
    
    # Create the fourth argument 4 the drawNormalCI() function 
    
    # If boxsize was provided override the info
    if (!is.null(boxsize)){
        # If matrix is provided this will convert it
        # to a vector but it doesn't matter in this case
        info <- rep(boxsize, length = length(cwidth))
    }else{
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
                    pushViewport(viewport(layout.pos.row = i, 
                                    layout.pos.col = 2 * j - 1))
                    grid.draw(labels[[j]][[i]])
                    popViewport()
                }
            }
        }
    }
    printLabels()
    
    pushViewport(viewport(layout.pos.col = 2 * nc + 1, xscale = xrange))
    
    # Print y-axis - the vertical "zero" axis
    grid.lines(x = unit(zero, "native"), y = 0:1, gp = gpar(col = col$zero, lwd=lwd.zero))
    
    # Print x-axis
    if (xlog) {
        if (is.null(xticks)) {
            ticks <- pretty(exp(xrange))
            ticks <- ticks[ticks > 0]
        }
        else {
            ticks <- xticks
        }
        if (length(ticks)) {
            if (min(lower, na.rm = TRUE) < clip[1]) 
                ticks <- c(exp(clip[1]), ticks)
            if (max(upper, na.rm = TRUE) > clip[2]) 
                ticks <- c(ticks, exp(clip[2]))
            xax <- xaxisGrob(gp = gpar(cex = cex*0.6, col = col$axes, lwd=lwd.xaxis), 
                    at = log(ticks), name = "xax")
            xax1 <- editGrob(xax, gPath("labels"), label = format(ticks, 
                            digits = 2))
            grid.draw(xax1)
        }
    } else {
        if (is.null(xticks)) {
            grid.xaxis(gp = gpar(cex = cex*0.6, col = col$axes, lwd=lwd.xaxis))
        }
        else if (length(xticks)) {
            grid.xaxis(at = xticks, gp = gpar(cex = cex*0.6, col = col$axes, lwd=lwd.xaxis))
        }
    }
    
    # Write the label for the x-axis
    grid.text(xlab, y = unit(-2, "lines"), gp = gpar(col = col$axes, cex=cex))
    popViewport()
    
    # Output the different confidence intervals
    for (i in 1:nr) {
        if (is.na(mean[i])) 
            next
        pushViewport(viewport(layout.pos.row = i, 
                        layout.pos.col = 2 * nc + 1, 
                        xscale = xrange))
        
        if (is.matrix(org_mean)){
            low_values <- org_lower[i,] 
            mean_values <- org_mean[i,] 
            up_values <- org_upper[i,]
            info_values <- matrix(info, ncol=length(low_values))[i, ]
        }else{
            low_values <- lower[i] 
            mean_values <- mean[i] 
            up_values <- upper[i]
            info_values <- info[i]
        }
        
        # The line and box colors may vary
        clr.line <- rep(col$line, length=length(low_values))
        clr.box <- rep(col$box, length=length(low_values))
        if (is.summary[i]) 
            drawSummaryCI(low_values, mean_values, up_values, info_values)
        else{
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
        
        
        popViewport()
    }
    popViewport()
}