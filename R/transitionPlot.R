#' A transition plot
#' 
#' This plot purpose is to illustrate how states change before and
#' after. In my research I use it before surgery and after surgery
#' but it can be used in any situation where you have a change from 
#' one state to another
#'  
#' @param transition_flow This should be a matrix with the size of the transitions.
#'  The unit for each cell should be number of observations, row/column-proportions 
#'  will show incorrect sizes. The matrix needs to be square. The best way to generate
#'  this matrix is probably just do a \code{table(starting_state, end_state)}. The rows 
#'  represent the starting positions, while the columns the end positions. I.e. the first
#'  rows third column is the number of observations that go from the first class to the 
#'  third class.
#' @param type_of_arrow The types of arrow may be grid, simple, or gradient. Simple grid 
#'  arrows are the \code{\link[grid]{bezierGrob}} arrows (not that pretty), 
#'  simple is the \code{\link{bezierArrowSmpl}} that I've created to get a more exact 
#'  control of the arrow position and width, while gradient corresponds to \code{\link{bezierArrowSmplGradient}}
#'  allowing the arrow to have a fill color that slowly turns into the color of the arrow.
#' @param box_txt The text to appear inside of the boxes. If you need line breaks
#'  then you need to manually add a \\n inside the string. 
#' @param tot_spacing The proportion of the vertical space that is to be left
#'  empty. It is then split evenly between the boxes.
#' @param box_width The width of the box. By default the box is one fourth of
#'  the plot width. 
#' @param fill_start_box The fill color of the start boxes. This can either 
#'  be a single value ore a vector if you desire different colors for each 
#'  box. If you specify box_prop then this has to be a 2 column matrix.
#' @param txt_start_clr The text color of the start boxes. This can either 
#'  be a single value ore a vector if you desire different colors for each 
#'  box. If you specify box_prop then this has to be a 2 column matrix.
#' @param fill_end_box The fill color of the end boxes. This can either 
#'  be a single value ore a vector if you desire different colors for each 
#'  box. If you specify box_prop then this has to be a 2 column matrix.
#' @param txt_end_clr The text color of the end boxes. This can either 
#'  be a single value ore a vector if you desire different colors for each 
#'  box. If you specify box_prop then this has to be a 2 column matrix.
#' @param cex The cex \code{\link{gpar}} of the text
#' @param min_lwd The minimum width of the line that we want to illustrate the
#'  tranisition with. 
#' @param max_lwd The maximum width of the line that we want to illustrate the
#'  tranisition with. 
#' @param lwd_prop_total The width of the lines may be proportional to either the 
#'  other flows from that box, or they may be related to all flows. This is a boolean
#'  parameter that is set to true by default, i.e. relating to all flows.
#' @param arrow_clr The color of the arrows. Usually black, can be a vector indicating each arrow
#'  from first to last arrow (counting from the top). If the vector is of the same length as the 
#'  boxes then all box arrows will have the same color (that is all the arrows stemming from the
#'  left boxes)
#' @param abs_arrow_width The width can either be absolute, i.e. each arrow headed for a box
#'  has the exact same width. The alternative is that the width is related to the line width.
#' @param overlap_bg_clr In order to enhance the 3D perspective and to make it easier
#'  to follow arrows the arrows have a background color to separate them from those underneath.
#' @param overlap_order The order from first->last for the lines. This means that the last
#'  line will be on top while the first one will appear at the bottom. This should be provided
#'  as a vector.
#' @param overlap_add_width The width of the white cross-over line. You can specify this as a scalar
#'  multiplication of the current line width. In case of non-grid arrows then you can also have this
#'  as a unit which is recommended as it looks better. If the scalar is < 1 then the overlap is ignored.
#' @param box_prop If you want the boxes to have proportions indicating some other factors then input
#'  a matrix with quantiles for the proportions. Note the size mus be nrow(transition_flow) x 2.
#' @param mar A numerical vector of the form c(bottom, left, top, right) of the type \code{unit()}
#' @param main The title of the plot if any, default \code{NULL}
#' @param box_label A vector of length 2 if you want to label each box column
#' @param box_label_pos The position of the label, either \code{'top'} or \code{'bottom'}
#' @param box_label_cex The cex of the label, defaults to the default cex
#' @param new_page If you want the plot to appear on a new blank page then set this to \code{TRUE}, by
#'  default it is \code{FALSE}.
#' @return void 
#' @examples 
#' \dontrun{
#' # This example does not run since it
#' # takes a little while to assemble the
#' # arrows and RMD Check complains that this
#' # is more than allowed for
#' par_org <- par(ask=TRUE) 
#' # Settings
#' no_boxes <- 3
#' # Generate test setting
#' transition_matrix <- matrix(NA, nrow=no_boxes, ncol=no_boxes)
#' transition_matrix[1,] <- 200*c(.5, .25, .25)
#' transition_matrix[2,] <- 540*c(.75, .10, .15)
#' transition_matrix[3,] <- 340*c(0, .2, .80)
#' 
#' grid.newpage()
#' transitionPlot(transition_matrix,  
#'                box_txt = c("First", "Second", "Third"),
#'                type_of_arrow = "simple",
#'                min_lwd = unit(1, "mm"),
#'                max_lwd = unit(6, "mm"),
#'                overlap_add_width = unit(1, "mm"))
#' 
#' 
#' # Setup proportions
#' box_prop <- cbind(c(1,0,0.5), c(.52,.2,.8))
#' # From the Set2 Colorbrewer
#' start_box_clr <- c("#8DA0CB", "#FC8D62")
#' # Darken the colors slightly
#' end_box_clr <- c(colorRampPalette(c(start_box_clr[1], "#000000"))(10)[2],
#'                  colorRampPalette(c(start_box_clr[2], "#000000"))(10)[2])
#' # Create a new grid
#' grid.newpage()
#' transitionPlot(transition_matrix, box_prop=box_prop,
#'                fill_start_box=start_box_clr, fill_end_box=end_box_clr, 
#'                txt_start_clr = c("#FFFFFF", "#000000"), txt_end_clr = c("#FFFFFF", "#000000"),
#'                box_txt = c("First", "Second", "Third"),
#'                type_of_arrow = "gradient",
#'                min_lwd = unit(1, "mm"),
#'                max_lwd = unit(10, "mm"),
#'                overlap_add_width = unit(1, "mm"))
#' par(par_org)
#' }
#' @author max
#' @import grid
#' @export
transitionPlot <- function (transition_flow,
                            type_of_arrow = c("grid", "simple", "gradient"),
                            box_txt = rownames(transition_flow),
                            tot_spacing = 0.2,
                            box_width = 1/4, 
                            fill_start_box = "darkgreen",
                            txt_start_clr = "white",
                            fill_end_box = "steelblue",
                            txt_end_clr = "white",
                            cex=2,
                            min_lwd = 1,
                            max_lwd = 6,
                            lwd_prop_total = TRUE,
                            arrow_clr = "#000000",
                            abs_arrow_width = FALSE, 
                            overlap_bg_clr = "#FFFFFF",
                            overlap_order = 1:nrow(transition_flow),
                            overlap_add_width = 1.5,
                            box_prop = NULL,
                            mar = unit(rep(3, times=4), "mm"),
                            main = NULL,
                            box_label = NULL,
                            box_label_pos = "top",
                            box_label_cex = cex,
                            new_page = FALSE) {
  # Just for convenience
  no_boxes <- nrow(transition_flow)
  
  if (length(arrow_clr) == no_boxes){
    arrow_clr <- t(sapply(arrow_clr, FUN=function(x){rep(x, ncol(transition_flow))}))
  } else if (length(arrow_clr) == 1){
    arrow_clr <- rep(arrow_clr, no_boxes*ncol(transition_flow))
  } 
  
  if (length(arrow_clr) != no_boxes*ncol(transition_flow))
    stop("You have provided invalid number of arrow colors,",
      " you have ", length(arrow_clr), " colors, while you should provide either 1, ",
      no_boxes, ", or ", no_boxes*ncol(transition_flow), " colors")
  
  if (length(overlap_order) != no_boxes)
    stop("You have the wrong number of overlap orders, you provided ",
      length(overlap_order), " while it should be ", no_boxes)
  else if (all(overlap_order %in% 1:no_boxes)==FALSE)
    stop("Your overlap numbers contain numbers outside the rowrange of",
      " the transition rows, i.e. not between 1 and ", no_boxes)
  
  type_of_arrow <- match.arg(type_of_arrow)
  if (type_of_arrow != "grid"){
    if (!"unit" %in% class(min_lwd) ||
          !"unit" %in% class(max_lwd))
      stop("Your line widths must be in units when you specify the alternative arrows, e.g. unit(10, 'pt')")
    
    # We need to convert these into regular values in order to use
    # them later on in the calculations
    min_lwd <- convertUnit(min_lwd, unitTo="npc", valueOnly=TRUE)
    max_lwd <- convertUnit(max_lwd, unitTo="npc", valueOnly=TRUE)
  }
  
  # Do some sanity checking of the variables
  if (tot_spacing < 0 ||
        tot_spacing > 1)
    stop("Total spacing, the tot_spacing param,",
      " must be a fraction between 0-1,",
      " you provided ", tot_spacing)
  
  if (box_width < 0 ||
        box_width > 1)
    stop("Box width, the box_width param,",
      " must be a fraction between 0-1,",
      " you provided ", box_width)
  
  # If the text element is a vector then that means that 
  # the names are the same prior and after
  if (is.null(box_txt))
    box_txt = matrix("", ncol=2, nrow=no_boxes)
  if (is.null(dim(box_txt)) && is.vector(box_txt))
    if (length(box_txt) != no_boxes)
      stop("You have an invalid length of text description, the box_txt param,",
          " it should have the same length as the boxes, ", no_boxes, ",",
          " but you provided a length of ", length(box_txt))
    else
      box_txt <- cbind(box_txt, box_txt)
  else if (nrow(box_txt) != no_boxes ||
        ncol(box_txt) != 2)
    stop("Your box text matrix doesn't have the right dimension, ", 
         no_boxes, " x 2, it has: ", 
         paste(dim(box_txt), collapse=" x "))
  

  if (length(box_prop) == 0){
    # Make sure that the clrs correspond to the number of boxes
    fill_start_box <- rep(fill_start_box, length.out=no_boxes)
    txt_start_clr <- rep(txt_start_clr, length.out=no_boxes)
    fill_end_box <- rep(fill_end_box, length.out=no_boxes)
    txt_end_clr <- rep(txt_end_clr, length.out=no_boxes)
  }else{
    getBoxPropClr <- function(clr, lengthOneOK = FALSE){
      if (is.matrix(clr)){
        if (nrow(clr) == no_boxes &&
          ncol(clr) == 2){
          return (clr)
        }
      }else if (length(clr) == 2 ||
        (lengthOneOK && length(clr) == 1)){
        return (matrix(clr, ncol=2, nrow=no_boxes, byrow=TRUE))
      }
    
      return (NULL)
    }
    
    fill_start_box <- getBoxPropClr(fill_start_box)
    fill_end_box <- getBoxPropClr(fill_end_box)
    txt_start_clr <- getBoxPropClr(txt_start_clr, TRUE)
    txt_end_clr <- getBoxPropClr(txt_end_clr, TRUE)
    
    # Input checks
    if (is.matrix(box_prop) == FALSE)
      stop("You have to provide the box_prop as a matrix corresponding to the boxes")
    else if (nrow(box_prop) != no_boxes || ncol(box_prop) != 2)
      stop("Your box_prop matrix must have ", no_boxes, "x", 2,
        " dimensions, your matrix is currently of ",
        nrow(box_prop), "x", ncol(box_prop), " dimensions")
    else if (any(box_prop > 1 | box_prop < 0))
      stop("You have provided in box_prop invalid quantiles outside the 0-1 range")
    else if (length(fill_start_box) == 0)
      stop("You have provided invalid number of fill colors (fill_start_box) when used together with box_prop")
    else if (length(fill_end_box) == 0)
      stop("You have provided invalid number of fill colors (fill_end_box) when used together with box_prop")
    else if (length(txt_start_clr) == 0)
      stop("You have provided invalid number of text colors (txt_start_clr) when used together with box_prop")
    else if (length(txt_end_clr) == 0)
      stop("You have provided invalid number of text colors (txt_end_clr) when used together with box_prop")
  }
  
  if(nrow(transition_flow) != ncol(transition_flow))
    stop("Invalid input array, the matrix is not square but ",
      nrow(transition_flow), " x ", ncol(transition_flow))
     
  # Set the proportion of the start/end sizes of the boxes
  prop_start_sizes <- rowSums(transition_flow)/sum(transition_flow)
  prop_end_sizes <- colSums(transition_flow)/sum(transition_flow)
  
  if (sum(prop_end_sizes) == 0)
    stop("You can't have all empty boxes after the transition")
  
  getBoxPositions <- function (no, side){
    empty_boxes <- ifelse(side == "left", 
      sum(prop_start_sizes==0), 
      sum(prop_end_sizes==0))
    
    # Calculate basics
    space <- tot_spacing/(no_boxes-1-empty_boxes)
    
    # Do the y-axis
    ret <- list(height=(1-tot_spacing)*ifelse(side == "left", 
                                              prop_start_sizes[no], 
                                              prop_end_sizes[no]))
    if (no == 1){
      ret$top <- 1
    }else{
      ret$top <- 1 - 
        ifelse(side == "left", 
               sum(prop_start_sizes[1:(no-1)]), 
               sum(prop_end_sizes[1:(no-1)])) * (1-tot_spacing) -
        space*(no-1)
    }
    ret$bottom <- ret$top - ret$height
    ret$y <- mean(c(ret$top, ret$bottom))
    
    ret$y_exit <- rep(ret$y, times=no_boxes)
    ret$y_entry_height <- ret$height/3
    ret$y_entry <- seq(to=ret$y-ret$height/6,
                       from=ret$y+ret$height/6,
                       length.out=no_boxes)
  
    # Now the x-axis
    if (side == "right"){
      ret$left <- 1-box_width
      ret$right <- 1
    }else{
      ret$left <- 0
      ret$right <- box_width
    }
    
    txt_margin <- box_width/10
    ret$txt_height <- ret$height - txt_margin*2
    ret$txt_width <- box_width - txt_margin*2
  
    ret$x <- mean(c(ret$left, ret$right))
  
    return(ret)
  }
  
  plotArrows <- function(type, box_row, max_flow, min_lwd, max_lwd, clr, box_clr, add_width = NA){
    bx_left <- getBoxPositions(box_row, "left")

    # Plot the widest arrow last
    for (flow in order(transition_flow[box_row,])){
      if (transition_flow[box_row,flow] > 0){
        bx_right <- getBoxPositions(flow, "right")

        # Calculate line width
        lwd <- min_lwd + (max_lwd-min_lwd)*transition_flow[box_row,flow]/max_flow
        adjusted_lwd <- lwd
        if (is.na(add_width) == FALSE){
          if ("unit" %in% class(add_width)){
            adjusted_lwd <- convertUnit(unit(lwd, "npc") + add_width, unitTo="npc", valueOnly=TRUE)
          }else if (add_width > 1){
            adjusted_lwd <- lwd*add_width
          }else{
            # Quit if the width isn't bigger as it won't show
            return()
          }
        }
        a_l <- (box_width/4)
        x_ctrl_points <- c(bx_left$right, .5, .5, bx_right$left)
        y_ctrl_points <- c(bx_left$y_exit[flow], bx_left$y_exit[flow], 
          bx_right$y_entry[box_row], bx_right$y_entry[box_row])
        current_arrow_clr <- clr[(flow+(box_row-1)*no_boxes)]
        if (type=="grid"){
          if (abs_arrow_width){
            a_width <- bx_right$y_entry_height/no_boxes
          }else{
            # Not really sure but points seem to be a reasonable
            # unit for the lwd as a basis for this part
            a_width <- getGridVal(unit(lwd, "pt"), "npc")+
              bx_right$y_entry_height/(no_boxes+1)
          }
          a_angle <- atan(a_width/2/a_l)*180/pi
          # Need to adjust the end of the arrow as it otherwise overwrites part of the box
          # if it is thick
          x_ctrl_points[4] <- x_ctrl_points[4]-.00075*adjusted_lwd
          grid.bezier(x=x_ctrl_points, 
            y=y_ctrl_points, 
            gp=gpar(lwd=adjusted_lwd, fill=current_arrow_clr),
            arrow=arrow(type="closed", angle=a_angle, length=unit(a_l, "npc")))
          
        }else{
          # The width can be wider using the special bezier arrows
          if (abs_arrow_width){
            a_width <- bx_right$y_entry_height*1.5/no_boxes
          }else{
            a_width <- getGridVal(lwd, "npc") + 
              bx_right$y_entry_height/(no_boxes+1)
          }
          
          if (a_width < adjusted_lwd)
            warning("The arrow width is smaller than the width of the line,",
                "thus not appearing as a regular arrow: ", 
                a_width, " < ", adjusted_lwd)
          
          if (type=="simple"){
            bz <- bezierArrowSmpl(x=x_ctrl_points, 
                y=y_ctrl_points, 
                width=adjusted_lwd,
                arrow=list(length=a_l, base=a_width),
                clr=current_arrow_clr)
            grid.draw(bz)
          }else if (type=="gradient"){
            bz <- bezierArrowSmplGradient(x=x_ctrl_points, 
                y=y_ctrl_points, 
                width=adjusted_lwd,
                arrow=list(length=a_l, base=a_width),
                clr=current_arrow_clr,
                grdt_type = "triangle",
                grdt_clr_prop = 0.5,
                grdt_start_prop = .3,
                grdt_decrease_prop = .3,
                grdt_clr = box_clr)
            grid.draw(bz)
            
          }else{
            stop("The arrow type ", type, " is not yet implemented, sorry.")
          }
        } 
      }
    }
  }
  
  if (new_page) grid.newpage()
    
  # Add plot margin
  prPushMarginViewport(bottom = convertY(mar[1], unitTo="npc"),
                       left = convertX(mar[2], unitTo="npc"),
                       top = convertY(mar[3], unitTo="npc"),
                       right = convertX(mar[4], unitTo="npc"),
                       "main_margins")
  
  if (!is.null(main) && nchar(main) > 0){
    prGridPlotTitle(main, cex[1])
  }

  if (!is.null(box_label) && length(box_label) == 2){
    left <- getBoxPositions(side="left", no=1)
    right <- getBoxPositions(side="right", no=1)
    left_label <- textGrob(box_label[1],
                           gp=gpar(cex=box_label_cex))
    right_label <- textGrob(box_label[2],
                            gp=gpar(cex=box_label_cex))
    label_height <- convertY(max(grobHeight(left_label), grobHeight(right_label)), 
                   unitTo="npc", valueOnly=TRUE)
    # Add ygjp space and some margin
    label_height <- unit(label_height * 2 + label_height * 0.1, "npc")
    width <- list(left = unit(left$right - left$left, "npc"),
                  right = unit(right$right - right$left, "npc"))
    if (box_label_pos == "top"){
      gl <- grid.layout(nrow=2, ncol=3,
                        heights = unit.c(label_height, 
                                         unit(1, "npc") - label_height),
                        widths = unit.c(width$left,
                                        unit(1, "npc") - 
                                          width$left - 
                                          width$right, 
                                        width$right))
      label_row_no <- 1
      main_row_no <- 2
    }else{
      gl <- grid.layout(nrow=2, ncol=3,
                        heights = unit.c(unit(1, "npc") - label_height,
                                         label_height),
                        widths = unit.c(width$left,
                                        unit(1, "npc") - 
                                          width$left - 
                                          width$right, 
                                        width$right))
      label_row_no <- 2
      main_row_no <- 1
    }
    
    pushViewport(viewport(layout=gl, name="Label_layout"))
    
    pushViewport(viewport(layout.pos.row=label_row_no, layout.pos.col=1, name="Left_label"))
    grid.draw(left_label)
    popViewport()
    pushViewport(viewport(layout.pos.row=label_row_no, layout.pos.col=3, name="Right_label"))
    grid.draw(right_label)
    popViewport()
  
    pushViewport(viewport(layout.pos.row=main_row_no, layout.pos.col=1:3, name="Main_exc_label"))
  }
  
  plotBoxes <- function (no_boxes, width, txt, 
    fill_start_clr, fill_end_clr,
    lwd=2, line_col="#000000", plot_arrows = TRUE, proportion=FALSE) {
    
    getBoxSizedTextGrob <- function(txt, 
                                    txt_clr, 
                                    txt_cex,
                                    force_cex = FALSE,
                                    ...){
      bx_grob <- textGrob(txt,
                          gp=gpar(col=txt_clr, cex=txt_cex), 
                          ...)
      attr(bx_grob, "adjusted_cex") <- txt_cex
      if (force_cex)
        return(bx_grob)
      
      bx_height <- convertY(grobHeight(bx_grob), "npc", valueOnly=TRUE)
      # The box height is by definition 1 npc
      # We want to avoid anything that is bigger that
      # 95 % and that is includingt he yjp (the 1.5)
      if (.95 < bx_height*1.5){
        new_cex <- txt_cex * .95 / (bx_height * 1.5)
        if (new_cex < .25){
          bx_grob <- nullGrob()
        }else{
          bx_grob <- textGrob(txt,
                              gp=gpar(col=txt_clr, cex=new_cex),
                              ...)
        }
        attr(bx_grob, "adjusted_cex") <- new_cex
      }
      
      return(bx_grob)
    }
    
    plotBox <- function(bx, bx_txt, fill, txt_clr, proportion=FALSE){
      pushViewport(viewport(y=bx$y, x=bx$x, 
          height=bx$height, width=width))
      if (is.na(proportion)){
        grid.roundrect(gp = gpar(lwd=lwd, fill=fill, col=line_col))
        
        if (bx_txt != ""){
          bx_grob <- getBoxSizedTextGrob(txt=bx_txt, 
                                         txt_clr = txt_clr, 
                                         txt_cex = cex)
          grid.draw(bx_grob)
        }
      }else{
        # Adapted from Paul Murray's example http://www.stat.auckland.ac.nz/~paul/RG2e/customgrid-nestedlay.R
        pushViewport(viewport(layout=grid.layout(nrow=2, ncol=1, heights=c(proportion, 1-proportion))))
        grid.roundrect(gp = gpar(lwd=lwd, fill=fill[1], col=NA))
        if (bx_txt != ""){
          bx_grob <- getBoxSizedTextGrob(txt=bx_txt, 
                                         txt_clr = txt_clr[1], 
                                         txt_cex = cex)
          grid.draw(bx_grob)
        }
        
        pushViewport(viewport(layout.pos.row=2, clip="on"))
        if ((1-proportion) > 0){
          grid.roundrect(y=.5/(1-proportion), height=1/(1-proportion), gp = gpar(lwd=lwd, fill=fill[2], col=NA))
          if (bx_txt != ""){
            # Should not autoadjust the cex but keep the previous one
            prev_cex <- attr(bx_grob, "adjusted_cex")
            bx_grob <- getBoxSizedTextGrob(txt=bx_txt, 
                                           txt_clr = txt_clr[2], 
                                           txt_cex = prev_cex,
                                           force_cex = TRUE,
                                           y=0.5/(1-proportion))
            grid.draw(bx_grob)
          }
        }
        popViewport(2)
        grid.roundrect(gp = gpar(lwd=lwd, fill=NA, col=line_col))
      }
      popViewport()
    }
    
    for(i in overlap_order){
      if (prop_start_sizes[i] > 0){
        bx_left <- getBoxPositions(i, "left")
        if(length(box_prop) > 0 & proportion){
          fill_clr = fill_start_clr[i,]
          # Get a color in between using colorRampPalette
          # The color is a mix of the two colors
          transition_clr = rev(colorRampPalette(fill_clr)(101))[1+ceiling(box_prop[i,1]*100)]
          txt_clr = txt_start_clr[i,]
          prop = box_prop[i, 1]
        }else{
          prop = NA
          fill_clr = fill_start_clr[i]
          transition_clr = fill_clr
          txt_clr = txt_start_clr[i]
        }
        
        if (plot_arrows){
          # Plot arrows
          if (lwd_prop_total)
            max_flow <- max(transition_flow)
          else
            max_flow <- sum(transition_flow[i,])
          
          # Do the background arrows
          plotArrows(type = ifelse(type_of_arrow == "grid", "grid", "simple"),
            box_row = i,
            max_flow = max_flow,
            min_lwd = min_lwd,
            max_lwd = max_lwd,
            clr = rep(overlap_bg_clr, no_boxes*ncol(transition_flow)),
            box_clr = overlap_bg_clr,
            add_width = overlap_add_width)
          
          # The actual arrows
          plotArrows(type = type_of_arrow,
            box_row = i,
            max_flow = max_flow,
            min_lwd = min_lwd,
            max_lwd = max_lwd,
            clr = arrow_clr,
            box_clr = transition_clr,
            add_width = NA)
        }
        
        
        plotBox(bx=bx_left, bx_txt = txt[i, 1], fill=fill_clr, txt_clr = txt_clr, proportion = prop) 
      }
      
      if (prop_end_sizes[i] > 0){
        bx_right <- getBoxPositions(i, "right")
        
        if(length(box_prop) > 0 & proportion){
          fill_clr = fill_end_clr[i,]
          txt_clr = txt_end_clr[i,]
          prop = box_prop[i, 2]
        }else{
          prop = NA
          fill_clr = fill_end_clr[i]
          txt_clr = txt_end_clr[i]
        }
        
        plotBox(bx=bx_right, bx_txt = txt[i, 2], fill=fill_clr, txt_clr = txt_clr, proportion = prop)
      }
    }
  }
  
  # Do the plot
  # Plot shadow boxes a little shifted
  shift <- .01
  vp1 <- viewport(x = 0.5+shift, y = 0.5-shift, height=1-shift*2, width=1-shift*2, name="shadow_boxes")
  pushViewport(vp1)
  
  shadow_clr <- rep(grey(.8), length.out=no_boxes)
  plotBoxes(no_boxes, 
            box_width, 
            txt = matrix("", nrow=no_boxes, ncol=2), # Don't print anything in the shadow boxes
            fill_start_clr = shadow_clr, 
            fill_end_clr  = shadow_clr,
            line_col=shadow_clr[1],
            plot_arrows = FALSE,
            proportion = FALSE)
  popViewport()

  # Plot real boxes
  vp1 <- viewport(x = 0.5, y = 0.5, height=1, width=1)
  pushViewport(vp1)
  plotBoxes(no_boxes, box_width, 
            txt = box_txt,
            fill_start_clr = fill_start_box, 
            fill_end_clr  = fill_end_box,
            plot_arrows = TRUE,
            proportion = TRUE)
  
  popViewport()

  if (!is.null(main) && nchar(main) > 0){
    popViewport()
  }
  
  if (!is.null(box_label) && length(box_label) == 2){
    popViewport()
  }
}
