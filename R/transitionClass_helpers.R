#' Plots a column of boxes
#'
#' Takes a set of box settings and plots them.
#'
#' @param fill The fill colors of length equal to the proportions
#' @param txt The texts of length equal to the proportions
#' @param txt_clr The text colors of length equal to the proportions
#' @param cex The fontsize multiplier
#' @inheritParams prTcGetBoxPositions
#' @return \code{void}
prTcPlotBoxColumn <- function(x_offset,
                              width,
                              proportions,
                              fill,
                              txt,
                              txt_clr,
                              cex){
  bx <- prTcGetBoxPositions(x_offset = x_offset,
                            width = width,
                            proportions = proportions)
  for (i in 1:length(proportions)){
    if (bx[[i]]$height == 0)
      next;

    args <- list(bx = bx[[i]],
                 bx_txt = txt[i],
                 cex = cex,
                 line_col = "black",
                 lwd = 1)
    if (!is.null(attr(proportions, "prop"))){
      args[["prop"]] <- attr(proportions, "prop")[i]
      args[["fill"]] <- fill[i,]
      args[["txt_clr"]] <- txt_clr[i,]
    }else{
      args[["fill"]] <- fill[i]
      args[["txt_clr"]] <- txt_clr[i]
    }

    fastDoCall(prTpPlotBox, args)
  }
}

#' Plots the arrows within a single transition set
#'
#' @param trnstn_set The set of transitions to plot
#' @param left_box_clrs The colors of the left side boxes
#' @param arrow_type The type of arrow to be used
#' @param min_width The minimum line width
#' @param max_width The maximum line width
#' @param origin_boxes The origin boxes' positions to the left
#' @param target_boxes The target boxes' positions to the right
#' @param max_flow The maximum flow if it is common for the entire image
#' @inheritParams prTcPlotBoxColumn
#' @keywords internal
prTcPlotArrows <- function(trnstn_set,
                           arrow_type =  c("grid", "simple", "gradient"),
                           origin_boxes, target_boxes,
                           left_box_clrs,
                           max_flow, min_width, max_width){
  for (box_row in 1:nrow(trnstn_set)){
    # Plot the widest arrow last
    for (flow in order(trnstn_set[box_row,])){
      if (trnstn_set[box_row,flow] > 0){
        # Calculate line width
        lwd <- max_lwd*trnstn_set[box_row,flow]/max_flow
        if (lwd < min_lwd){
          message("The minimum width reached and the arrow at box '", box_row, "' to '", flow, "'",
                  " will not be shown. This is due to the fact that the lwd will generate a falsely
                  strong arrow.")
          next;
        }

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
        x_ctrl_points <- c(origin_boxes$right, .5, .5, target_boxes$left)
        y_ctrl_points <- c(origin_boxes$y_exit[flow], origin_boxes$y_exit[flow],
                           target_boxes$y_entry[box_row], target_boxes$y_entry[box_row])
        current_arrow_clr <- clr[(flow+(box_row-1)*no_boxes)]
        if (type=="grid"){
          if (abs_arrow_width){
            a_width <- target_boxes$y_entry_height/no_boxes
          }else{
            # Not really sure but points seem to be a reasonable
            # unit for the lwd as a basis for this part
            a_width <- getGridVal(unit(lwd, "pt"), "npc")+
              target_boxes$y_entry_height/(no_boxes+1)
          }

          # Add line width addition if it is a background line
          if (!is.na(add_width)){
            if (is.unit(add_width)){
              a_width <- a_width + convertY(add_width, unitTo="npc", valueOnly=TRUE)
            }else{
              a_width <- a_width * add_width
            }
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
            a_width <- target_boxes$y_entry_height*1.5/no_boxes
          }else{
            a_width <- # getGridVal(lwd, "npc") +
              target_boxes$y_entry_height*trnstn_set[box_row,flow]/max_flow*2.2
            # Set a maximum size in proportion to the line
            if (getGridVal(lwd, "npc", axisTo="y")*1.66 < a_width)
              a_width <- getGridVal(lwd, "npc", axisTo="y")*1.66
          }

          # Add line width addition if it is a background line
          if (!is.na(add_width)){
            if (is.unit(add_width)){
              a_width <- a_width + convertY(add_width, unitTo="npc", valueOnly=TRUE)
            }else{
              a_width <- a_width * add_width
            }
          }


          if (a_width < adjusted_lwd){
            sp_float_string <- sprintf("%%.%df", -floor(log10(adjusted_lwd-a_width))+1)
            warning("The arrow width is smaller than the width of the line,",
                    "thus not appearing as a regular arrow: ",
                    sprintf(sp_float_string, a_width),
                    " < ",
                    sprintf(sp_float_string, adjusted_lwd))

            # Looks really weird if this is allowed
            a_width <- adjusted_lwd

          }

          if (type=="simple"){
            bz <- bezierArrowSmpl(x=x_ctrl_points,
                                  y=y_ctrl_points,
                                  width=adjusted_lwd,
                                  arrow=list(length=a_l, base=a_width),
                                  clr=current_arrow_clr)
            grid.draw(bz)
          }else if (type=="gradient"){
            if (length(box_clr) > 1){
              # Invert order as that is the fill order
              current_grdt_clr <- prTpGetColors(colors = box_clr,
                                                proportion = 1-transition_arrow_props[box_row, flow],
                                                space = color_bar_subspace)
            }else{
              current_grdt_clr <- box_clr
            }
            bz <- bezierArrowGradient(x=x_ctrl_points,
                                      y=y_ctrl_points,
                                      width=adjusted_lwd,
                                      arrow=list(length=a_l, base=a_width),
                                      clr=current_arrow_clr,
                                      grdt_type = "triangle",
                                      grdt_clr_prop = 0.5,
                                      grdt_start_prop = .3,
                                      grdt_decrease_prop = .3,
                                      grdt_clr = current_grdt_clr)
            grid.draw(bz)

          }else{
            stop("The arrow type ", type, " is not yet implemented, sorry.")
          }
        }
      }
    }

  }
}

#' Gets the box positions
#'
#' The box positions as a list with scalars for the positions:
#' \enumerate{
#'  \item \emph{x} The center x-position
#'  \item \emph{y} The center y-position
#'  \item \emph{right} The right edge
#'  \item \emph{left} The left edge
#'  \item \emph{top} The top edge
#'  \item \emph{bottom} The bottom edge
#'  \item \emph{height} The box height
#'  \item \emph{width} The box width
#' }
#' @param x_offset The x start position of the box
#' @param width The box width
#' @param proportions The vertical proportions that
#'  are \code{(1-sum(proportions)) == total_vertical_space},
#'  i.e. the sum is always <= 1.
#' @return \code{list} Returns a list with all the elements
#'  positions.
prTcGetBoxPositions <- function(x_offset,
                                width,
                                proportions){
  vertical_space <- (1-sum(proportions))/(sum(proportions > 0) - 1)
  y_offset <- 1
  raw_width <- convertX(width, unitTo = "npc", valueOnly = TRUE)
  bx <- list()
  for (i in 1:length(proportions)){
    bx[[i]] <-
      list(
        # Center
        y = y_offset - proportions[i]/2,
        x = x_offset + raw_width/2,
        # Borders
        left = x_offset,
        right = x_offset + raw_width,
        top = y_offset,
        bottom = y_offset - proportions[i],
        # Size
        height = proportions[i],
        width = raw_width)
    if (proportions[i] > 0){
      y_offset <- y_offset - sum(proportions[i], vertical_space)
    }
  }
  # If there is only one box then we center that box
  if (length(proportions) == 1){
    box[[1]]$y = 0.5
    box[[1]]$top = box[[1]]$height + vertical_space/2
    box[[1]]$bottom = vertical_space/2
  }
  return(bx)
}

#' Checks and prepares colors
#'
#' The colors need to match the size of the transition matrix
#' and therefore we need to have this input check. The output is
#' always a matrix of the exact same dimensions as the transition
#' matrix.
#'
#' @param value The color values
#' @param transitions The transition matrix
#' @param tcObject The Transtion-object
#' @return \code{matrix} Returns a matrix of the same size as the
#'  transition matrix
prTcValidateAndPrepClr <- function (value, transitions, tcObject) {
  if (!is.matrix(value) ||
        !all(dim(value) == dim(transitions))){
    if (length(tcObject$getDim()) == 3){
      if (length(value) == 1){
        value <- array(value, dim = tcObject$getDim())
      }else if (is.array(value)){
        if (length(dim(value)) == 3){
          if (dim(value)[3] != 2)
            stop("The third dimension of the color matrix must be of length 2, reflecting the proportions.")
          value <- abind(asub(value, 1, dims = 3)[rep(1:nrow(value), length.out = tcObject$noRows()),
                                                  rep(1:ncol(value), length.out = tcObject$noCols())],
                         asub(value, 2, dims = 3)[rep(1:nrow(value), length.out = tcObject$noRows()),
                                                  rep(1:ncol(value), length.out = tcObject$noCols())],
                         along = 3)
        }else if (dim(value)[2] == 2){
          value <- abind(value[rep(1:nrow(value), length.out = tcObject$noRows()),
                               rep(1, length.out = tcObject$noCols())],
                         value[rep(1:nrow(value), length.out = tcObject$noRows()),
                               rep(2, length.out = tcObject$noCols())],
                         along = 3)
        }else{
          stop("Could not interpret the provided color matrix")
        }
      }else if (length(value) == 2){
        value <- abind(matrix(value[1], nrow = tcObject$noRows(), ncol=tcObject$noCols()),
                       matrix(value[2], nrow = tcObject$noRows(), ncol=tcObject$noCols()),
                       along = 3)
      }else if (length(value) == tcObject$noCols()){
        if (length(value) != tcObject$noRows()){
          value <- matrix(value, nrow = tcObject$noRows(), ncol=tcObject$noCols(), byrow = TRUE)
        }else{
          value <- matrix(value, nrow = tcObject$noRows(), ncol=tcObject$noCols())
        }
      }else if (length(value) == tcObject$noCols()){
        value <- matrix(value, nrow = tcObject$noRows(), ncol=tcObject$noCols(), byrow = TRUE)
      }else{
        stop("The provided colors do not seem to match either rows/columns of the matrix")
      }
    }else{
      if (is.matrix(value)){
        value <- value[rep(1:nrow(value), length.out = tcObject$noRows()),
                       rep(1:ncol(value), length.out = tcObject$noCols())]
      }else{
        if (length(value) == 1){
          value <- array(value, dim = tcObject$getDim())
        }else if (length(value) == tcObject$noCols()){
          if (length(value) != tcObject$noRows()){
            value <- matrix(value, nrow = tcObject$noRows(), ncol=tcObject$noCols(), byrow = TRUE)
          }else{
            value <- matrix(value, nrow = tcObject$noRows(), ncol=tcObject$noCols())
          }
        }else if (length(value) == tcObject$noCols()){
          value <- matrix(value, nrow = tcObject$noRows(), ncol=tcObject$noCols(), byrow = TRUE)
        }else{
          stop("The provided colors do not seem to match either rows/columns of the matrix")
        }
      }
    }
  }

  return(value)
}