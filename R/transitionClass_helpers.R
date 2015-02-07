#' Plots a column of boxes
#'
#' Takes a set of box settings and plots them.
#'
#' @param box_positions The box positions
#' @param fill The fill colors of length equal to the proportions
#' @param txt The texts of length equal to the proportions
#' @param txt_clr The text colors of length equal to the proportions
#' @param cex The fontsize multiplier
#' @inheritParams prTcGetBoxPositions
#' @return \code{void}
prTcPlotBoxColumn <- function(box_positions,
                              proportions,
                              fill,
                              txt,
                              txt_clr,
                              cex){
  for (i in 1:length(proportions)){
    if (box_positions[[i]]$height == 0)
      next;

    args <- list(bx = box_positions[[i]],
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
#' @param type The type of arrow to be used
#' @param widths The arrow widths
#' @param clr The arrow colors (a matrix)
#' @param min_width The minimum line width
#' @param max_width The maximum line width
#' @param add_width Adds a certain width - useful for background arrows
#' @param origin_boxes The origin boxes' positions to the left
#' @param target_boxes The target boxes' positions to the right
#' @param max_flow The maximum flow if it is common for the entire image
#' @param abs_arrow_width If the arrow width should be the same for all arrows
#' @inheritParams prTcPlotBoxColumn
#' @keywords internal
prTcPlotArrows <- function(trnstn_set,
                           widths,
                           type =  c("grid", "simple", "gradient"),
                           clr,
                           origin_boxes, target_boxes,
                           left_box_clrs, add_width,
                           max_flow, min_width, max_width,
                           abs_arrow_width = FALSE,
                           color_bar_subspace){
  if (length(dim(trnstn_set)) == 3){
    transition_arrow_props <- trnstn_set[,,1]/(trnstn_set[,,1] + trnstn_set[,,2])
    trnstn_set <- (trnstn_set[,,1] + trnstn_set[,,2])
  }
  for (org_row in 1:nrow(trnstn_set)){
    origin <- origin_boxes[[org_row]]

    # Check what arrows that should be skipped
    no_arrows <- sapply(widths[[org_row]], function(x) is.null(x$lwd), USE.NAMES = FALSE)

    # Plot the widest arrow last
    for (target_row in order(trnstn_set[org_row,])){
      if (no_arrows[target_row])
        next;

      # Start with background arrow
      for (add_width in list(unit(2, "pt"), NA)){
        no_trgt <- sapply(widths, function(x) is.null(x[[target_row]]$lwd), USE.NAMES = FALSE)

        target <- target_boxes[[target_row]]
        entry_length <- target$height/3
        arrow_length <- target$width/4

        adjusted_lwd <- widths[[org_row]][[target_row]]$adj_lwd
        x_ctrl_points <- c(origin$right, .5, .5, target$left)
        arrow_dodge <- seq(from = target$height/6,
                           to = -target$height/6,
                           length.out = sum(!no_trgt))
        exit_pos <- org_row - sum(no_trgt[0:org_row])
        y_ctrl_points <- c(rep(origin$y, 2),
                           rep(target$y + arrow_dodge[exit_pos], 2))
        if (type=="grid"){
          current_arrow_clr <-
            if (abs_arrow_width){
              a_width <- entry_length/no_boxes
            }else{
              # Not really sure but points seem to be a reasonable
              # unit for the lwd as a basis for this part
              a_width <- convertHeight(unit(lwd, "pt"), "npc")+
                entry_length/(no_boxes+1)
            }

          # Add line width addition if it is a background line
          if (!is.na(add_width)){
            if (is.unit(add_width)){
              a_width <- a_width + convertY(add_width, unitTo="npc", valueOnly=TRUE)
              adjusted_lwd <- adjusted_lwd + convertY(add_width, unitTo="npc", valueOnly=TRUE)
            }else{
              a_width <- a_width * add_width
              adjusted_lwd <- adjusted_lwd * add_width
            }
            arrow_clr <- "#FFFFFF"
          }else{
            arrow_clr <- clr[org_row, target_row]
          }

          # Need to adjust the end of the arrow as it otherwise overwrites part of the box
          # if it is thick
          x_ctrl_points[4] <- x_ctrl_points[4]-.00075*adjusted_lwd
          grid.bezier(x=x_ctrl_points,
                      y=y_ctrl_points,
                      gp=gpar(lwd=adjusted_lwd,
                              fill=arrow_clr,
                              col=arrow_clr),
                      arrow=arrow(type="closed",
                                  angle=atan((a_width/2)/arrow_length)*180/pi,
                                  length=unit(arrow_length, target$unit)))

        }else{
          adjusted_lwd <- convertY(adjusted_lwd, unitTo = target$unit, valueOnly = TRUE)
          # The width can be wider using the special bezier arrows
          if (abs_arrow_width){
            a_width <- entry_length*1.5/no_boxes
          }else{
            a_width <-
              adjusted_lwd * 5/3
          }

          # Add line width addition if it is a background line
          if (!is.na(add_width)){
            if (is.unit(add_width)){
              a_width <- a_width + convertY(add_width, unitTo="npc", valueOnly=TRUE)
              adjusted_lwd <- adjusted_lwd + convertY(add_width, unitTo="npc", valueOnly=TRUE)
            }else{
              a_width <- a_width * add_width
              adjusted_lwd <- adjusted_lwd * add_width
            }
            arrow_clr <- "#FFFFFF"
          }else{
            arrow_clr <- clr[org_row, target_row]
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

          adjusted_lwd <- unit(adjusted_lwd, target$unit)
          arrow_length <- convertY(unit(arrow_length, target$unit), unit="mm")
          a_width <- convertY(unit(a_width, target$unit), unitTo = "mm")
          if (type=="simple" || !is.na(add_width)){
            bezierArrowSmpl(x=x_ctrl_points,
                            y=y_ctrl_points,
                            width=adjusted_lwd,
                            arrow=list(length=arrow_length, base=a_width),
                            clr=arrow_clr) %>%
              grid.draw
          }else{
            if (NCOL(left_box_clrs) == 2){
              current_grdt_clr <- prTpGetColors(colors = left_box_clrs[org_row,],
                                                proportion = 1-transition_arrow_props[org_row, target_row],
                                                space = color_bar_subspace)
            }else{
              if (is.matrix(left_box_clrs))
                current_grdt_clr <- left_box_clrs[org_row,1]
              else
                current_grdt_clr <- left_box_clrs[org_row]
            }

            bezierArrowGradient(x=x_ctrl_points,
                                y=y_ctrl_points,
                                width=adjusted_lwd,
                                arrow=list(length=arrow_length, base=a_width),
                                clr=arrow_clr,
                                grdt_type = "triangle",
                                grdt_clr_prop = 0.5,
                                grdt_start_prop = .3,
                                grdt_decrease_prop = .3,
                                grdt_clr = current_grdt_clr) %>%
              grid.draw
          }
        }
      }
    }
  }
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