#' Plots a column of boxes
#'
#' Takes a set of box settings and plots them.
#'
#' @param box_positions The box positions
#' @param proportions The box vertical proportions (needed for the prop attribute)
#' @param fill The fill colors of length equal to the proportions
#' @param txt The texts of length equal to the proportions
#' @param txt_clr The text colors of length equal to the proportions
#' @param cex The fontsize multiplier
#' @return \code{void}
#' @keywords internal
prTcPlotBoxColumn <- function(box_positions,
                              proportions,
                              fill,
                              txt,
                              txt_clr,
                              cex){
  for (i in 1:length(box_positions)){
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
#' @param rez The arrow rezolution
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
                           type =  c("simple", "gradient"),
                           clr,
                           rez,
                           origin_boxes, target_boxes,
                           left_box_clrs, add_width,
                           max_flow, min_width, max_width,
                           abs_arrow_width = FALSE,
                           clr_bar_subspace){
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
        x_ctrl_points <- c(origin$right,
                           c(origin$right, target$left) %>%
                             mean %>%
                             rep(times=2),
                           target$left)
        arrow_dodge <- seq(from = target$height/6,
                           to = -target$height/6,
                           length.out = sum(!no_trgt))
        exit_pos <- org_row - sum(no_trgt[0:org_row])
        y_ctrl_points <- c(rep(origin$y, 2),
                           rep(target$y + arrow_dodge[exit_pos], 2))
        adjusted_lwd <- convertY(adjusted_lwd, unitTo = target$unit, valueOnly = TRUE)
        # The width can be wider using the special bezier arrows
        if (abs_arrow_width){
          a_width <- entry_length*1.5/sum(!no_trgt)
        }else{
          a_width <-
            adjusted_lwd * 2
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

        adjusted_lwd <- convertY(unit(adjusted_lwd, target$unit), unitTo = "mm")
        arrow_length <- convertY(unit(arrow_length, target$unit), unitTo = "mm")
        a_width <- convertY(unit(a_width, target$unit), unitTo = "mm")
        if (type=="simple" || !is.na(add_width)){
          bezierArrowSmpl(x=x_ctrl_points,
                          y=y_ctrl_points,
                          width=adjusted_lwd,
                          arrow=list(length=arrow_length, base=a_width),
                          clr=arrow_clr,
                          rez = rez) %>%
            grid.draw
        }else{
          if (NCOL(left_box_clrs) == 2){
            current_grdt_clr <- prTpGetColors(colors = left_box_clrs[org_row,],
                                              proportion = 1-transition_arrow_props[org_row, target_row],
                                              space = clr_bar_subspace)
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
                              rez = rez,
                              grdt_type = "triangle",
                              grdt_clr_prop = 0.5,
                              grdt_prop = .8,
                              grdt_decrease_prop = .5,
                              grdt_clr = current_grdt_clr) %>%
            grid.draw
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
#' @keywords internal
prTcValidateAndPrepClr <- function (value, transitions, tcObject) {
  if (is.matrix(value) || is.vector(value)){
    if (length(value) == 1){
      clrs <- list(
        rep(value, nrow(transitions[[1]]))
      )
      for (i in 1:length(transitions)){
        clrs <-
          c(clrs,
            list(rep(value, ncol(transitions[[i]]))))
      }
    }else{
      if (length(transitions) > 1)
        stop("The colors have to be provided as a list if you have more than one",
             " transition matrix")
      if (is.matrix(value)){
        if (!length(dim(value)) == length(dim(transitions[[1]])))
          stop("The color matrix must match the transition matrix.",
               " You have currently a dim of ",
               paste(dim(value), collapse = ":"),
               " while the transition matrix has a dim of",
               paste(dim(transitions[[1]]), collapse = ":"),
               " They have to be of the same length")
        if (ncol(value) == 1){
          value <- cbind(value, value)
        }else if (ncol(value) != 2){
          stop("Your color matrix has incorrect number of columns, 1 or 2 are allowed")
        }
        if (nrow(value) != nrow(transitions[[1]]) ||
            nrow(value) != ncol(transitions[[1]]))
          stop("Your color matrix does not match the transition matrix",
               " rows in value = ", nrow(value),
               " rows in transition matrix = ", nrow(transitions[[1]]),
               " cols in value = ", ncol(value),
               " cols in transition matrix = ", ncol(transitions[[1]]))

        value <- list(list(value[,1],
                           value[,2]))
      } else {
        if (length(value) == 1){
          value <- list(rep(value, nrow(transitions[[1]])),
                        rep(value, ncol(transitions[[1]])))
        }else if (length(value) == 2){
          value <- list(rep(value[1], nrow(transitions[[1]])),
                        rep(value[2], ncol(transitions[[1]])))
        }else if (length(value) == 4){
          if (length(dim(transitions[[1]])) != 3)
            stop("Invalid color vector")
          value <- list(
            cbind(rep(value[1], nrow(transitions[[1]])),
                  rep(value[2], nrow(transitions[[1]]))),
            cbind(rep(value[3], ncol(transitions[[1]])),
                  rep(value[4], ncol(transitions[[1]]))))
        }else{
          stop("Cannot interpret your color values")
        }

      }
    }
  }

  if (!is.list(value))
    stop("Invalud color value")

  if (length(value) != length(transitions) + 1)
    stop("Invalid color length, got ", length(value), " expected ", length(transitions) + 1)

  for (i in 1:length(value)){
    if (is.matrix(value)){
      if (length(dim(transitions[[t]])) != 3 ||
          ncol(value[[i]]) != 2)
        stop("Cannot have matrix in this use case,",
             " the color matrix should be of two columns",
             " if the corresponding transtion matrix has a third dimension")
      if (nrow(value[[i]]) != tcObject$noRows(i))
        stop("The colors don't match the transition object.",
             " ", nrow(value[[i]]), "  != ", tcObject$noRows(i))
    }else{
      if (length(value[[i]]) != tcObject$noRows(i))
        stop("The colors don't match the transition object.",
             " ", length(value[[i]]), "  != ", tcObject$noRows(i))
    }
  }

  return(value)
}
