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
#' @param .self The Transtion-object
#' @return \code{matrix} Returns a matrix of the same size as the
#'  transition matrix
prTcValidateAndPrepClr <- function (value, transitions, .self) {
  if (!is.matrix(value) ||
        !all(dim(value) == dim(transitions))){
    if (length(.self$getDim()) == 3){
      if (length(value) == 1){
        value <- array(value, dim = .self$getDim())
      }else if (is.array(value)){
        if (length(dim(value)) == 3){
          if (dim(value)[3] != 2)
            stop("The third dimension of the color matrix must be of length 2, reflecting the proportions.")
          value <- abind(asub(value, 1, dims = 3)[rep(1:nrow(value), length.out = .self$noRows()),
                                                  rep(1:ncol(value), length.out = .self$noCols())],
                         asub(value, 2, dims = 3)[rep(1:nrow(value), length.out = .self$noRows()),
                                                  rep(1:ncol(value), length.out = .self$noCols())],
                         along = 3)
        }else if (dim(value)[2] == 2){
          value <- abind(value[rep(1:nrow(value), length.out = .self$noRows()),
                               rep(1, length.out = .self$noCols())],
                         value[rep(1:nrow(value), length.out = .self$noRows()),
                               rep(2, length.out = .self$noCols())],
                         along = 3)
        }else{
          stop("Could not interpret the provided color matrix")
        }
      }else if (length(value) == 2){
        value <- abind(matrix(value[1], nrow = .self$noRows(), ncol=.self$noCols()),
                       matrix(value[2], nrow = .self$noRows(), ncol=.self$noCols()),
                       along = 3)
      }else if (length(value) == .self$noCols()){
        if (length(value) != .self$noRows()){
          value <- matrix(value, nrow = .self$noRows(), ncol=.self$noCols(), byrow = TRUE)
        }else{
          value <- matrix(value, nrow = .self$noRows(), ncol=.self$noCols())
        }
      }else if (length(value) == .self$noCols()){
        value <- matrix(value, nrow = .self$noRows(), ncol=.self$noCols(), byrow = TRUE)
      }else{
        stop("The provided colors do not seem to match either rows/columns of the matrix")
      }
    }else{
      if (is.matrix(value)){
        value <- value[rep(1:nrow(value), length.out = .self$noRows()),
                       rep(1:ncol(value), length.out = .self$noCols())]
      }else{
        if (length(value) == 1){
          value <- array(value, dim = .self$getDim())
        }else if (length(value) == .self$noCols()){
          if (length(value) != .self$noRows()){
            value <- matrix(value, nrow = .self$noRows(), ncol=.self$noCols(), byrow = TRUE)
          }else{
            value <- matrix(value, nrow = .self$noRows(), ncol=.self$noCols())
          }
        }else if (length(value) == .self$noCols()){
          value <- matrix(value, nrow = .self$noRows(), ncol=.self$noCols(), byrow = TRUE)
        }else{
          stop("The provided colors do not seem to match either rows/columns of the matrix")
        }
      }
    }
  }

  return(value)
}