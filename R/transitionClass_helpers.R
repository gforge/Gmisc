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

prTcGetBoxPositions <- function(x_offset,
                                width,
                                proportions){
  vertical_space <- (1-sum(proportions))/(length(proportions) - 1)
  y_offset <- 1
  raw_width <- convertUnit(width, unitTo = "npc", axisFrom = "x", valueOnly = TRUE)
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
    y_offset <- y_offset - sum(proportions[i], vertical_space)
  }
  return(bx)
}

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