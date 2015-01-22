#' An object for generating transition plots
#'
#' @field transitions This is a 3 dimensional array storing all the transitions.
#'
#' @import magrittr
#' @importFrom methods setRefClass
#' @import abind
#' @export
transitionClass <-
  setRefClass(
    "transitionClass",
    fields = list(data = "list",
                  transitions = function(value){
                    if (missing(value)){
                      return(data$transitions)
                    }

                    if (is.null(attr(value, "transition")))
                      stop("You are only allowed to use new()/addTransitions() method for setting the transitions")
                    if (!is.numeric(value))
                      stop("You have provided a non-numeric matrix: '", typeof(value), "'' of class '", class(value), "'")
                    if (!length(dim(value)) %in% 3:4)
                      stop("The dimensions of the transition matrix has to be between 2 and 3.",
                           " This means that the stored transitions should have an additional dimension",
                           " in order to allow for multiple transitions.")
                    if (ncol(value) != nrow(value))
                      stop("The transtion matrix has to be of equal number of rows and columns.",
                           " If a certain factor level is not present after the transition that level's",
                           " column should sum up to 0. You have provided '", nrow(value), "' rows",
                           " and '", ncol(value), "' columns.")
                    data$transitions <<- value
                  },
                  box_width = function(value){
                    if (missing(value))
                      return(data$box_width)

                    if (!inherits(value, "unit") &&
                          value > 1 && value < 0)
                      stop("The box width must be grid::unit or a double between 0 and 1")
                    if (inherits(value, "unit")){
                      raw_width <- convertUnit(value, unitTo = "npc", axisFrom = "x", axisTo = "x", valueOnly = TRUE)
                    }else{
                      raw_width <- value
                      value <- unit(value, "npc")
                    }

                    if (raw_width * .self$noCols() * 1.1 > 1)
                      stop("Your box_width leaves less than 10% for arrows assuming you have the current plot size")

                    data$box_width <<- value
                  },
                  box_txt = function(value){
                    if (missing(value))
                      return(data$box_txt)

                    if (NROW(value) != .self$noRows())
                      stop("Your labels should match the number of rows within the transition matrix")
                    if (is.matrix(value) &&
                          ncol(value) > 1){
                      if (ncol(value) != .self$noCols())
                        stop("Your labels need to match the number of columns")
                    }else{
                      value <- matrix(value,
                                      nrow = .self$noRows(),
                                      ncol = .self$noCols())
                    }
                    data$box_txt <<- value
                  },
                  colnames = function(value){
                    if (missing(value))
                      return(data$colnames)

                    if (length(value) != .self$noCols())
                      stop("Your column names (colnames) should match the number of columns")

                    data$colnames <<- value
                  },
                  vertical_space = function(value){
                    if (missing(value))
                      return(data$vertical_space)

                    if (!inherits(value, "unit") &&
                         value >= 1 && value < 0)
                      stop("The box width must be grid::unit or a double at least 0 and below 1")
                    if (inherits(value, "unit")){
                      raw_space <- convertUnit(value, unitTo = "npc", axisFrom = "y", axisTo = "y", valueOnly = TRUE)
                      if (raw_space >= 1)
                        stop("Using your current graph size the provided value is as large as the graph")
                      if (raw_space < 0)
                        stop("You cannot have empty space smaller than 0")
                    }else{
                      value <- unit(value, "npc")
                    }

                    data$vertical_space <<- value
                  },
                  fill_clr = function(value){
                    if (missing(value))
                      return(data$fill_clr)

                    value <- prTcValidateAndPrepClr(value, transitions, .self)

                    data$fill_clr <<- value
                  },
                  txt_clr = function(value){
                    if (missing(value))
                      return(data$txt_clr)

                    value <- prTcValidateAndPrepClr(value, transitions, .self)

                    data$txt_clr <<- value
                  },
                  box_cex = function(value){
                    if (missing(value)){
                      if (!is.null(data$box_cex))
                        return(data$box_cex)
                      if (is.null(box_txt))
                        return(1)

                      all_texts <- unlist(sapply(as.vector(box_txt), function(x) strsplit(x, "\n")[[1]], USE.NAMES = FALSE))
                      longest_txt <- all_texts[which.max(sapply(all_texts, nchar))]
                      base_width <- convertX(grobWidth(textGrob(label = longest_txt, gp = gpar(cex = 1))), unitTo = "npc", valueOnly = TRUE)
                      width_cex <- convertX(box_width, unitTo = "npc", valueOnly = TRUE)*.8/base_width

                      min_height <- Inf
                      for (col in 1:.self$noCols()){
                        min_height %<>%
                          min(getYProps(col))
                      }
                      max_txt_height <- min_height * .6
                      base_height <- convertUnit(grobHeight(textGrob(label = "A", gp = gpar(cex = 1))),
                                                 unitTo = "npc", axisFrom = "y", valueOnly = TRUE)
                      height_cex <- max_txt_height/base_height

                      return(min(width_cex, height_cex))
                    }

                    if (value < 0)
                      stop("The cex is a multiplier of the font size and has to be at minimum 0")

                    data$box_cex <<- value
                  }) ,
    methods = list(
      initialize = function(transitions, fill_clr, txt_clr, ...){
        "Set up a transitionClass object. The \\code{transitions} should be a 2D or 3D matrix
        as defined in the \\code{$addTransitions} section and not as later internally stored."
        if (missing(transitions))
          stop("You must provide a transition matrix when creating a transitionClass object")

        if (is.character(transitions) &&
              all(transitions == "copy"))
          return(callSuper(...))

        .self$addTransitions(transitions)
        if (missing(fill_clr)){
          if (length(.self$getDim()) == 3){
            fill_clr <<- c("#fdc086", "#386cb0")
          }else{
            fill_clr <<- c("darkgreen")
          }
        }else{
          fill_clr <<- fill_clr
        }

        if (missing(txt_clr)){
          if (length(.self$getDim()) == 3){
            txt_clr <<- c("#000000", "#ffffff")
          }else{
            txt_clr <<- c("#ffffff")
          }
        }else{
          txt_clr <<- txt_clr
        }
        callSuper(...)
      },
      copy = function (shallow = FALSE)
      {
        "A custom \\code{$copy} function as the initialize requires a transitions argument"
        def <- .refClassDef
        value <- new(def, "copy")
        vEnv <- as.environment(value)
        selfEnv <- as.environment(.self)
        for (field in names(def@fieldClasses)) {
          if (shallow)
            assign(field, get(field, envir = selfEnv), envir = vEnv)
          else {
            current <- get(field, envir = selfEnv)
            if (is(current, "envRefClass"))
              current <- current$copy(FALSE)
            assign(field, current, envir = vEnv)
          }
        }
        value
      },
      addTransitions = function(mtrx, txt){
        "Add a transition matrix. The input has to be a numerical matrix between 2 and 3 dimensions."

        if (length(transitions) > 0){
          if (!is.null(attr(mtrx, "transition"))){
            transitions <<- mtrx
            return()
          }else if(.self$noRows() !=
                     nrow(mtrx)){
            stop("The number of elements within the new matrix must be equal to the previous matrix.",
                 " You have provided '", nrow(mtrx), "' elements",
                 " while there are previously '", .self$noRows(), "' elements.")
          } else if(sum(.self$boxSizes("last") - rowSums(mtrx)) > .Machine$double.eps*nrow(mtrx)){
            stop("You have provided a transition matrix starting with the sizes ", prPasteVec(rowSums(mtrx)),
                 " while the previous transition matrix resulted in sizes ", prPasteVec(.self$boxSizes("last")), ".",
                 " These two should be equal.")
          }

          mtrx <- abind(transitions, mtrx, along = length(dim(mtrx)) + 1)

          raw_width <- convertUnit(box_width, unitTo = "npc", axisFrom = "x", axisTo = "x", valueOnly = TRUE)
          shrinkage <- (.self$noCols() * 2 - 1)/((.self$noCols() + 1) * 2 - 1)
          bw <- unit(raw_width * shrinkage, units = "npc")
        }else{
          mtrx <- abind(mtrx, along = length(dim(mtrx)) + 1)
          bw <- unit(1/4, units = "npc")
        }
        attr(mtrx, "transition") <- TRUE

        transitions <<- mtrx
        box_width <<- bw
        vertical_space <<- unit(.6/.self$noRows(), units = "npc")

        if (missing(txt)){
          if (all(sapply(dimnames(mtrx), is.null))){
            txt <- rep("", times = nrow(mtrx))
          }else if (sum(!sapply(dimnames(mtrx)[1:2], is.null)) == 1){
            txt <- dimnames(mtrx)[[which(!sapply(dimnames(mtrx)[1:2], is.null))]]
          }else if (.self$noCols() == 2){
            txt <- cbind(rownames(mtrx), colnames(mtrx))
          }else{
            txt <- colnames(mtrx)
          }
        }else if (NROW(txt) != nrow(mtrx)){
          stop("You must provide the same number of txt as rows in the transition matrix")
        }

        if (.self$noCols() > 2){
          box_txt <<- cbind(box_txt, txt)
        }else{
          box_txt <<- txt
        }
        invisible(mtrx)
      },
      getDim = function(){
        "Gets the current dimensions of the transitions"
        return(dim(transitions)[-length(dim(transitions))])
      },
      noRows = function(){
        "Gets the number of boxes in each row"
        return(.self$getDim()[1])
      },
      noCols = function(){
        "Gets the number of columns"
        return(tail(dim(transitions), 1) + 1)
      },
      boxSizes = function(col){
        "Gets the size of the boxes. Generally an integer but you can also set \\code{col = 'last'}."
        if (is.character(col)){
          if(col == "last"){
            col <- .self$noCols()
          }
        }else if(is.numeric(col)){
          if (!col %in% 1:.self$noCols())
            stop("The column must be within the available columns 1 to ", .self$noCols(),
                 " while you have requested '", col, "'")
        }

        if (col == .self$noCols()){
          # Get last transition matrix and extract the column sums from that one
          mtrx <- asub(transitions, tail(dim(transitions), 1), dims = length(dim(transitions)))
          if (length(.self$getDim()) == 3){
            raw_sizes <- apply(mtrx, 3, colSums)
            sizes <- rowSums(raw_sizes)
            attr(sizes, "prop") <-
              raw_sizes[,1]/sizes
            return(sizes)
          }else if(length(.self$getDim()) == 2){
            return(colSums(mtrx))
          }else{
            stop("Invalid dimensionality of transition matrix: '", paste(.self$getDim(), collapse="', '"), "'")
          }
        }else{
          mtrx <- asub(transitions, col, dims = length(dim(transitions)))
          if (length(.self$getDim()) == 3){
            raw_sizes <- apply(mtrx, 3, rowSums)
            sizes <- rowSums(raw_sizes)
            attr(sizes, "prop") <-
              raw_sizes[,1]/sizes
            return(sizes)
          }else if(length(.self$getDim()) == 2){
            return(rowSums(mtrx))
          }else{
            stop("Invalid dimensionality of transition matrix: '", paste(.self$getDim(), collapse="', '"), "'")
          }
        }
      },
      getYProps = function (col) {
        vertical_sizes <- .self$boxSizes(col)
        (1 - convertUnit(vertical_space, unitTo = "npc", axisFrom = "y", valueOnly = TRUE))*vertical_sizes/sum(vertical_sizes)
      },
      render = function(new_page = TRUE){
        if (new_page)
          grid.newpage()

        raw_width <- convertUnit(box_width, unitTo = "npc", axisFrom = "x", valueOnly = TRUE)
        space_between <- (1- raw_width * .self$noCols())/(.self$noCols() - 1)

        shift <- unit(raw_width*.02, "snpc")
        pushViewport(viewport(x = unit(0.5, "npc")+shift,
                              y = unit(0.5, "npc")-shift,
                              height= unit(1, "npc")-shift-shift,
                              width= unit(1, "npc")-shift-shift, name="shadows"))
        upViewport()
        pushViewport(viewport(x = unit(0.5, "npc")-shift,
                              y = unit(0.5, "npc")+shift,
                              height= unit(1, "npc")-shift-shift,
                              width= unit(1, "npc")-shift-shift,
                              name="regular"))
        upViewport()

        for (col in 1:.self$noCols()){
          proportions <- getYProps(col)

          txt <- box_txt[,col]
          x_offset <- (raw_width + space_between) * (col - 1)

          box_args <- list(x_offset = x_offset,
                           width = box_width,
                           proportions = as.vector(proportions),
                           fill = rep(grey(level = .3), times = .self$noRows()),
                           txt = rep("", times = .self$noRows()),
                           txt_clr = rep(grey(level = .3), times = .self$noRows()),
                           cex = box_cex)
          seekViewport("shadows")
          fastDoCall(prTcPlotBoxColumn, box_args)
          upViewport()

          seekViewport("regular")
          box_args[["proportions"]] <- proportions
          box_args[["fill"]] <- asub(fill_clr, idx = col, dims = 2)
          box_args[["txt"]] <- box_txt[,col]
          box_args[["txt_clr"]] <- asub(txt_clr, idx = col, dims = 2)
          fastDoCall(prTcPlotBoxColumn, box_args)
          upViewport()
        }
      }
    )
  )