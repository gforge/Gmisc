#' An object for generating transition plots
#'
#' @field transitions This is a 3 dimensional array storing all the transitions.
#' @field type_of_arrow The types of arrow may be grid, simple, or gradient. Simple grid
#'  arrows are the \code{\link[grid]{bezierGrob}} arrows (not that pretty),
#'  simple is the \code{\link{bezierArrowSmpl}} that I've created to get a more exact
#'  control of the arrow position and width, while gradient
#'  corresponds to \code{\link{bezierArrowGradient}}
#'  allowing the arrow to have a fill color that slowly turns into the color of the arrow.
#' @field box_txt The text to appear inside of the boxes. If you need line breaks
#'  then you need to manually add a \\n inside the string.
#' @field tot_spacing The proportion of the vertical space that is to be left
#'  empty. It is then split evenly between the boxes.
#' @field box_width The width of the box. By default the box is one fourth of
#'  the plot width.
#' @field fill_start_box The fill color of the start boxes. This can either
#'  be a single value ore a vector if you desire different colors for each
#'  box. If you specify box_prop then this has to be a 2 column matrix.
#' @field txt_start_clr The text color of the start boxes. This can either
#'  be a single value ore a vector if you desire different colors for each
#'  box. If you specify box_prop then this has to be a 2 column matrix.
#' @field fill_end_box The fill color of the end boxes. This can either
#'  be a single value ore a vector if you desire different colors for each
#'  box. If you specify box_prop then this has to be a 2 column matrix.
#' @field txt_end_clr The text color of the end boxes. This can either
#'  be a single value ore a vector if you desire different colors for each
#'  box. If you specify box_prop then this has to be a 2 column matrix.
#' @field cex The cex \code{\link{gpar}} of the text
#' @field min_lwd The minimum width of the line that we want to illustrate the
#'  tranisition with.
#' @field max_lwd The maximum width of the line that we want to illustrate the
#'  tranisition with.
#' @field lwd_prop_total The width of the lines may be proportional to either the
#'  other flows from that box, or they may be related to all flows. This is a boolean
#'  parameter that is set to true by default, i.e. relating to all flows.
#' @field arrow_clr The color of the arrows. Usually black, can be a vector indicating each arrow
#'  from first to last arrow (counting from the top). If the vector is of the same length as the
#'  boxes then all box arrows will have the same color (that is all the arrows stemming from the
#'  left boxes)
#' @field abs_arrow_width The width can either be absolute, i.e. each arrow headed for a box
#'  has the exact same width. The alternative is that the width is related to the line width.
#' @field overlap_bg_clr In order to enhance the 3D perspective and to make it easier
#'  to follow arrows the arrows have a background color to separate them from those underneath.
#' @field overlap_order The order from first->last for the lines. This means that the last
#'  line will be on top while the first one will appear at the bottom. This should be provided
#'  as a vector.
#' @field overlap_add_width The width of the white cross-over line. You can specify this as a scalar
#'  multiplication of the current line width. In case of non-grid arrows then you can also have this
#'  as a unit which is recommended as it looks better. If the scalar is < 1 then the overlap is ignored.
#' @field box_prop If you want the boxes to have proportions indicating some other factors then input
#'  a matrix with quantiles for the proportions. Note the size mus be \code{nrow(transition_flow) x 2}.
#' @field mar A numerical vector of the form c(bottom, left, top, right) of the type \code{unit()}
#' @field main The title of the plot if any, default \code{NULL}
#' @field box_label A vector of length 2 if you want to label each box column
#' @field box_label_pos The position of the label, either \code{'top'} or \code{'bottom'}
#' @field box_label_cex The cex of the label, defaults to the default cex
#' @field color_bar If you have proportions inside the transition_flow variable
#'  then the color_bar will automatically appear at the bottom unless you set
#'  this to \code{FALSE}
#' @field color_bar_cex The size of the tick labels for the color bar
#' @field color_bar_labels The labels of the two proportions that make up the color bar.
#'  Defaults to the labels of the third dimension for the \code{transition_flow}
#'  argument.
#' @field color_bar_subspace If there is little or no difference exists
#'  at the low/high proportions of the spectrum then it
#'  can be of interest to focus the color change to the center
#'  leaving the tails constant
#' @import magrittr
#' @importFrom methods setRefClass
#' @import abind
transitionClass <-
  setRefClass(
    "transitionClass",
    fields = list(transitions = "array"),
    methods = list(
      initialize = function(transitions, ...){
        "Set up a transitionClass object. The \\code{transitions} should be a 2D or 3D matrix
        as defined in the \\code{$addTransitions} section and not as later internally stored."
        if (missing(transitions))
          stop("You must provide a transition matrix when creating a transitionClass object")

        if (is.character(transitions) &&
              all(transitions == "copy"))
          return(callSuper(...))

        .self$addTransitions(transitions)
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
      addTransitions = function(mtrx){
        "Add a transition matrix. The input has to be a numerical matrix between 2 and 3 dimensions."
        if (!is.numeric(mtrx))
          stop("You have provided a non-numeric matrix: '", typeof(mtrx), "'' of class '", class(mtrx), "'")
        if (!length(dim(mtrx)) %in% 2:3)
          stop("The dimensions of the matrix has to be between 2 and 3")
        if (ncol(mtrx) != nrow(mtrx))
          stop("The transtion matrix has to be of equal number of rows and columns.",
               " If a certain factor level is not present after the transition that level's",
               " column should sum up to 0. You have provided '", nrow(mtrx), "' rows",
               " and '", ncol(mtrx), "' columns.")

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
        }else{
          mtrx <- abind(mtrx, along = length(dim(mtrx)) + 1)
        }
        attr(mtrx, "transition") <- TRUE

        transitions <<-
          mtrx
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
        return(tail(.self$getDim(), 1) + 1)
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
          mtrx <- asub(transitions, col, dims = length(.self$getDim()))
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
      }
    )
  )