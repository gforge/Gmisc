#' A reference class for generating transition plots
#'
#' This class simplifies the creating of transition plots. It also
#' allows for advanced multi-column transitions.
#'
#' Transition plots are a type of \emph{Sankey diagrams}. These are a specific type
#' of flow diagram, in which the width of the arrows is shown proportionally
#' to the flow quantity. See \href{https://en.wikipedia.org/wiki/Sankey_diagram}{Wikipedia}
#' for details.
#'
#' @field id Optional id. The render uses named viewports that require a unique id if multiple transition plots
#'  are combined. In order to avoid having overlapping graphs we need to generate a unique id for each viewport
#'  and thus this variable exists. If left empty it will create a counter that is stored in the \code{\link[base]{options}}
#'  (\code{"Gmisc.transitionClassCounter"}) and each viewport will have the name preceded with \code{tc_[0-9]+}. Set this
#'  if you intend to use \code{\link[grid:viewports]{seekViewport}}.
#' @field transitions This is a >= 3 dimensional array with the transitions. Should not be directly accessed.
#' @field box_width The box width
#' @field box_txt The texts of each box
#' @field box_label Box labels
#' @field box_label_pos The label's positions, either "top"/"bottom"
#' @field box_label_cex The size of the box labels
#' @field box_cex The font-size multiplier for the text within the boxes
#' @field arrow_type The type of arrow to use, defaults to "gradient", but can also be "simple".
#'  The corresponding functions are \code{\link{bezierArrowGradient}}, and
#'  \code{\link{bezierArrowSmpl}}. \emph{Note} The \code{bezierGrob} has been deprecated
#'  as it is no longer faster than the bezier arrows and there is a difference in design.
#' @field arrow_clr The arrow color
#' @field arrow_rez The resolution of the arrow
#' @field vertical_space The space between the boxes
#' @field fill_clr The box fill color
#' @field clr_bar Shows a color bar if there are proportions. Can be \code{"none"}, \code{"top"}, \code{"bottom"}
#' @field clr_bar_clrs Extracts the colors for the color bar from the \code{fill_clr} if none is provided
#' @field clr_bar_cex The size of the ticks in the color bar
#' @field clr_bar_subspace  If little or no difference exists at the low/high proportions of
#'  the spectrum then it can be of interest to focus the color change to the center leaving the tails constant
#' @field clr_bar_labels The labels of the color bars. Defaults to the dim names for the proportions.
#' @field txt_clr The text color within the boxes
#' @field title The plot title if any
#' @field title_cex The font-size multiplier for the title
#' @field skip_shadows Skip the shadow effect on the boxes
#' @field mar The margins for the plot.
#' @field min_lwd The minimum line width that is still shown. The pixels will most likely
#'  not have the same fine resolution as the data and you therefore may want to hide
#'  lines that are smaller than a certain amount.
#' @field max_lwd The maximum line width to show
#' @field lwd_prop_type The line can either be proportional to the \code{"set"} of transitions
#'  (group of two box columns), to \code{"all"} transitions, or to each \code{"box"}. It defaults
#'  to \code{"all"}.
#' @field data Internal storage variable. Should not be accessed directly.
#'
#' @import magrittr
#' @importFrom methods setRefClass new
#' @import abind
#' @importFrom utils head tail
#' @importFrom grDevices grey as.raster
#' @example inst/examples/transitionClass_example.R
#' @export
Transition <-
  setRefClass(
    "Transition",
    fields = list(
      id = "character",
      data = "list",
      transitions = function(value) {
        if (missing(value)) {
          return(data$transitions)
        }

        if (is.null(attr(value, "transition"))) {
          stop("You are only allowed to use new()/addTransitions() method for setting the transitions")
        }
        if (!is.list(value)) {
          value <- list(value)
        }
        dims <- sapply(value, function(mtrx) length(dim(mtrx)))
        if (!all(dims == dims[1])) {
          stop(
            "Your transition matrices have different dimensions: ",
            paste(dims, collapse = ":")
          )
        }
        if (!dims[1] %in% 2:3) {
          stop(
            "The dimension of the transition matrix has to be between 2 and 3.",
            " This means that the stored transitions should have an additional dimension",
            " in order to allow for multiple transitions."
          )
        }
        data$transitions <<- value
      },
      box_width = function(value) {
        if (missing(value)) {
          return(data$box_width)
        }

        if (!inherits(value, "unit") &&
          value > 1 && value < 0) {
          stop("The box width must be grid::unit or a double between 0 and 1")
        }
        if (inherits(value, "unit")) {
          raw_width <- convertUnit(value, unitTo = "npc", axisFrom = "x", axisTo = "x", valueOnly = TRUE)
        } else {
          raw_width <- value
          value <- unit(value, "npc")
        }

        if (raw_width * .self$noCols() * 1.1 > 1) {
          stop("Your box_width leaves less than 10% for arrows assuming you have the current plot size")
        }

        data$box_width <<- value
      },
      box_txt = function(value) {
        if (missing(value)) {
          return(data$box_txt)
        }

        if (!is.list(value)) {
          stop("The value has to be a list")
        }

        if (length(value) != .self$noCols()) {
          stop(
            "Your labels should match the number of rows within the transition matrix.",
            " You have currently ", length(value), " sets of labels while there are ",
            .self$noCols(), " columns"
          )
        }

        for (i in 1:length(value)) {
          if (length(value[[i]]) != .self$noRows(i)) {
            stop(
              "Your labels should match the number of rows within the transition matrix",
              " This is not the case for column no. ", i, " that is ", .self$noRows(i),
              " rows while you've provided ", length(value[[i]]), " texts."
            )
          }
        }

        data$box_txt <<- value
      },
      box_label = function(value) {
        if (missing(value)) {
          return(data$box_label)
        }

        if (length(value) != .self$noCols()) {
          stop(
            "You have provided '", length(value), "' box labels",
            " while there are '", .self$noCols(), "' columns that require a label."
          )
        }
        data$box_label <<- as.character(value)
      },
      box_label_pos = function(value = c("top", "bottom")) {
        if (missing(value)) {
          if (!is.null(data$box_label_pos)) {
            return(data$box_label_pos)
          }

          return("top")
        }

        data$box_label_pos <<- match.arg(value)
      },
      box_label_cex = function(value) {
        if (missing(value)) {
          if (!is.null(data$box_label_cex)) {
            return(data$box_label_cex)
          }

          label_cex <- box_cex * 1.2
          raw_width <- convertWidth(box_width, unitTo = "npc", valueOnly = TRUE)

          for (lab in box_label) {
            r_lab_width <- convertX(unit(1, "strwidth", data = lab), "npc", valueOnly = TRUE)
            size_ratio <- r_lab_width * label_cex * 1.05 / raw_width
            if (size_ratio > 1) {
              label_cex <- label_cex / size_ratio
            }
          }

          # Once used it should save the value for consistency
          data$box_label_cex <<- label_cex

          return(label_cex)
        }

        if (!is.numeric(value) && !is.null(value)) {
          stop("Only numeric cex values are accepted")
        }

        data$box_label_cex <<- value
      },
      box_cex = function(value) {
        if (missing(value)) {
          if (!is.null(data$box_cex)) {
            return(data$box_cex)
          }
          if (is.null(data$box_txt)) {
            return(1)
          }

          base_width <-
            box_txt %>%
            unlist() %>%
            as.vector() %>%
            lapply(function(x) strsplit(x, "\n")[[1]]) %>%
            unlist() %>%
            sapply(function(txt) {
              textGrob(label = txt, gp = gpar(cex = 1)) %>%
                grobWidth() %>%
                convertWidth(unitTo = "npc", valueOnly = TRUE)
            }) %>%
            max()

          width_cex <- convertWidth(box_width, unitTo = "npc", valueOnly = TRUE) * .8 / base_width

          min_height <- Inf
          for (col in 1:.self$noCols()) {
            proportions <- .self$getYProps(col)
            proportions <- proportions[proportions > 0]
            min_height %<>%
              min(proportions)
          }
          max_txt_height <- min_height * .6
          base_height <-
            textGrob(label = "A", gp = gpar(cex = 1)) %>%
            grobHeight() %>%
            convertHeight(unitTo = "npc", valueOnly = TRUE)
          height_cex <- max_txt_height / base_height

          return(max(.75, min(width_cex, height_cex)))
        }

        if (value < 0 && !is.null(value)) {
          stop("The cex is a multiplier of the font size and has to be at minimum 0")
        }

        data$box_cex <<- value
      },
      arrow_type = function(value = c("gradient", "simple")) {
        if (missing(value)) {
          if (!is.null(data$arrow_type)) {
            return(data$arrow_type)
          }
          return("gradient")
        }

        data$arrow_type <<- match.arg(value)
      },
      arrow_clr = function(value) {
        if (missing(value)) {
          if (!is.null(data$arrow_clr)) {
            return(data$arrow_clr)
          }
          clrs <- list()
          for (i in 1:length(transitions)) {
            clrs <- c(
              clrs,
              list(matrix("#000000",
                nrow = nrow(transitions[[i]]),
                ncol = ncol(transitions[[i]])
              ))
            )
          }
          return(clrs)
        }

        if (is.matrix(value) && .self$noCols() == 2) {
          if (!all(dim(value) != transitions[[1]])) {
            stop(
              "If you provide an arrow color matrix it must have the same",
              " rows and columns that your transition matrix has: ", paste(.self$getDim(), collapse = "-"),
              " You have provided a matrix of the dimensions: ", paste(dim(value), collapse = "-")
            )
          }
          value <- list(value)
        } else if (!is.list(value) &&
          length(value) == 1) {
          clrs <- list()
          for (i in 1:length(transitions)) {
            clrs <- c(
              clrs,
              list(matrix(value,
                nrow = nrow(transitions[[i]]),
                ncol = ncol(transitions[[i]])
              ))
            )
          }
          value <- clrs
        } else if (is.vector(value) && .self$noCols() == 2) {
          if (length(value) != .self$noRows(1)) {
            stop("Could not match the number of rows in the transition matrix")
          }

          value <- matrix(value, nrow = .self$noRows(1), ncol = .self$noRows(2))
        } else if (is.list(value)) {
          if (length(value) != length(transitions)) {
            stop("The list length for the arrow colors should match the no of transition matrices")
          }

          for (i in 1:length(transitions)) {
            if (nrow(value[[i]]) != nrow(transitions[[i]])) {
              stop("In the ", i, " transitions there is a row mismatch")
            }

            if (ncol(value[[i]]) != ncol(transitions[[i]])) {
              stop("In the ", i, " transitions there is a column mismatch")
            }
          }
        } else {
          stop(
            "The color length '", length(value), "'",
            " could not be interpreted"
          )
        }

        data$arrow_clr <<- value
      },
      arrow_rez = function(value) {
        if (missing(value)) {
          if (!is.null(data$arrow_rez)) {
            return(data$arrow_rez)
          }
          return(400)
        }

        if (length(value) != 1 ||
          value < 1 ||
          round(value) != value) {
          stop("Invalid arrow resolution, needs to be integer between 1 -> Inf")
        }

        data$arrow_rez <<- value
      },
      vertical_space = function(value) {
        if (missing(value)) {
          if (is.null(data$vertical_space)) {
            return(unit(.1, "npc"))
          }

          return(data$vertical_space)
        }

        if (!inherits(value, "unit") &&
          value >= 1 && value < 0) {
          stop("The box width must be grid::unit or a double at least 0 and below 1")
        }
        if (inherits(value, "unit")) {
          raw_space <- convertUnit(value, unitTo = "npc", axisFrom = "y", axisTo = "y", valueOnly = TRUE)
          if (raw_space >= 1) {
            stop("Using your current graph size the provided value is as large as the graph")
          }
          if (raw_space < 0) {
            stop("You cannot have empty space smaller than 0")
          }
        } else {
          value <- unit(value, "npc")
        }

        data$vertical_space <<- value
      },
      fill_clr = function(value) {
        if (missing(value)) {
          return(data$fill_clr)
        }

        value <- prTcValidateAndPrepClr(value,
          no_cols = .self$noCols(),
          no_rows = .self$noRows(),
          is3D = .self$is3D()
        )

        data$fill_clr <<- value
      },
      txt_clr = function(value) {
        if (missing(value)) {
          return(data$txt_clr)
        }

        value <- prTcValidateAndPrepClr(value,
          no_cols = .self$noCols(),
          no_rows = .self$noRows(),
          is3D = .self$is3D()
        )

        data$txt_clr <<- value
      },
      clr_bar = function(value = c("none", "bottom", "top")) {
        if (missing(value)) {
          # Only show bar if there is actually something to show
          if (.self$is3D()) {
            if (arrow_type == "gradient" &&
              length(clr_bar_clrs) == 2) {
              if (!is.null(data$clr_bar)) {
                return(data$clr_bar)
              }

              return("bottom")
            }
          }

          return("none")
        }

        data$clr_bar <<- match.arg(value)
      },
      clr_bar_clrs = function(value) {
        if (missing(value)) {
          if (!is.null(data$clr_bar_clrs)) {
            return(data$clr_bar_clrs)
          }

          if (.self$is3D()) {
            return(fill_clr[[1]][1, ])
          }

          return(fill_clr[[1]][1])
        }

        data$clr_bar_clrs <<- value
      },
      clr_bar_txt_clr = function(value) {
        if (missing(value)) {
          if (!is.null(data$clr_bar_txt_clr)) {
            return(data$clr_bar_txt_clr)
          }

          if (.self$is3D()) {
            return(txt_clr[[1]][1, ])
          }

          return(rep(txt_clr[[1]][1], length.out = 2))
        }

        data$clr_bar_txt_clr <<- rep(value, length.out = 2)
      },
      clr_bar_cex = function(value) {
        if (missing(value)) {
          if (!is.null(data$clr_bar_cex)) {
            return(data$clr_bar_cex)
          }

          return(box_cex / 3)
        }

        data$clr_bar_cex <<- value
      },
      clr_bar_subspace = function(value) {
        if (missing(value)) {
          if (!is.null(data$clr_bar_subspace)) {
            return(data$clr_bar_subspace)
          }

          return(c(0, 1))
        }

        if (length(value) > 2 &&
          !is.numeric(value)) {
          stop("The color subspace needs to be a numeric vector of length between 1 and 2")
        }
        if (is.list(value)) {
          value <- unlist(value)
        }
        if (any(sapply(value, function(x) (x > 1 || x < 0)))) {
          stop("The color subspace is only defined between 1 and 0")
        }

        if (length(value) == 1) {
          if (value > 0.5) {
            value <- c((1 - value) / 2, 1 - (1 - value) / 2)
          } else {
            value <- c(value, 1 - value)
          }
        } else if (length(unique(value)) == 1) {
          stop("You have provided an undefined color space - the two values should be two unique values between 0 and 1")
        } else {
          value <- sort(value)
        }
        data$clr_bar_subspace <<- value
      },
      clr_bar_labels = function(value) {
        if (missing(value)) {
          if (length(.self$getDim()) == 2) {
            return(NULL)
          }

          if (!is.null(data$clr_bar_labels)) {
            return(data$clr_bar_labels)
          }

          return(dimnames(transitions[[1]])[[3]])
        }

        data$clr_bar_labels <<- value
      },
      title = function(value) {
        if (missing(value)) {
          return(data$title)
        }

        if (is.null(value) ||
          nchar(value) == 0) {
          data$title <<- NULL
        } else {
          data$title <<- value
        }
      },
      min_lwd = function(value) {
        if (missing(value)) {
          if (!is.null(data$min_lwd)) {
            return(data$min_lwd)
          }

          return(unit(1, "pt"))
        }
        data$min_lwd <<- value
      },
      max_lwd = function(value) {
        if (missing(value)) {
          if (!is.null(data$max_lwd)) {
            return(data$max_lwd)
          }

          return(unit(5, "mm"))
        }
        data$max_lwd <<- value
      },
      title_cex = function(value) {
        if (missing(value)) {
          if (!is.null(data$title_cex)) {
            return(data$title_cex)
          }

          return(box_label_cex * 1.2)
        }

        if (!is.numeric(value)) {
          stop("Only numeric cex values are accepted")
        }

        data$title_cex <<- value
      },
      skip_shadows = function(value) {
        if (missing(value)) {
          if (!is.null(data$skip_shadows)) {
            return(data$skip_shadows)
          }

          return(FALSE)
        }

        if (!is.logical(value)) {
          stop("Only logical values are accepted for skip_shadows")
        }

        data$skip_shadows <<- value
      },
      mar = function(value) {
        if (missing(value)) {
          if (!is.null(data$mar)) {
            return(data$mar)
          }
          if (is.null(data$mar)) {
            return(unit(rep(3, times = 4), "mm"))
          }
        }

        if (!is.unit(value)) {
          value <- unit(value, "npc")
        }

        if (length(value) == 1) {
          tmp <- value
          for (i in 1:3) {
            tmp <- unit.c(tmp, value)
          }
          value <- tmp
        }

        if (length(value) != 4) {
          stop("There are 4 margins, you have supplied '", length(value), "'")
        }

        data$mar <<- value
      },
      lwd_prop_type = function(value = c("set", "all", "box")) {
        if (missing(value)) {
          if (!is.null(data$lwd_prop_type)) {
            return(data$lwd_prop_type)
          }

          return("all")
        }

        data$lwd_prop_type <<- match.arg(value)
      }
    ),
    methods = list(
      initialize = function(transitions, label, txt, fill_clr, txt_clr, id, ...) {
        "Set up a Transition object. The \\code{transitions} should be a 2D or 3D matrix
        as defined in the \\code{$addTransitions} section and not as later internally stored."
        if (missing(transitions)) {
          stop("You must provide a transition matrix when creating a Transition object")
        }

        if (is.character(transitions) &&
          all(transitions == "copy")) {
          return(callSuper(...))
        }

        if (missing(id)) {
          option_name <- "Gmisc.transitionClassCounter"
          last_id_no <- getOption(option_name)
          if (is.null(last_id_no)) {
            last_id_no <- 0
          }
          current_id_no <- last_id_no + 1

          args <- list()
          args[option_name] <- 0
          do.call(options, args)
          getOption(option_name)

          .self$id <- paste0("tc_", current_id_no)
        } else if (is.character(id)) {
          .self$id <- id
        } else {
          stop("The id has to be a string, you provided: ", id)
        }

        .self$addTransitions(transitions, label, txt = txt, fill_clr = fill_clr, txt_clr = txt_clr)
        callSuper(...)
      },
      copy = function(shallow = FALSE) {
        "A custom \\code{$copy} function as the initialize requires a transitions argument"
        def <- .refClassDef
        value <- new(def, "copy")
        vEnv <- as.environment(value)
        selfEnv <- as.environment(.self)
        # This object stores everything in the data field
        field <- "data"
        if (shallow) {
          assign(field, get(field, envir = selfEnv), envir = vEnv)
        } else {
          current <- get(field, envir = selfEnv)
          if (is(current, "envRefClass")) {
            current <- current$copy(FALSE)
          }
          assign(field, current, envir = vEnv)
        }
        value
      },
      addTransitions = function(mtrx, label, txt, fill_clr, txt_clr) {
        "Add a transition matrix. The input has to be a numerical matrix between 2 and 3 dimensions.
        If you don't provide the txt field the box' text field will be deduced from the transition matrix'
        dimnames. The fill_clr and txt_clr are passed on to the \\code{addClr} function."

        if (is.null(dimnames(mtrx)) ||
          any(sapply(dimnames(mtrx), is.null))) {
          stop(
            "Your transition matrix must have dimension names.",
            " If you use table() for generating your transition matrix",
            " it should automatically add the names.",
            " This feature is for making sure that the names don't",
            " accidentally differ between matrices."
          )
        }

        if (length(transitions) > 0) {
          #           if (!is.null(attr(mtrx, "transition"))) {
          #             transitions <<- list(mtrx)
          #             return()
          #           }
          if (.self$noRows("last") !=
            nrow(mtrx)) {
            stop(
              "The number of elements within the new matrix must be equal to the previous matrix.",
              " You have provided '", nrow(mtrx), "' elements",
              " while there are previously '", .self$noRows("last"), "' elements."
            )
          } else if (sum(.self$boxSizes("last") - rowSums(mtrx)) > .Machine$double.eps * 10 * nrow(mtrx)) {
            stop(
              "You have provided a transition matrix starting with the sizes ", prPasteVec(rowSums(mtrx)),
              " while the previous transition matrix resulted in sizes ", prPasteVec(.self$boxSizes("last")), ".",
              " These two should be equal."
            )
          }

          # Clean from empty rows/columns
          empty_rows <- rowSums(mtrx) == 0
          empty_cols <- colSums(mtrx) == 0
          if (.self$is3D()) {
            # Merge 2D matrix into 1D-vector
            empty_cols <- rowSums(!empty_cols) == 0
            # This is not an issue with rows
          }
          if (any(empty_rows)) {
            if (.self$is3D()) {
              mtrx <- mtrx[!empty_rows, , , drop = FALSE]
            } else {
              mtrx <- mtrx[!empty_rows, , drop = FALSE]
            }
          }

          if (any(empty_cols)) {
            if (.self$is3D()) {
              mtrx <- mtrx[, !empty_cols, , drop = FALSE]
            } else {
              mtrx <- mtrx[, !empty_cols, drop = FALSE]
            }
          }

          # The new matrix must match up to the previous
          prev_match <-
            rownames(mtrx) %in% colnames(.self$getTransitionSet("last"))
          if (any(!prev_match)) {
            stop(
              "Could not find the names ",
              paste(rownames(mtrx)[!prev_match], collapse = ", "),
              " in the previous transition matrix' column names: ",
              paste(colnames(.self$getTransitionSet("last")), collapse = ", ")
            )
          }

          # Align the column order
          prev_order <-
            sapply(
              rownames(mtrx),
              function(n) which(n == colnames(.self$getTransitionSet("last")))
            )
          if (.self$is3D()) {
            mtrx <- mtrx[order(prev_order), , ]
          } else {
            mtrx <- mtrx[order(prev_order), ]
          }

          # Merge the two transitions
          mtrx <- c(transitions, list(mtrx))

          # Update box width
          raw_width <- convertUnit(box_width, unitTo = "npc", axisFrom = "x", axisTo = "x", valueOnly = TRUE)
          shrinkage <- (.self$noCols() * 2 - 1) / ((.self$noCols() + 1) * 2 - 1)
          bw <- unit(raw_width * shrinkage, units = "npc")
        } else {
          mtrx <- list(mtrx)
          bw <- unit(1 / 4, units = "npc")
        }
        attr(mtrx, "transition") <- TRUE

        # Set the transition matrix and the new width
        # Note that more sanity checks are performed when setting the
        # transitions variable
        transitions <<- mtrx
        box_width <<- bw

        # Now adding a label
        if (missing(label)) {
          label <- .self$noCols()
          if (label == 2) {
            label <- 1:2
          }
        }
        if (.self$noCols() > 2) {
          box_label <<- c(box_label, label)
        } else {
          box_label <<- label
        }

        # Next we add txt
        if (missing(txt)) {
          if (.self$noCols() > 2) {
            txt <- colnames(.self$getTransitionSet("last"))
          } else {
            txt <- list(
              rownames(transitions[[1]]),
              colnames(transitions[[1]])
            )
          }
        } else {
          # Prep txt argument
          if (is.vector(txt)) {
            txt <- list(txt)
          } else if (is.matrix(txt)) {
            if (ncol(txt) == 1) {
              txt <- list(txt[, 1])
            } else if (ncol(txt) == 2) {
              if (.self$noCols() > 2) {
                stop("Can only have two txt columns if this is the first transition matrix")
              }
              txt <-
                list(
                  txt[, 1],
                  txt[, 2]
                )
            } else {
              stop("Invalid number of txt columns")
            }
          }

          # If this is the first text we should have a
          # text for both columns and its assumed to be the
          # same
          if (length(txt) == 1 &&
            .self$noCols == 2) {
            txt <- rep(txt, 2)
          }
        }

        if (!is.list(txt)) {
          txt <- list(txt)
        }

        if (.self$noCols() > 2) {
          box_txt <<- c(box_txt, txt)
        } else {
          box_txt <<- txt
        }

        # Now add the colors
        .self$addClr(
          fill = fill_clr,
          txt = txt_clr
        )

        invisible(mtrx)
      },
      getTransitionSet = function(no, reduce_dim = FALSE) {
        "Gets a specific set of transitions. If the \\code{reduce_dim} is set
        to TRUE it will only return a 2-dimensional matrix even if the original
        has a 3rd proportions dimension"
        if (no == "last") {
          no <- length(transitions)
        }

        if (no >= .self$noCols() ||
          no < 1) {
          stop("You can only select transition sets ranging from 1 to ", .self$noCols() - 1)
        }

        set <- transitions[[no]]
        if (reduce_dim &&
          length(dim(set)) == 3) {
          set <- set[, , 1, drop = FALSE] + set[, , 2, drop = FALSE]
          set <- set[, , 1]
        }
        return(set)
      },
      addClr = function(fill, txt) {
        "Adds colors or extends existing one so that they match the transition matrix.
        The fill corresponds to the fill_clr and txt corresponds to the txt_clr. If
        the colors are missing and the transitions consist of only two columns the default
        colors will be used. If the matrix is being extended and these values are missing the
        values from the previous last column will be used for the default columns."

        if (missing(fill) &&
          .self$noCols() == 2) {
          # This is the color initialization
          if (.self$is3D()) {
            fill <-
              list(
                cbind(
                  rep("#fdc086", .self$noRows(1)),
                  rep("#386cb0", .self$noRows(1))
                ),
                cbind(
                  rep("#fdc086", .self$noRows(2)),
                  rep("#386cb0", .self$noRows(2))
                )
              )
          } else {
            fill <-
              list(
                rep("darkgreen", .self$noRows(1)),
                rep("darkgreen", .self$noRows(2))
              )
          }
        }

        if (is.null(fill_clr)) {
          fill_clr <<- fill
        } else {
          fill_clr <<- prTcMatchClr(fill, fill_clr,
            no_cols = .self$noCols(),
            no_rows = .self$noRows(),
            is3D = .self$is3D()
          )
        }

        if (missing(txt) &&
          .self$noCols() == 2) {
          if (.self$is3D()) {
            txt <-
              list(
                cbind(
                  rep("#000000", .self$noRows(1)),
                  rep("#ffffff", .self$noRows(1))
                ),
                cbind(
                  rep("#000000", .self$noRows(2)),
                  rep("#ffffff", .self$noRows(2))
                )
              )
          } else {
            txt <-
              list(
                rep("#ffffff", .self$noRows(1)),
                rep("#ffffff", .self$noRows(2))
              )
          }
        }

        if (is.null(txt_clr)) {
          txt_clr <<- txt
        } else {
          txt_clr <<- prTcMatchClr(txt, txt_clr,
            no_cols = .self$noCols(),
            no_rows = .self$noRows(),
            is3D = .self$is3D()
          )
        }
      },
      getDim = function() {
        "Gets the current dimensions of the transitions"
        transition_dim <- dim(transitions[[1]])
        if (length(transitions) > 1) {
          if (all(sapply(transitions, nrow) == transition_dim[1])) {
            transition_dim[2] <- length(transitions) + 1
          } else {
            transition_dim[2] <- "x"
          }
        }
        return(transition_dim)
      },
      is3D = function() {
        return(length(.self$getDim()) == 3)
      },
      noRows = function(no) {
        "Gets the number of boxes in each row.
        If multiple rows the number of rows may differ
        betwen each transition matrix we therefore need
        to specify what transitions that we refer to.
        If no value is specified it returns all of them."
        no_rows <-
          c(
            sapply(transitions, nrow),
            ncol(tail(transitions, 1)[[1]])
          )

        if (missing(no)) {
          return(no_rows)
        }

        if (no == "last") {
          return(tail(no_rows, 1))
        }

        if (no < 0 ||
          no > length(transitions) + 1) {
          stop("Invalid column specified")
        }

        return(no_rows[no])
      },
      noCols = function() {
        "Gets the number of columns, i.e. the number of transitions"
        return(length(transitions) + 1)
      },
      arrowWidths = function(set_no, add_width) {
        "Retrieves the details regarding arrow sizes for each arrow within the transition
        group"
        trnstn_set <- .self$getTransitionSet(set_no, reduce_dim = TRUE)

        # Get the maximum transition within the entire plot
        if (lwd_prop_type == "all") {
          max_flow <- -Inf
          for (i in 2:.self$noCols()) {
            mtrx <- trnstnSizes(i - 1)
            max_flow <- max(max_flow, mtrx)
          }
        } else if (lwd_prop_type == "set") {
          max_flow <- max(trnstn_set)
        }

        internal.unit <- "mm"
        raw_max_lwd <- max_lwd
        if (is.unit(raw_max_lwd)) {
          raw_max_lwd <- convertHeight(raw_max_lwd,
            unitTo = internal.unit,
            valueOnly = TRUE
          )
        }
        raw_min_lwd <- min_lwd
        if (is.unit(raw_min_lwd)) {
          raw_min_lwd <- convertHeight(raw_min_lwd,
            unitTo = internal.unit,
            valueOnly = TRUE
          )
        }
        arrows <- list()
        for (org_row in 1:.self$noRows(set_no)) {
          if (lwd_prop_type == "box") {
            max_flow <- max(trnstn_set[org_row, ])
          }

          arrows[[org_row]] <-
            lapply(1:.self$noRows(set_no + 1), function(x) list())

          for (targ_row in 1:.self$noRows(set_no + 1)) {
            # Calculate line width
            lwd <- raw_max_lwd * trnstn_set[org_row, targ_row] / max_flow
            if (lwd < raw_min_lwd) {
              # Lines that are below the minimum should not be shown
              next
            }

            adjusted_lwd <- lwd
            if (!missing(add_width)) {
              if ("unit" %in% class(add_width)) {
                adjusted_lwd <- convertY(unit(lwd, "npc") + add_width, unitTo = "npc", valueOnly = TRUE)
              } else if (add_width > 1) {
                adjusted_lwd <- lwd * add_width
              }
            }

            arrows[[org_row]][[targ_row]]$lwd <- unit(adjusted_lwd, internal.unit)
            arrows[[org_row]][[targ_row]]$adj_lwd <- unit(adjusted_lwd, internal.unit)
          }
        }
        return(arrows)
      },
      trnstnSizes = function(set_no) {
        "Gets the transitions per box as a 2D matrix. For the proportions
         it also adds an attribute \\code{attr('props', prop_mtrx)} that
         is a 2D matrix with the corresponding proportions."
        if (set_no >= .self$noCols()) {
          return(NULL)
        }

        mtrx <- .self$getTransitionSet(set_no)
        if (.self$is3D()) {
          props <- mtrx[, , 1] / (mtrx[, , 1] + mtrx[, , 2])
          # Remove the third dimension
          mtrx <- mtrx[, , 1] + mtrx[, , 2]
          attr(mtrx, "props") <- props
        }

        return(mtrx)
      },
      boxSizes = function(col) {
        "Gets the size of the boxes. The \\code{col} argument should
        be either an integer or 'last'"
        if (is.character(col)) {
          if (col == "last") {
            col <- .self$noCols()
          }
        } else if (is.numeric(col)) {
          if (!col %in% 1:.self$noCols()) {
            stop(
              "The column must be within the available columns 1 to ", .self$noCols(),
              " while you have requested '", col, "'"
            )
          }
        }

        if (col == .self$noCols()) {
          # Get last transition matrix and extract the column sums from that one
          mtrx <- .self$getTransitionSet("last")
          if (.self$is3D()) {
            raw_sizes <- apply(mtrx, 3, colSums)
            sizes <- rowSums(raw_sizes)
            attr(sizes, "prop") <-
              raw_sizes[, 1] / sizes
            return(sizes)
          } else if (length(.self$getDim()) == 2) {
            return(colSums(mtrx))
          } else {
            stop("Invalid dimensionality of transition matrix: '", paste(.self$getDim(), collapse = "', '"), "'")
          }
        } else {
          mtrx <- .self$getTransitionSet(col)
          if (.self$is3D()) {
            raw_sizes <- apply(mtrx, 3, rowSums)
            sizes <- rowSums(raw_sizes)
            attr(sizes, "prop") <-
              raw_sizes[, 1] / sizes
            return(sizes)
          } else if (length(.self$getDim()) == 2) {
            return(rowSums(mtrx))
          } else {
            stop("Invalid dimensionality of transition matrix: '", paste(.self$getDim(), collapse = "', '"), "'")
          }
        }
      },
      getYProps = function(col) {
        "Gets the proportions after removing the \\code{vertical_space}
         between the boxes"
        vertical_sizes <- .self$boxSizes(col)
        (1 - convertY(vertical_space,
          unitTo = "npc",
          valueOnly = TRUE
        )) *
          vertical_sizes / sum(vertical_sizes)
      },
      boxPositions = function(col) {
        "The box positions as a list with scalars for the positions:
        \\enumerate{
         \\item \\emph{x} The center x-position
         \\item \\emph{y} The center y-position
         \\item \\emph{right} The right edge
         \\item \\emph{left} The left edge
         \\item \\emph{top} The top edge
         \\item \\emph{bottom} The bottom edge
         \\item \\emph{height} The box height
         \\item \\emph{width} The box width
         \\item \\emph{unit} The unit used for the values (npc)
        }"
        raw_width <- convertWidth(box_width, unitTo = "npc", valueOnly = TRUE)
        space_between <- (1 - raw_width * .self$noCols()) / (.self$noCols() - 1)
        x_offset <- (raw_width + space_between) * (col - 1)
        proportions <- .self$getYProps(col)
        raw_v_space <- convertY(vertical_space,
          unitTo = "npc",
          valueOnly = TRUE
        )
        y_offset <- 1
        bx <- list()
        for (i in 1:length(proportions)) {
          bx[[i]] <-
            list(
              # Center
              y = y_offset - proportions[i] / 2,
              x = x_offset + raw_width / 2,
              # Borders
              left = x_offset,
              right = x_offset + raw_width,
              top = y_offset,
              bottom = y_offset - proportions[i],
              # Size
              height = proportions[i],
              width = raw_width,
              unit = "npc"
            )
          if (proportions[i] > 0) {
            y_offset <- y_offset - sum(proportions[i], raw_v_space / (sum(proportions > 0) - 1))
          }
        }
        # If there is only one box then we center that box
        if (length(proportions) == 1) {
          box[[1]]$y <- 0.5
          box[[1]]$top <- box[[1]]$height + raw_v_space / 2
          box[[1]]$bottom <- raw_v_space / 2
        }
        return(bx)
      },
      getViewportName = function(name) {
        paste0(.self$id, "_", name)
      },
      render = function(new_page = TRUE) {
        "Call this to render the full graph. The \\code{new_page} argument
        is for creating a new plot, set this to \\code{FALSE}
        if you want to combine this plot with another or if you have
        additional viewports that you intend to use."
        if (new_page) {
          grid.newpage()
        }
        no_upview_ports <- 0

        prPushMarginViewport(
          bottom = mar[1],
          left = mar[2],
          top = mar[3],
          right = mar[4],
          name = .self$getViewportName("main_margins")
        )
        no_upview_ports <- no_upview_ports + 2

        if (!is.null(title)) {
          prGridPlotTitle(title, title_cex, cex_mult = 1)
          no_upview_ports <- no_upview_ports + 2
        }

        raw_width <- convertWidth(box_width, unitTo = "npc", valueOnly = TRUE)
        space_between <- (1 - raw_width * .self$noCols()) / (.self$noCols() - 1)

        if (!is.null(box_label)) {
          raw_height <- 0
          for (i in 1:noCols()) {
            raw_height <-
              unit(1, "strheight", box_label[i]) %>%
              convertHeight(unitTo = "npc", valueOnly = TRUE) %>%
              max(raw_height)
          }

          raw_mm <- convertHeight(unit(raw_height, "npc"), unitTo = "mm", valueOnly = TRUE)

          widths <- c()
          for (i in 1:.self$noCols()) {
            widths %<>% c(raw_width)
            if (i != .self$noCols()) {
              widths %<>% c(space_between)
            }
          }

          box_label_heights <- c(raw_height * box_label_cex * 2.5, 1 - raw_height * 2.5 * box_label_cex)
          label_pos <- 1
          graph_pos <- 2
          if (box_label_pos == "bottom") {
            box_label_heights <- rev(box_label_heights)
            label_pos <- 2
            graph_pos <- 1
          }

          labels <- list()
          for (i in 1:noCols()) {
            labels[[i]] <- textGrob(box_label[i], gp = gpar(cex = box_label_cex))
          }

          pushViewport(viewport(
            layout = grid.layout(
              nrow = 2, ncol = length(widths),
              heights = unit(box_label_heights, "npc"),
              widths = unit(widths, "npc")
            ),
            name = .self$getViewportName("box_label_grid")
          ))
          no_upview_ports <- no_upview_ports + 1

          for (i in 1:.self$noCols()) {
            col_no <- i + (i - 1)
            pushViewport(viewport(
              layout.pos.row = label_pos,
              layout.pos.col = col_no,
              name = .self$getViewportName(paste0("box_label_grid_", label_pos, ":", col_no))
            ))
            grid.draw(labels[[i]])
            upViewport()
          }

          pushViewport(viewport(
            layout.pos.row = graph_pos,
            layout.pos.col = 1:length(widths),
            name = .self$getViewportName(paste0("box_graph_section_", graph_pos, ":1-", col_no))
          ))
          no_upview_ports <- no_upview_ports + 1
        }

        if (clr_bar != "none") {
          bar_height <- unit(1, "cm")
          colorAxis <- xaxisGrob(
            at = c(0, .25, .5, .75, 1),
            label = sprintf("%d %%", c(0, .25, .5, .75, 1) * 100),
            main = FALSE, gp = gpar(cex = clr_bar_cex)
          )

          # Add a little space to the actual height
          axis_height <- grobHeight(colorAxis)

          if (clr_bar == "bottom") {
            axis_height <- axis_height
            clr_bar_heights <- unit.c(
              unit(1, "npc") -
                axis_height -
                bar_height,
              unit(3, "mm"),
              axis_height,
              bar_height
            )
            bar_pos <- 4
            graph_pos <- 1
          } else {
            clr_bar_heights <- unit.c(
              axis_height,
              bar_height,
              unit(3, "mm"),
              unit(1, "npc") -
                axis_height -
                bar_height
            )
            bar_pos <- 2
            graph_pos <- 4
          }

          bar_layout <- grid.layout(
            nrow = 4, ncol = 3,
            heights = clr_bar_heights,
            widths = unit.c(
              box_width,
              unit(1, "npc") -
                box_width -
                box_width,
              box_width
            )
          )

          pushViewport(viewport(
            layout = bar_layout,
            name = .self$getViewportName("Bar_layout")
          ))

          pushViewport(viewport(
            layout.pos.row = bar_pos,
            layout.pos.col = 2,
            name = .self$getViewportName("Color_bar")
          ))

          bar_clrs <- prTpGetColors(clr_bar_clrs, space = clr_bar_subspace)
          grid.raster(t(as.raster(bar_clrs)), width = 1, height = 1, interpolate = FALSE)
          grid.draw(colorAxis)

          if (!is.null(clr_bar_labels)) {
            # The height is actually oblivious to upper case and lower case letters
            lab_height <- convertY(unit(1, units = "strheight", clr_bar_labels), "npc", valueOnly = TRUE)
            lab_cex_adjusted <- 1 / (lab_height * 2)

            lab_margin <- .05
            left <- textGrob(clr_bar_labels[1],
              x = 0 + lab_margin,
              just = "left",
              y = .5,
              gp = gpar(
                cex = lab_cex_adjusted,
                col = clr_bar_txt_clr[1]
              )
            )
            right <- textGrob(clr_bar_labels[2],
              x = 1 - lab_margin,
              just = "right",
              y = .5,
              gp = gpar(
                cex = lab_cex_adjusted,
                col = clr_bar_txt_clr[2]
              )
            )
            grid.draw(left)
            grid.draw(right)
          }
          popViewport()

          pushViewport(viewport(
            layout.pos.row = graph_pos,
            layout.pos.col = 1:3,
            name = .self$getViewportName("Main_exc_bar")
          ))
          no_upview_ports <- no_upview_ports + 2
        }

        shift <- unit(raw_width * .02, "snpc")
        pushViewport(viewport(
          x = unit(0.5, "npc") + shift,
          y = unit(0.5, "npc") - shift,
          height = unit(1, "npc") - shift - shift,
          width = unit(1, "npc") - shift - shift,
          name = .self$getViewportName("shadows")
        ))
        upViewport()
        pushViewport(viewport(
          x = unit(0.5, "npc") - shift,
          y = unit(0.5, "npc") + shift,
          height = unit(1, "npc") - shift - shift,
          width = unit(1, "npc") - shift - shift,
          name = .self$getViewportName("regular")
        ))
        upViewport()


        for (col in 1:.self$noCols()) {
          proportions <- .self$getYProps(col)

          txt <- box_txt[[col]]
          bx_pos <- .self$boxPositions(col)

          box_args <- list(
            box_positions = bx_pos,
            proportions = as.vector(proportions),
            fill = rep(grey(level = .3), times = .self$noRows(col)),
            txt = rep("", times = .self$noRows(col)),
            txt_clr = rep(grey(level = .3), times = .self$noRows(col)),
            cex = box_cex
          )

          if (!.self$skip_shadows) {
            seekViewport(.self$getViewportName("shadows"))
            fastDoCall(prTcPlotBoxColumn, box_args)
            upViewport()
          }

          seekViewport(.self$getViewportName("regular"))
          box_args[["proportions"]] <- proportions
          box_args[["fill"]] <- fill_clr[[col]]
          box_args[["txt"]] <- box_txt[[col]]
          box_args[["txt_clr"]] <- txt_clr[[col]]

          # Output the transitions
          if (col < .self$noCols()) {
            trnstn_set <- .self$getTransitionSet(col)

            prTcPlotArrows(trnstn_set,
              widths = .self$arrowWidths(col),
              type = arrow_type,
              clr = arrow_clr[[col]],
              rez = arrow_rez,
              origin_boxes = bx_pos,
              target_boxes = .self$boxPositions(col + 1),
              left_box_clrs = box_args[["fill"]],
              max_flow = max_flow,
              min_width = min_width,
              max_width = max_width,
              clr_bar_subspace = clr_bar_subspace
            )
          }

          fastDoCall(prTcPlotBoxColumn, box_args)
          upViewport()
        }

        if (no_upview_ports > 0) {
          upViewport(no_upview_ports)
        }
      }
    )
  )
