#' A transition plot
#'
#' This plot's purpose is to illustrate how states change before and
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
#'  arrows are the \code{\link[grid:grid.bezier]{bezierGrob}} arrows (not that pretty),
#'  simple is the \code{\link{bezierArrowSmpl}} that I've created to get a more exact
#'  control of the arrow position and width, while gradient
#'  corresponds to \code{\link{bezierArrowGradient}}
#'  allowing the arrow to have a fill color that slowly turns into the color of the arrow.
#' @param box_txt The text to appear inside of the boxes. If you need line breaks
#'  then you need to manually add a \\n inside the string.
#' @param tot_spacing The proportion of the vertical space that is to be left
#'  empty. It is then split evenly between the boxes.
#' @param box_width The width of the box. By default the box is one fourth of
#'  the plot width.
#' @param fill_start_box The fill color of the start boxes. This can either
#'  be a single value or a vector if you desire different colors for each
#'  box. If you specify box_prop then this has to be a 2 column matrix.
#' @param txt_start_clr The text color of the start boxes. This can either
#'  be a single value or a vector if you desire different colors for each
#'  box. If you specify box_prop then this has to be a 2 column matrix.
#' @param fill_end_box The fill color of the end boxes. This can either
#'  be a single value or a vector if you desire different colors for each
#'  box. If you specify box_prop then this has to be a 2 column matrix.
#' @param txt_end_clr The text color of the end boxes. This can either
#'  be a single value or a vector if you desire different colors for each
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
#'  a matrix with quantiles for the proportions. Note the size must be \code{nrow(transition_flow) x 2}.
#' @param mar A numerical vector of the form c(bottom, left, top, right) of the type \code{unit()}
#' @param main The title of the plot if any, default \code{NULL}
#' @param box_label A vector of length 2 if you want to label each box column
#' @param box_label_pos The position of the label, either \code{'top'} or \code{'bottom'}
#' @param box_label_cex The cex of the label, defaults to the default cex
#' @param color_bar If you have proportions inside the transition_flow variable
#'  then the color_bar will automatically appear at the bottom unless you set
#'  this to \code{FALSE}
#' @param color_bar_cex The size of the tick labels for the color bar
#' @param color_bar_labels The labels of the two proportions that make up the color bar.
#'  Defaults to the labels of the third dimension for the \code{transition_flow}
#'  argument.
#' @param color_bar_subspace If there is little or no difference
#'  at the low/high proportions of the spectrum then it
#'  can be of interest to focus the color change to the center
#'  leaving the tails constant
#' @param new_page If you want the plot to appear on a new blank page then set this to \code{TRUE}, by
#'  default it is \code{FALSE}.
#' @return void
#' @examples
#' # This example does not run since it
#' # takes a little while to assemble the
#' # arrows and RMD Check complains that this
#' # is more than allowed for
#' library(grid)
#' par_org <- par(ask = TRUE)
#' # Settings
#' no_boxes <- 3
#' # Generate test setting
#' transition_matrix <- matrix(NA, nrow = no_boxes, ncol = no_boxes)
#' transition_matrix[1, ] <- 200 * c(.5, .25, .25)
#' transition_matrix[2, ] <- 540 * c(.75, .10, .15)
#' transition_matrix[3, ] <- 340 * c(0, .2, .80)
#'
#' grid.newpage()
#' transitionPlot(transition_matrix,
#'   box_txt = c("First", "Second", "Third"),
#'   type_of_arrow = "simple",
#'   min_lwd = unit(1, "mm"),
#'   max_lwd = unit(6, "mm"),
#'   overlap_add_width = unit(1, "mm")
#' )
#'
#'
#' # Setup proportions
#' box_prop <- cbind(c(1, 0, 0.5), c(.52, .2, .8))
#' # From the Set2 Colorbrewer
#' start_box_clr <- c("#8DA0CB", "#FC8D62")
#' # Darken the colors slightly
#' end_box_clr <- c(
#'   colorRampPalette(c(start_box_clr[1], "#000000"))(10)[2],
#'   colorRampPalette(c(start_box_clr[2], "#000000"))(10)[2]
#' )
#' # Create a new grid
#' grid.newpage()
#' transitionPlot(transition_matrix,
#'   box_prop = box_prop,
#'   fill_start_box = start_box_clr, fill_end_box = end_box_clr,
#'   txt_start_clr = c("#FFFFFF", "#000000"), txt_end_clr = c("#FFFFFF", "#000000"),
#'   box_txt = c("First", "Second", "Third"),
#'   type_of_arrow = "gradient",
#'   min_lwd = unit(1, "mm"),
#'   max_lwd = unit(10, "mm"),
#'   overlap_add_width = unit(1, "mm")
#' )
#' par(par_org)
#' @import grid
#' @import magrittr
#' @importFrom grDevices grey as.raster
#' @export
transitionPlot <- function(transition_flow,
                           type_of_arrow = c("grid", "simple", "gradient"),
                           box_txt = rownames(transition_flow),
                           tot_spacing = 0.2,
                           box_width = 1 / 4,
                           fill_start_box = "darkgreen",
                           txt_start_clr = "white",
                           fill_end_box = fill_start_box,
                           txt_end_clr = txt_start_clr,
                           cex = 2,
                           min_lwd = if (type_of_arrow == "grid") 1 else unit(.1, "mm"),
                           max_lwd = if (type_of_arrow == "grid") 6 else unit(5, "mm"),
                           lwd_prop_total = TRUE,
                           arrow_clr = "#000000",
                           abs_arrow_width = FALSE,
                           overlap_bg_clr = "#FFFFFF",
                           overlap_order = 1:nrow(transition_flow),
                           overlap_add_width = if (type_of_arrow == "grid") 1.5 else unit(1, "mm"),
                           box_prop,
                           mar = unit(rep(3, times = 4), "mm"),
                           main = NULL,
                           box_label = NULL,
                           box_label_pos = "top",
                           box_label_cex = cex,
                           color_bar = TRUE,
                           color_bar_cex = cex * .33,
                           color_bar_labels,
                           color_bar_subspace = NULL,
                           new_page = FALSE) {
  # Just for convenience
  no_boxes <- nrow(transition_flow)


  # If the matrix is a 3D matrix then the third dimension gives the proportion
  if (length(dim(transition_flow)) > 2) {
    if (length(dim(transition_flow)) > 3) {
      stop(
        "Your transition matrix should be created through:",
        " table(var_a, var_b, var_c) providing a 3D-matrix",
        " you have provided a ", length(dim(transition_flow)), "D matrix."
      )
    }
    if (!missing(box_prop)) {
      stop("You can't have both box_prop and a three dimensional matrix as input")
    }
    if (dim(transition_flow)[3] != 2) {
      stop(
        "Your third dimension should be a proportion,",
        " i.e. a variable with two alternatives.",
        " You have provided a variable with ", dim(transition_flow)[3], " alternatives"
      )
    }

    prop_fn <- function(x) {
      if (x[1] == 0) {
        return(0)
      }
      if (x[2] == 0) {
        return(1)
      }
      return(x[1] / x[2])
    }
    no_1_start <- rowSums(transition_flow[, , 1])
    no_tot_start <- rowSums(transition_flow)
    no_1_end <- colSums(transition_flow[, , 1])
    no_tot_end <- rowSums(colSums(transition_flow[, , 1:2]))
    box_prop <- cbind(
      apply(cbind(no_1_start, no_tot_start), 1, prop_fn),
      apply(cbind(no_1_end, no_tot_end), 1, prop_fn)
    )
    transition_arrow_props <- transition_flow[, , 1] / (transition_flow[, , 1] + transition_flow[, , 2])

    if (color_bar == FALSE) {
      color_bar <- "none"
    } else if (!is.character(color_bar)) {
      color_bar <- "bottom"
    }

    if (missing(color_bar_labels) &&
      !is.null(dimnames(transition_flow))) {
      color_bar_labels <- dimnames(transition_flow)[[3]]
    }

    # Remove the third dimension
    transition_flow <- transition_flow[, , 1] + transition_flow[, , 2]
  } else if (!missing(box_prop)) {
    transition_arrow_props <- t(sapply(box_prop[, 1], function(x) rep(x, no_boxes)))
    color_bar <- "none"
  } else {
    transition_arrow_props <- matrix(1, ncol = no_boxes, nrow = no_boxes)
    color_bar <- "none"
  }

  if (length(arrow_clr) == no_boxes) {
    arrow_clr <- t(sapply(arrow_clr, FUN = function(x) {
      rep(x, ncol(transition_flow))
    }))
  } else if (length(arrow_clr) == 1) {
    arrow_clr <- rep(arrow_clr, no_boxes * ncol(transition_flow))
  }

  if (length(arrow_clr) != no_boxes * ncol(transition_flow)) {
    stop(
      "You have provided an invalid number of arrow colors,",
      " you have ", length(arrow_clr), " colors, while you should provide either 1, ",
      no_boxes, ", or ", no_boxes * ncol(transition_flow), " colors"
    )
  }

  if (length(overlap_order) != no_boxes) {
    stop(
      "You have the wrong number of overlap orders, you provided ",
      length(overlap_order), " while it should be ", no_boxes
    )
  } else if (all(overlap_order %in% 1:no_boxes) == FALSE) {
    stop(
      "Your overlap numbers contain numbers outside the rowrange of",
      " the transition rows, i.e. not between 1 and ", no_boxes
    )
  }

  type_of_arrow <- match.arg(type_of_arrow)
  if (type_of_arrow != "grid") {
    if (!"unit" %in% class(min_lwd) ||
      !"unit" %in% class(max_lwd)) {
      stop("Your line widths must be in units when you specify the alternative arrows, e.g. unit(10, 'pt')")
    }

    # We need to convert these into regular values in order to use
    # them later on in the calculations
    min_lwd <- convertUnit(min_lwd, unitTo = "npc", valueOnly = TRUE)
    max_lwd <- convertUnit(max_lwd, unitTo = "npc", valueOnly = TRUE)
  }

  # Do some sanity checking of the variables
  if (tot_spacing < 0 ||
    tot_spacing > 1) {
    stop(
      "Total spacing, the tot_spacing param,",
      " must be a fraction between 0-1,",
      " you provided ", tot_spacing
    )
  }

  if (box_width < 0 ||
    box_width > 1) {
    stop(
      "Box width, the box_width param,",
      " must be a fraction between 0-1,",
      " you provided ", box_width
    )
  }

  # If the text element is a vector then that means that
  # the names are the same prior and after
  if (is.null(box_txt)) {
    box_txt <- matrix("", ncol = 2, nrow = no_boxes)
  }
  if (is.null(dim(box_txt)) && is.vector(box_txt)) {
    if (length(box_txt) != no_boxes) {
      stop(
        "You have an invalid length of text description, the box_txt param,",
        " it should have the same length as the boxes, ", no_boxes, ",",
        " but you provided a length of ", length(box_txt)
      )
    } else {
      box_txt <- cbind(box_txt, box_txt)
    }
  } else if (nrow(box_txt) != no_boxes ||
    ncol(box_txt) != 2) {
    stop(
      "Your box text matrix doesn't have the right dimension, ",
      no_boxes, " x 2, it has: ",
      paste(dim(box_txt), collapse = " x ")
    )
  }


  if (missing(box_prop)) {
    # Make sure that the clrs correspond to the number of boxes
    fill_start_box <- rep(fill_start_box, length.out = no_boxes)
    txt_start_clr <- rep(txt_start_clr, length.out = no_boxes)
    fill_end_box <- rep(fill_end_box, length.out = no_boxes)
    txt_end_clr <- rep(txt_end_clr, length.out = no_boxes)
  } else {
    fill_start_box <- prTpGetBoxPropClr(fill_start_box,
      no_boxes = no_boxes
    )
    fill_end_box <- prTpGetBoxPropClr(fill_end_box,
      no_boxes = no_boxes
    )
    txt_start_clr <- prTpGetBoxPropClr(txt_start_clr,
      no_boxes = no_boxes,
      lengthOneOK = TRUE
    )
    txt_end_clr <- prTpGetBoxPropClr(txt_end_clr,
      no_boxes = no_boxes,
      lengthOneOK = TRUE
    )

    # Input checks
    if (is.matrix(box_prop) == FALSE) {
      stop("You have to provide the box_prop as a matrix corresponding to the boxes")
    } else if (nrow(box_prop) != no_boxes || ncol(box_prop) != 2) {
      stop(
        "Your box_prop matrix must have ", no_boxes, "x", 2,
        " dimensions, your matrix is currently of ",
        nrow(box_prop), "x", ncol(box_prop), " dimensions"
      )
    } else if (any(box_prop > 1 | box_prop < 0)) {
      stop("You have provided in box_prop invalid quantiles outside the 0-1 range")
    } else if (length(fill_start_box) == 0) {
      stop("You have provided invalid number of fill colors (fill_start_box) when used together with box_prop")
    } else if (length(fill_end_box) == 0) {
      stop("You have provided invalid number of fill colors (fill_end_box) when used together with box_prop")
    } else if (length(txt_start_clr) == 0) {
      stop("You have provided invalid number of text colors (txt_start_clr) when used together with box_prop")
    } else if (length(txt_end_clr) == 0) {
      stop("You have provided invalid number of text colors (txt_end_clr) when used together with box_prop")
    }
  }

  if (nrow(transition_flow) != ncol(transition_flow)) {
    stop(
      "Invalid input array, the matrix is not square but ",
      nrow(transition_flow), " x ", ncol(transition_flow)
    )
  }

  # Set the proportion of the start/end sizes of the boxes
  prop_start_sizes <- rowSums(transition_flow) / sum(transition_flow)
  prop_end_sizes <- colSums(transition_flow) / sum(transition_flow)

  if (sum(prop_end_sizes) == 0) {
    stop("You can't have all empty boxes after the transition")
  }


  if (new_page) grid.newpage()

  # For popViewport at the need to keep track of how
  # many levels we have added
  vp_depth <- 1
  # Add plot margin
  prPushMarginViewport(
    bottom = convertY(mar[1], unitTo = "npc"),
    left = convertX(mar[2], unitTo = "npc"),
    top = convertY(mar[3], unitTo = "npc"),
    right = convertX(mar[4], unitTo = "npc"),
    "main_margins"
  )

  if (!is.null(main) && nchar(main) > 0) {
    prGridPlotTitle(main, cex[1])
    vp_depth %<>% +2
  }

  if (!is.null(box_label) && length(box_label) == 2) {
    left <- prTpGetBoxPositions(
      side = "left", no = 1,
      transitions = transition_flow[1, ],
      prop_start_sizes = prop_start_sizes,
      prop_end_sizes = prop_end_sizes,
      tot_spacing = tot_spacing,
      box_width = box_width
    )
    right <- prTpGetBoxPositions(
      side = "right", no = 1,
      transitions = transition_flow[, 1],
      prop_start_sizes = prop_start_sizes,
      prop_end_sizes = prop_end_sizes,
      tot_spacing = tot_spacing,
      box_width = box_width
    )
    left_label <- textGrob(box_label[1],
      gp = gpar(cex = box_label_cex)
    )
    right_label <- textGrob(box_label[2],
      gp = gpar(cex = box_label_cex)
    )
    label_height <- convertY(max(grobHeight(left_label), grobHeight(right_label)),
      unitTo = "npc", valueOnly = TRUE
    )
    # Add ygjp space and some margin
    label_height <- unit(label_height * 2 + label_height * 0.1, "npc")
    width <- list(
      left = unit(left$right - left$left, "npc"),
      right = unit(right$right - right$left, "npc")
    )
    if (box_label_pos == "top") {
      gl <- grid.layout(
        nrow = 2, ncol = 3,
        heights = unit.c(
          label_height,
          unit(1, "npc") - label_height
        ),
        widths = unit.c(
          width$left,
          unit(1, "npc") -
            width$left -
            width$right,
          width$right
        )
      )
      label_row_no <- 1
      main_row_no <- 2
    } else {
      gl <- grid.layout(
        nrow = 2, ncol = 3,
        heights = unit.c(
          unit(1, "npc") - label_height,
          label_height
        ),
        widths = unit.c(
          width$left,
          unit(1, "npc") -
            width$left -
            width$right,
          width$right
        )
      )
      label_row_no <- 2
      main_row_no <- 1
    }

    # Set layout
    pushViewport(viewport(layout = gl, name = "Label_layout"))

    # Add labels
    pushViewport(viewport(layout.pos.row = label_row_no, layout.pos.col = 1, name = "Left_label"))
    grid.draw(left_label)
    popViewport()
    pushViewport(viewport(layout.pos.row = label_row_no, layout.pos.col = 3, name = "Right_label"))
    grid.draw(right_label)
    popViewport()

    # Set the graph viewport
    pushViewport(viewport(layout.pos.row = main_row_no, layout.pos.col = 1:3, name = "Main_exc_label"))
    vp_depth %<>% +2
  }

  if (color_bar != "none" &&
    type_of_arrow == "gradient") {
    if (color_bar == "bottom") {
      bar_height <- unit(.05, "npc")
      colorAxis <- xaxisGrob(
        at = c(0, .25, .5, .75, 1),
        label = sprintf("%d %%", c(0, .25, .5, .75, 1) * 100),
        main = FALSE, gp = gpar(cex = color_bar_cex)
      )

      # Add a little space to the actual height
      axis_height <- grobHeight(colorAxis) + unit(.01, "npc")
      bar_layout <- grid.layout(
        nrow = 3, ncol = 3,
        heights = unit.c(
          unit(1, "npc") -
            axis_height -
            bar_height,
          axis_height,
          bar_height
        ),
        widths = unit.c(
          unit(box_width, "npc"),
          unit(1, "npc") -
            unit(box_width * 2, "npc"),
          unit(box_width, "npc")
        )
      )

      pushViewport(viewport(layout = bar_layout, name = "Bar_layout"))

      pushViewport(viewport(
        layout.pos.row = 3,
        layout.pos.col = 2,
        name = "Color_bar"
      ))
      bar_clrs <- prTpGetColors(fill_start_box[1, ], space = color_bar_subspace)
      grid.raster(t(as.raster(bar_clrs)), width = 1, height = 1, interpolate = FALSE)
      grid.draw(colorAxis)
      if (!missing(color_bar_labels)) {
        # The height is actually oblivious to upper case and lower case letters
        lab_height <- convertY(grobHeight(textGrob("Ij")), "npc", valueOnly = TRUE)
        lab_cex_adjusted <- 1 / (lab_height * 2)

        if (missing(txt_start_clr)) {
          color_bar_txt_clr <- c("black", "black")
        } else if (ncol(txt_start_clr) == 1) {
          color_bar_txt_clr <- rep(txt_start_clr[1], 2)
        } else {
          color_bar_txt_clr <- txt_start_clr[1, ]
        }

        lab_margin <- .05
        left <- textGrob(color_bar_labels[1],
          x = 0 + lab_margin,
          just = "left",
          y = .5,
          gp = gpar(
            cex = lab_cex_adjusted,
            col = color_bar_txt_clr[1]
          )
        )
        right <- textGrob(color_bar_labels[2],
          x = 1 - lab_margin,
          just = "right",
          y = .5,
          gp = gpar(
            cex = lab_cex_adjusted,
            col = color_bar_txt_clr[2]
          )
        )
        grid.draw(left)
        grid.draw(right)
      }
      popViewport()

      pushViewport(viewport(
        layout.pos.row = 1,
        layout.pos.col = 1:3,
        name = "Main_exc_bar"
      ))
      vp_depth %<>% +2
    } else {
      stop("The color bar position you want, '", color_bar, "', is not yet supported")
    }
  }
  # Do the plot
  # Plot shadow boxes 2 % shifted of the box width
  shift <- box_width * .02
  vp1 <- viewport(x = 0.5 + shift, y = 0.5 - shift, height = 1 - shift * 2, width = 1 - shift * 2, name = "shadow_boxes")
  pushViewport(vp1)

  shadow_clr <- rep(grey(.8), length.out = no_boxes)
  prTpPlotBoxes(
    overlap_order = overlap_order,
    transition_flow = transition_flow,
    no_boxes = no_boxes,
    box_width = box_width,
    tot_spacing = tot_spacing,
    txt = matrix("", nrow = no_boxes, ncol = 2), # Don't print anything in the shadow boxes
    cex = cex,
    prop_start_sizes = prop_start_sizes,
    prop_end_sizes = prop_end_sizes,
    box_prop = box_prop,
    lwd_prop_total = lwd_prop_total,
    fill_start_clr = shadow_clr,
    fill_end_clr = shadow_clr,
    txt_start_clr = txt_start_clr,
    txt_end_clr = txt_end_clr,
    line_col = shadow_clr[1],
    plot_arrows = FALSE,
    proportion = FALSE
  )
  popViewport()

  # Plot real boxes
  vp1 <- viewport(
    x = 0.5 - shift, y = 0.5 + shift,
    height = 1 - shift * 2, width = 1 - shift * 2, name = "actual_boxes"
  )
  pushViewport(vp1)
  prTpPlotBoxes(
    overlap_order = overlap_order,
    transition_flow = transition_flow,
    no_boxes = no_boxes,
    box_width = box_width,
    tot_spacing = tot_spacing,
    txt = box_txt,
    cex = cex,
    prop_start_sizes = prop_start_sizes,
    prop_end_sizes = prop_end_sizes,
    box_prop = box_prop,
    lwd_prop_total = lwd_prop_total,
    fill_start_clr = fill_start_box,
    fill_end_clr = fill_end_box,
    txt_start_clr = txt_start_clr,
    txt_end_clr = txt_end_clr,
    min_lwd = min_lwd,
    max_lwd = max_lwd,
    overlap_add_width = overlap_add_width,
    overlap_bg_clr = overlap_bg_clr,
    type_of_arrow = type_of_arrow,
    abs_arrow_width = abs_arrow_width,
    arrow_clr = arrow_clr,
    transition_arrow_props = transition_arrow_props,
    color_bar_subspace = color_bar_subspace,
    plot_arrows = TRUE,
    proportion = TRUE
  )
  popViewport()

  # Exit margin viewport
  popViewport(vp_depth)
}
