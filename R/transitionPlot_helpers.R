#' Gets the color for the given box
#'
#' @param clr The color of the boxes, either
#'  a matrix, a vector of length two, or a single color
#' @param no_boxes The number of boxes
#' @param lengthOneOK If it is ok to have only one color
#' @return \code{matrix} With two columns and no_boxes rows.
#'  If function fails then return \code{NULL}
#'
#' @keywords internal
prTpGetBoxPropClr <- function(clr, no_boxes, lengthOneOK = FALSE) {
  if (is.matrix(clr)) {
    if (nrow(clr) == no_boxes &&
      ncol(clr) == 2) {
      return(clr)
    }
  } else if (length(clr) == 2 ||
    (lengthOneOK && length(clr) == 1)) {
    return(matrix(clr, ncol = 2, nrow = no_boxes, byrow = TRUE))
  }

  return(NULL)
}

#' Plots the box for the transition plot
#'
#' @param bx A list with the x, y, height and width parameters
#' @param bx_txt The box text
#' @param fill The fill color
#' @param txt_clr The text color
#' @param cex The font size
#' @param line_col The line color around the box
#' @param lwd The line width
#' @param prop Provide a proportion if the box should be split (0-1)
#' @return \code{void}
#'
#' @keywords internal
prTpPlotBox <- function(bx, bx_txt, fill, txt_clr,
                        cex, line_col, lwd,
                        prop = NA) {
  pushViewport(viewport(
    y = bx$y, x = bx$x,
    height = bx$height, width = bx$width
  ))
  if (is.na(prop)) {
    grid.roundrect(gp = gpar(
      lwd = lwd,
      fill = fill,
      col = line_col
    ))

    if (bx_txt != "") {
      bx_grob <- prTpGetBoxSizedTextGrob(
        txt = bx_txt,
        txt_clr = txt_clr,
        txt_cex = cex
      )
      if (!is.null(bx_grob)) {
        grid.draw(bx_grob)
      }
    }
  } else {
    # Adapted from Paul Murray's example http://www.stat.auckland.ac.nz/~paul/RG2e/customgrid-nestedlay.R
    pushViewport(viewport(layout = grid.layout(
      nrow = 2,
      ncol = 1,
      heights = c(prop, 1 - prop)
    )))
    grid.roundrect(gp = gpar(
      lwd = lwd,
      fill = fill[1],
      col = NA
    ))

    bx_grob <- NULL
    if (bx_txt != "") {
      bx_grob <- prTpGetBoxSizedTextGrob(
        txt = bx_txt,
        txt_clr = txt_clr[1],
        txt_cex = cex
      )
      if (!is.null(bx_grob)) {
        grid.draw(bx_grob)
      }
    }

    pushViewport(viewport(layout.pos.row = 2, clip = "on"))
    if ((1 - prop) > 0) {
      grid.roundrect(y = .5 / (1 - prop), height = 1 / (1 - prop), gp = gpar(lwd = lwd, fill = fill[2], col = NA))
      if (bx_txt != "" && !is.null(bx_grob)) {
        # Should not autoadjust the cex but keep the previous one
        prev_cex <- attr(bx_grob, "adjusted_cex")
        bx_grob <- prTpGetBoxSizedTextGrob(
          txt = bx_txt,
          txt_clr = txt_clr[2],
          txt_cex = prev_cex,
          force_cex = TRUE,
          y = 0.5 / (1 - prop)
        )
        if (!is.null(bx_grob)) {
          grid.draw(bx_grob)
        }
      }
    }
    popViewport(2)
    grid.roundrect(gp = gpar(lwd = lwd, fill = NA, col = line_col))
  }
  popViewport()
}

#' Gets the text size for the box
#'
#' @param txt The text
#' @param txt_clr The color of the text
#' @param txt_cex The font size
#' @param force_cex If font size should be forced
#' @param ... Other options
#'
#' @keywords internal
prTpGetBoxSizedTextGrob <- function(txt,
                                    txt_clr,
                                    txt_cex,
                                    force_cex = FALSE,
                                    ...) {
  bx_grob <- textGrob(txt,
    gp = gpar(col = txt_clr, cex = txt_cex),
    ...
  )
  attr(bx_grob, "adjusted_cex") <- txt_cex
  if (force_cex) {
    return(bx_grob)
  }

  bx_height <- convertY(grobHeight(bx_grob), "npc", valueOnly = TRUE)
  # The box height is by definition 1 npc
  # We want to avoid anything that is bigger that
  # 95 % and that is includingt he yjp (the 2 - 1.5 is a little too small)
  if (.95 < bx_height * 2) {
    new_cex <- txt_cex * .95 / (bx_height * 2)
    # Don't go below 1/4 of the original text size
    if (new_cex < txt_cex * .25) {
      return(NULL)
    } else {
      bx_grob <- textGrob(txt,
        gp = gpar(col = txt_clr, cex = new_cex),
        ...
      )
    }
    attr(bx_grob, "adjusted_cex") <- new_cex
  }

  return(bx_grob)
}


#' Plots the arrows
#'
#' Outputs all the arrows from a box row
#'
#' @param type The type of arrow used
#' @param box_row Which box do the arrows originate from
#' @param max_flow The largest transition flow
#' @param min_lwd The minimum line width
#' @param max_lwd The maximum line width
#' @param clr The color of the line
#' @param box_clr The color of the box
#' @param transition_arrow_props The proportions of the different transitions if
#'  available.
#' @param prop_start_sizes The proportions to the left
#' @param prop_end_sizes The proportions to the right
#' @param tot_spacing Total spacing between boxes
#' @param box_width The box width
#' @param add_width Add a certain width
#' @param color_bar_subspace If there is little or no difference exists
#'  at the low/high proportions of the spectrum then it
#'  can be of interest to focus the color change to the center
#'  leaving the tails constant
#' @return \code{void}
#'
#' @keywords internal
prTpPlotArrows <- function(type,
                           box_row,
                           transition_flow,
                           max_flow,
                           min_lwd,
                           max_lwd,
                           clr,
                           box_clr,
                           transition_arrow_props,
                           prop_start_sizes,
                           prop_end_sizes,
                           tot_spacing,
                           box_width,
                           abs_arrow_width,
                           color_bar_subspace,
                           add_width = NA) {
  no_boxes <- nrow(transition_flow)
  bx_left <- prTpGetBoxPositions(
    no = box_row, side = "left",
    transitions = transition_flow[box_row, ],
    prop_start_sizes = prop_start_sizes,
    prop_end_sizes = prop_end_sizes,
    tot_spacing = tot_spacing,
    box_width = box_width
  )
  # Plot the widest arrow last
  for (flow in order(transition_flow[box_row, ])) {
    if (transition_flow[box_row, flow] > 0) {
      bx_right <- prTpGetBoxPositions(
        no = flow, side = "right",
        transitions = transition_flow[, box_row],
        prop_start_sizes = prop_start_sizes,
        prop_end_sizes = prop_end_sizes,
        tot_spacing = tot_spacing,
        box_width = box_width
      )

      # Calculate line width
      lwd <- max_lwd * transition_flow[box_row, flow] / max_flow
      if (lwd < min_lwd) {
        message(
          "The minimum width reached and the arrow at box no. '", box_row, "' to no. '", flow, "'",
          " will not be shown. This is due to the fact that the lwd will generate a falsely strong arrow."
        )
        next
      }

      adjusted_lwd <- lwd
      if (is.na(add_width) == FALSE) {
        if ("unit" %in% class(add_width)) {
          adjusted_lwd <- convertUnit(unit(lwd, "npc") + add_width, unitTo = "npc", valueOnly = TRUE)
        } else if (add_width > 1) {
          adjusted_lwd <- lwd * add_width
        } else {
          # Quit if the width isn't bigger as it won't show
          return()
        }
      }
      a_l <- (box_width / 4)
      x_ctrl_points <- c(bx_left$right, .5, .5, bx_right$left)
      y_ctrl_points <- c(
        bx_left$y_exit[flow], bx_left$y_exit[flow],
        bx_right$y_entry[box_row], bx_right$y_entry[box_row]
      )
      current_arrow_clr <- clr[(flow + (box_row - 1) * no_boxes)]
      if (type == "grid") {
        if (abs_arrow_width) {
          a_width <- bx_right$y_entry_height / no_boxes
        } else {
          # Not really sure but points seem to be a reasonable
          # unit for the lwd as a basis for this part
          a_width <- getGridVal(unit(lwd, "pt"), "npc") +
            bx_right$y_entry_height / (no_boxes + 1)
        }

        # Add line width addition if it is a background line
        if (!is.na(add_width)) {
          if (is.unit(add_width)) {
            a_width <- a_width + convertY(add_width, unitTo = "npc", valueOnly = TRUE)
          } else {
            a_width <- a_width * add_width
          }
        }

        a_angle <- atan(a_width / 2 / a_l) * 180 / pi
        # Need to adjust the end of the arrow as it otherwise overwrites part of the box
        # if it is thick
        x_ctrl_points[4] <- x_ctrl_points[4] - .00075 * adjusted_lwd
        grid.bezier(
          x = x_ctrl_points,
          y = y_ctrl_points,
          gp = gpar(lwd = adjusted_lwd, fill = current_arrow_clr, col = current_arrow_clr),
          arrow = arrow(type = "closed", angle = a_angle, length = unit(a_l, "npc"))
        )
      } else {
        # The width can be wider using the special bezier arrows
        if (abs_arrow_width) {
          a_width <- bx_right$y_entry_height * 1.5 / no_boxes
        } else {
          a_width <- # getGridVal(lwd, "npc") +
            bx_right$y_entry_height * transition_flow[box_row, flow] / max_flow * 2.2
          # Set a maximum size in proportion to the line
          if (getGridVal(lwd, "npc", axisTo = "y") * 1.66 < a_width) {
            a_width <- getGridVal(lwd, "npc", axisTo = "y") * 1.66
          }
        }

        # Add line width addition if it is a background line
        if (!is.na(add_width)) {
          if (is.unit(add_width)) {
            a_width <- a_width + convertY(add_width, unitTo = "npc", valueOnly = TRUE)
          } else {
            a_width <- a_width * add_width
          }
        }


        if (a_width < adjusted_lwd) {
          sp_float_string <- sprintf("%%.%df", -floor(log10(adjusted_lwd - a_width)) + 1)
          warning(
            "The arrow width is smaller than the width of the line,",
            "thus not appearing as a regular arrow: ",
            sprintf(sp_float_string, a_width),
            " < ",
            sprintf(sp_float_string, adjusted_lwd)
          )

          # Looks really weird if this is allowed
          a_width <- adjusted_lwd
        }

        if (type == "simple") {
          bz <- bezierArrowSmpl(
            x = x_ctrl_points,
            y = y_ctrl_points,
            width = adjusted_lwd,
            arrow = list(length = a_l, base = a_width),
            clr = current_arrow_clr
          )
          grid.draw(bz)
        } else if (type == "gradient") {
          if (length(box_clr) > 1) {
            # Invert order as that is the fill order
            current_grdt_clr <- prTpGetColors(
              colors = box_clr,
              proportion = 1 - transition_arrow_props[box_row, flow],
              space = color_bar_subspace
            )
          } else {
            current_grdt_clr <- box_clr
          }
          bz <- bezierArrowGradient(
            x = x_ctrl_points,
            y = y_ctrl_points,
            width = adjusted_lwd,
            arrow = list(length = a_l, base = a_width),
            clr = current_arrow_clr,
            grdt_type = "triangle",
            grdt_prop = .8,
            grdt_clr_prop = 0.5,
            grdt_decrease_prop = .5,
            grdt_clr = current_grdt_clr
          )
          grid.draw(bz)
        } else {
          stop("The arrow type ", type, " is not yet implemented, sorry.")
        }
      }
    }
  }
}

#' Plot boxes and arrows
#'
#' Outputs all the boxes and arrow
#'
#' @param no_boxes Number of boxes to plot
#' @param txt The text
#' @param prop_start_sizes The proportion of the different boxes
#'  to the left
#' @param prop_end_sizes The proportion of the different boxes
#'  to the right
#' @param fill_start_clr The color of the boxes to the left
#' @param fill_end_clr The color of the boxes to the right
#' @param lwd The line width
#' @param line_col The color of the line
#' @param abs_arrow_width The absolute width of the arrow
#' @param transition_arrow_props The proportions of the different transitions if
#'  available.
#' @param plot_arrows If we are plotting shadow boxes then
#'  arrows should not be plotted and this should be set to \code{FALSE}
#' @param proportion It there is a proportion
#' @return \code{void}
#'
#' @inheritParams transitionPlot
#' @keywords internal
prTpPlotBoxes <- function(overlap_order,
                          transition_flow,
                          no_boxes,
                          box_width,
                          tot_spacing,
                          txt,
                          cex,
                          prop_start_sizes, prop_end_sizes,
                          box_prop,
                          lwd_prop_total = lwd_prop_total,
                          fill_start_clr, fill_end_clr,
                          txt_start_clr, txt_end_clr,
                          lwd = 2, line_col = "#000000",
                          min_lwd,
                          max_lwd,
                          overlap_add_width,
                          overlap_bg_clr,
                          type_of_arrow,
                          abs_arrow_width,
                          arrow_clr,
                          transition_arrow_props,
                          color_bar_subspace,
                          plot_arrows = TRUE, proportion = FALSE) {
  for (i in overlap_order) {
    if (prop_start_sizes[i] > 0) {
      bx_left <- prTpGetBoxPositions(
        no = i, side = "left",
        transitions = transition_flow[i, ],
        prop_start_sizes = prop_start_sizes,
        prop_end_sizes = prop_end_sizes,
        tot_spacing = tot_spacing,
        box_width = box_width
      )
      if (!missing(box_prop) & proportion) {
        fill_clr <- fill_start_clr[i, ]
        txt_clr <- txt_start_clr[i, ]
        prop <- box_prop[i, 1]
      } else {
        prop <- NA
        fill_clr <- fill_start_clr[i]
        txt_clr <- txt_start_clr[i]
      }

      if (plot_arrows) {
        # Plot arrows
        if (lwd_prop_total) {
          max_flow <- max(transition_flow)
        } else {
          max_flow <- sum(transition_flow[i, ])
        }

        # Do the background arrows
        prTpPlotArrows(
          type = ifelse(type_of_arrow == "grid", "grid", "simple"),
          box_row = i,
          transition_flow = transition_flow,
          max_flow = max_flow,
          min_lwd = min_lwd,
          max_lwd = max_lwd,
          clr = rep(overlap_bg_clr, no_boxes * ncol(transition_flow)),
          box_clr = overlap_bg_clr,
          prop_start_sizes = prop_start_sizes,
          prop_end_sizes = prop_end_sizes,
          tot_spacing = tot_spacing,
          box_width = box_width,
          abs_arrow_width = abs_arrow_width,
          add_width = overlap_add_width
        )

        # The actual arrows
        prTpPlotArrows(
          type = type_of_arrow,
          box_row = i,
          transition_flow = transition_flow,
          max_flow = max_flow,
          min_lwd = min_lwd,
          max_lwd = max_lwd,
          clr = arrow_clr,
          box_clr = fill_clr,
          transition_arrow_props = transition_arrow_props,
          prop_start_sizes = prop_start_sizes,
          prop_end_sizes = prop_end_sizes,
          tot_spacing = tot_spacing,
          box_width = box_width,
          abs_arrow_width = abs_arrow_width,
          color_bar_subspace = color_bar_subspace,
          add_width = NA
        )
      }


      prTpPlotBox(
        bx = bx_left,
        bx_txt = txt[i, 1],
        fill = fill_clr,
        txt_clr = txt_clr,
        cex = cex,
        line_col = line_col,
        lwd = lwd,
        prop = prop
      )
    }

    if (prop_end_sizes[i] > 0) {
      bx_right <- prTpGetBoxPositions(
        no = i, side = "right",
        transitions = transition_flow[, i],
        prop_start_sizes = prop_start_sizes,
        prop_end_sizes = prop_end_sizes,
        tot_spacing = tot_spacing,
        box_width = box_width
      )

      if (!missing(box_prop) & proportion) {
        fill_clr <- fill_end_clr[i, ]
        txt_clr <- txt_end_clr[i, ]
        prop <- box_prop[i, 2]
      } else {
        prop <- NA
        fill_clr <- fill_end_clr[i]
        txt_clr <- txt_end_clr[i]
      }

      prTpPlotBox(
        bx = bx_right,
        bx_txt = txt[i, 2],
        fill = fill_clr,
        txt_clr = txt_clr,
        cex = cex,
        line_col = line_col,
        lwd = lwd,
        prop = prop
      )
    }
  }
}

#' Gets the box position
#'
#' @param no The box number
#' @param side The right or left side
#' @param no_boxes The number of boxes
#' @param transitions The flows to or from
#' @param prop_start_sizes The size of the start boxes
#' @param prop_end_sizes The size of the end boxes
#' @return \code{list(top, left, bottom, right, width, height)}
#'
#' @inheritParams transitionPlot
#' @importFrom utils head tail
#' @keywords internal
prTpGetBoxPositions <- function(no, side,
                                transitions,
                                prop_start_sizes, prop_end_sizes,
                                tot_spacing,
                                box_width) {
  no_boxes <- max(length(prop_start_sizes), length(prop_end_sizes))
  empty_boxes <- ifelse(side == "left",
    sum(prop_start_sizes == 0),
    sum(prop_end_sizes == 0)
  )

  # Calculate basics
  space <- tot_spacing / (no_boxes - 1 - empty_boxes)

  # Basic box vars
  ret <- list(
    height = (1 - tot_spacing) * ifelse(side == "left",
      prop_start_sizes[no],
      prop_end_sizes[no]
    ),
    width = box_width
  )

  if (no == 1) {
    ret$top <- 1
  } else {
    ret$top <- 1 -
      ifelse(side == "left",
        sum(prop_start_sizes[1:(no - 1)]),
        sum(prop_end_sizes[1:(no - 1)])
      ) * (1 - tot_spacing) -
      space * (no - 1)
  }
  ret$bottom <- ret$top - ret$height
  ret$y <- mean(c(ret$top, ret$bottom))

  ret$y_exit <- rep(ret$y, times = no_boxes)
  ret$y_entry_height <- ret$height / 3
  ret$y_entry <- rep(NA, times = no_boxes)
  if (any(is.na(transitions)) || any(sum(transitions[-1]) == 0)) {
    # There are invalid transitions or it seems that
    # only one arrow exists
    if (any(is.na(transitions))) {
      ret$y_entry <- seq(
        to = ret$y - ret$height / 6,
        from = ret$y + ret$height / 6,
        length.out = no_boxes
      )
    } else {
      ret$y_entry <- rep(ret$y, times = no_boxes)
    }
  } else {
    # The entry point should be distributed according to arrow width
    # Unfortunately the arrow width also relates to the min_lwd and max_lwd
    # there is therefore some need for adjustments
    for (i in 1:no_boxes) {
      if (i == 1) {
        ret$y_entry[i] <- ret$y + ret$y_entry_height / 2
      } else if (sum(transitions[1:i]) == sum(transitions)) {
        ret$y_entry[i] <- ret$y - ret$y_entry_height / 2
      } else {
        # Do a proportion and remove half of the top/bottom
        # as these attach at the ends
        arrow_proportion <- sum(transitions[2:i]) /
          (sum(transitions) - (head(transitions, 1) + tail(transitions, 1)) / 2)

        # Narrow the space slightly
        narrower <- (no_boxes + 8) / (no_boxes + 10)

        ret$y_entry[i] <- ret$y + ret$height / 6 -
          ret$y_entry_height / no_boxes / 2 -
          ret$y_entry_height * narrower * arrow_proportion
      }
    }
    ret$y_entry <- seq(
      to = ret$y - ret$height / 6,
      from = ret$y + ret$height / 6,
      length.out = no_boxes
    )
  }

  # Now the x-axis
  if (side == "right") {
    ret$left <- 1 - box_width
    ret$right <- 1
  } else {
    ret$left <- 0
    ret$right <- box_width
  }

  txt_margin <- box_width / 10
  ret$txt_height <- ret$height - txt_margin * 2
  ret$txt_width <- box_width - txt_margin * 2

  ret$x <- mean(c(ret$left, ret$right))

  return(ret)
}

#' Gets a set of colors or just one color
#'
#' Used in order to illustrate the mix between two
#' proportions in the \code{\link{transitionPlot}}.
#'
#' @param colors A set of min. two colors that is used
#'  for \code{\link[grDevices:colorRamp]{colorRampPalette}}.
#' @param proportion A proportion or a set of proportions
#'  between 0 and 1. If you leave this out then the full color
#'  span will be returned.
#' @param space If there is little or no difference
#'  at the low/high proportions of the spectrum then it
#'  can be of interest to focus the color change to the center
#'  leaving the tails constant
#' @param The color resolution to use
#' @return \code{character} The function can return both single colors
#'  or multiple colors as character vector (see \code{\link[grDevices]{rgb}})
#'
#' @importFrom utils tail
#' @keywords internal
prTpGetColors <- function(colors, proportion, space = NULL, no = 101) {
  start <- c()
  end <- c()
  if (!is.null(space)) {
    if (any(space > 1 | space < 0)) {
      stop("Your color subspace that you define should be between 0 and 1")
    }

    if (length(space) > 2) {
      stop(
        "The color subspace has to be a length of either one or two",
        " you have provided ", paste(space, collapse = ", "),
        " of length", length(space)
      )
    }

    if (length(space) == 2) {
      start_no <- ceiling(space[1] * no)
      end_no <- ceiling(space[2] * no)
      start <- rep(colors[1], times = start_no)
      end <- rep(tail(colors, 1), times = end_no)
      no <- end_no - start_no
      if (no < 0) {
        no <- 0
      }
    } else {
      tails_no <- ceiling(space * no)
      start <- rep(colors[1], times = tails_no)
      end <- rep(tail(colors, 1), times = tails_no)
      no <- no - 2 * tails_no
      if (no < 0) {
        no <- 0
      }
    }
  }

  if (no > 0) {
    clrs <- c(
      start,
      colorRampPalette(colors, space = "Lab")(no),
      end
    )
  } else {
    clrs <- c(start, end)
  }

  if (missing(proportion)) {
    return(clrs)
  }

  if (any(proportion < 0 | proportion > 1)) {
    stop(
      "You color proportion of interest must lie between 0 and 1",
      " you have provided: ", proportion
    )
  }

  return(clrs[1 + min(100, floor(length(clrs) * proportion))])
}
