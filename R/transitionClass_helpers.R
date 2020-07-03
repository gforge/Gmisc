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
                              cex) {
  for (i in 1:length(box_positions)) {
    if (box_positions[[i]]$height == 0) {
      next
    }

    args <- list(
      bx = box_positions[[i]],
      bx_txt = txt[i],
      cex = cex,
      line_col = "black",
      lwd = 1
    )
    if (!is.null(attr(proportions, "prop"))) {
      args[["prop"]] <- attr(proportions, "prop")[i]
      args[["fill"]] <- fill[i, ]
      args[["txt_clr"]] <- txt_clr[i, ]
    } else {
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
#' @param rez The arrow resolution
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
                           type = c("simple", "gradient"),
                           clr,
                           rez,
                           origin_boxes, target_boxes,
                           left_box_clrs, add_width,
                           max_flow, min_width, max_width,
                           abs_arrow_width = FALSE,
                           clr_bar_subspace) {
  if (length(dim(trnstn_set)) == 3) {
    transition_arrow_props <- trnstn_set[, , 1] / (trnstn_set[, , 1] + trnstn_set[, , 2])
    trnstn_set <- (trnstn_set[, , 1] + trnstn_set[, , 2])
  }
  for (org_row in 1:nrow(trnstn_set)) {
    origin <- origin_boxes[[org_row]]

    # Check what arrows that should be skipped
    no_arrows <- sapply(widths[[org_row]], function(x) is.null(x$lwd), USE.NAMES = FALSE)

    # Plot the widest arrow last
    for (target_row in order(trnstn_set[org_row, ])) {
      if (no_arrows[target_row]) {
        next
      }

      # Start with background arrow
      for (add_width in list(unit(2, "pt"), NA)) {
        no_trgt <- sapply(widths, function(x) is.null(x[[target_row]]$lwd), USE.NAMES = FALSE)

        target <- target_boxes[[target_row]]
        entry_length <- target$height / 3
        arrow_length <- target$width / 4

        adjusted_lwd <- widths[[org_row]][[target_row]]$adj_lwd
        x_ctrl_points <- c(
          origin$right,
          c(origin$right, target$left) %>%
            mean() %>%
            rep(times = 2),
          target$left
        )
        arrow_dodge <- seq(
          from = target$height / 6,
          to = -target$height / 6,
          length.out = sum(!no_trgt)
        )
        exit_pos <- org_row - sum(no_trgt[0:org_row])
        y_ctrl_points <- c(
          rep(origin$y, 2),
          rep(target$y + arrow_dodge[exit_pos], 2)
        )
        adjusted_lwd <- convertY(adjusted_lwd, unitTo = target$unit, valueOnly = TRUE)
        # The width can be wider using the special bezier arrows
        if (abs_arrow_width) {
          a_width <- entry_length * 1.5 / sum(!no_trgt)
        } else {
          a_width <-
            adjusted_lwd * 2
        }

        # Add line width addition if it is a background line
        if (!is.na(add_width)) {
          if (is.unit(add_width)) {
            a_width <- a_width + convertY(add_width, unitTo = "npc", valueOnly = TRUE)
            adjusted_lwd <- adjusted_lwd + convertY(add_width, unitTo = "npc", valueOnly = TRUE)
          } else {
            a_width <- a_width * add_width
            adjusted_lwd <- adjusted_lwd * add_width
          }
          arrow_clr <- "#FFFFFF"
        } else {
          arrow_clr <- clr[org_row, target_row]
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

        adjusted_lwd <- convertY(unit(adjusted_lwd, target$unit), unitTo = "mm")
        arrow_length <- convertY(unit(arrow_length, target$unit), unitTo = "mm")
        a_width <- convertY(unit(a_width, target$unit), unitTo = "mm")
        if (type == "simple" || !is.na(add_width)) {
          bezierArrowSmpl(
            x = x_ctrl_points,
            y = y_ctrl_points,
            width = adjusted_lwd,
            arrow = list(length = arrow_length, base = a_width),
            clr = arrow_clr,
            rez = rez
          ) %>%
            grid.draw()
        } else {
          if (NCOL(left_box_clrs) == 2) {
            current_grdt_clr <- prTpGetColors(
              colors = left_box_clrs[org_row, ],
              proportion = 1 - transition_arrow_props[org_row, target_row],
              space = clr_bar_subspace
            )
          } else {
            if (is.matrix(left_box_clrs)) {
              current_grdt_clr <- left_box_clrs[org_row, 1]
            } else {
              current_grdt_clr <- left_box_clrs[org_row]
            }
          }

          bezierArrowGradient(
            x = x_ctrl_points,
            y = y_ctrl_points,
            width = adjusted_lwd,
            arrow = list(length = arrow_length, base = a_width),
            clr = arrow_clr,
            rez = rez,
            grdt_type = "triangle",
            grdt_clr_prop = 0.5,
            grdt_prop = .8,
            grdt_decrease_prop = .5,
            grdt_clr = current_grdt_clr
          ) %>%
            grid.draw()
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
#' @inheritParams prTcMatchClr
#' @return \code{matrix} Returns a matrix of the same size as the
#'  transition matrix
#' @keywords internal
prTcValidateAndPrepClr <- function(value, no_cols, no_rows, is3D) {
  if (!is.list(value)) {
    stop("Invalid color value. Expecting a list with a vector/matrix matching the no rows in each column")
  }

  if (length(value) != no_cols) {
    stop("Invalid color length, got ", length(value), " expected ", no_cols)
  }

  for (i in 1:length(value)) {
    if (NROW(value[[i]]) != no_rows[i]) {
      if (NROW(value[[i]]) == 1) {
        if (is.matrix(value[[i]]) &&
          NCOL(value[[i]]) <= 2 &&
          is3D) {
          value[[i]] <-
            matrix(value[[i]], nrow = no_rows[i], ncol = 2, byrow = TRUE)
        } else if (length(value[[i]]) == 1) {
          value[[i]] <-
            rep(value[[i]], no_rows[i])
        } else {
          stop(
            "The number of colors don't match the transition object for column no. ", i,
            ": ", length(value[[i]]), "  != ", no_rows[i]
          )
        }
      } else {
        stop(
          "The number of colors don't match the transition object for column no. ", i,
          ": ", length(value[[i]]), "  != ", no_rows[i]
        )
      }
    }
  }

  return(value)
}

#' Finds the matching colors for the new addition based on the original set
#' of colors
#'
#' @param add The colors that we want to add
#' @param org The original colors
#' @param no_cols The no. of columns in the transition matrix
#' @param no_rows All the row counts in the transition plot
#' @param is3D Whether the transition matrix has 3 dimensions
#' @return A list with colors that is appropriate for the transition object
#' @keywords internal
prTcMatchClr <- function(add, org, no_cols, no_rows, is3D) {
  if (no_cols == 2) {
    if (missing(add) &&
      length(org) == 2) {
      return(org)
    }

    if (is.list(add)) {
      if (length(add) == 1) {
        add <- rep(add, 2)
      } else if (length(add) != 2) {
        stop(
          "Invalid color list - expecting a list with 2 columns",
          " got one with ", length(add), " columns"
        )
      }
    } else if (is.matrix(add)) {
      if (ncol(add) == 2) {
        if (is3D) {
          add <- rep(list(add), 2)
        } else {
          add <- list(
            add[, 1],
            add[, 2]
          )
        }
      } else if (ncol(add) == 4 && is3D) {
        add <- list(
          add[, 1:2],
          add[, 3:4]
        )
      } else if (ncol(add) == 1) {
        if (length(add) == 2) {
          if (is3D) {
            add <-
              list(
                cbind(
                  rep(add[1, ], no_rows[1]),
                  rep(add[2, ], no_rows[1])
                ),
                cbind(
                  rep(add[1, ], no_rows[2]),
                  rep(add[2, ], no_rows[2])
                )
              )
          } else {
            add <-
              list(
                rep(add[1, ], no_rows[1]),
                rep(add[2, ], no_rows[2])
              )
          }
        } else if (length(add) == 1) {
          if (is3D) {
            add <-
              list(
                cbind(matrix(add[1, ],
                  nrow = no_rows[1],
                  ncol = 2
                )),
                cbind(matrix(add[1, ],
                  nrow = no_rows[1],
                  ncol = 2
                ))
              )
          } else {
            add <-
              list(
                rep(add[1, ], no_rows[1]),
                rep(add[1, ], no_rows[2])
              )
          }
        } else {
          stop("Invalid color argument")
        }
      }
    } else if (is.vector(add)) {
      if (length(add) == 2 &&
        is3D) {
        add <-
          list(
            cbind(
              rep(add[1], no_rows[1]),
              rep(add[2], no_rows[1])
            ),
            cbind(
              rep(add[1], no_rows[2]),
              rep(add[2], no_rows[2])
            )
          )
      } else if (length(add) == 1) {
        if (is3D) {
          add <-
            list(
              matrix(add,
                nrow = no_rows[1],
                ncol = 2
              ),
              matrix(add,
                nrow = no_rows[2],
                ncol = 2
              )
            )
        } else {
          add <-
            list(
              rep(add, no_rows[1]),
              rep(add, no_rows[2])
            )
        }
      } else {
        add <- list(add)
      }
    } else {
      stop("Invalid color argument")
    }

    # If this is the initial value then simply use the default colors
    return(add)
  }

  # We need to handle the situation where a color is
  # added and the colors are already of the same dimension
  # as the transition matrix. This means that either we
  # switch entire matrix if the new colors have the correct
  # dimenension but otherwise we stick with the old colors
  if (all(length(org) == no_cols)) {
    if (missing(add)) {
      return(org)
    }
    # If the add is equal in dimentions to the target
    # dimentions then the reasonable way to go is to substitute
    # all colors
    if (is.matrix(add)) {
      if (ncol(add) != no_cols ||
        any(nrow(add) != unique(no_rows)) ||
        ((length(dim(add)) == 3) == is3D)) {
        stop("Can't add the color matrix as suggested since the dimensions don't match")
      }

      clrs <- list()
      for (i in 1:ncol(add)) {
        if (is3D) {
          clrs <- c(clrs, list(add[, i, ]))
        } else {
          clrs <- c(clrs, list(add[, i]))
        }
      }
      return(clrs)
    } else if (is.list(add) &&
      length(add) != length(no_cols)) {
      stop("The list is not of the same length as the number of columns")
    }

    return(org)
  }

  if (missing(add)) {
    if (is3D) {
      # Doesn't work with magrittr as expected
      tmp <- tail(org, 1)[[1]]
      add <- cbind(
        rep(tmp[, 1], length.out = tail(no_rows, 1)),
        rep(tmp[, 2], length.out = tail(no_rows, 1))
      )
      rm(tmp)
    } else {
      add <- tail(org, 1)[[1]] %>%
        rep(length.out = tail(no_rows, 1))
    }
    add <- list(add)
  } else {
    if (is.list(add)) {
      if (length(add) != 1) {
        stop(
          "Invalid color list - expecting a list with 1 column",
          " got one with ", length(add), " columns"
        )
      }
    } else if (is.matrix(add)) {
      if (ncol(add) == 2 &&
        is3D) {
        add <- list(add)
      } else if (ncol(add) == 1 &&
        !is3D) {
        add <-
          list(add[, 1])
      } else if (length(add) == 1) {
        if (is3D) {
          add <-
            list(cbind(matrix(add[1, ],
              nrow = tail(no_rows, 1),
              ncol = 2
            )))
        } else {
          add <-
            list(rep(add[1, ], tail(no_rows, 1)))
        }
      } else {
        stop("Invalid matrix color argument")
      }
    } else if (is.vector(add)) {
      if (length(add) == 2 &&
        is3D) {
        add <-
          list(cbind(
            rep(add[1], tail(no_rows, 1)),
            rep(add[2], tail(no_rows, 1))
          ))
      } else if (length(add) == 1) {
        if (is3D) {
          add <-
            list(matrix(add,
              nrow = tail(no_rows, 1),
              ncol = 2
            ))
        } else {
          add <-
            list(rep(add, tail(no_rows, 1)))
        }
      } else {
        add <- list(add)
      }
    } else {
      stop("Invalid color argument")
    }
  }
  return(c(org, add))
}