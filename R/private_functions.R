# This file contains all the helper functions that the outer exported
# functions utilize. I try to have a pr at the start of the name for all
# the private functions.
#
# Author: max
###############################################################################

#' Pushes viewport with margins
#'
#' A \code{\link[grid]{grid.layout}} object is used to
#' generate the margins. A second viewport selecting the
#' mid-row/col is used to create the effect of margins
#'
#' @param bottom The margin object, either in npc or a \code{\link[grid]{unit}} object,
#'  or a vector of 4 if you want the same margins allover.
#' @param left The margin object, either in npc or a \code{\link[grid]{unit}} object.
#' @param top The margin object, either in npc or a \code{\link[grid]{unit}} object
#' @param right The margin object, either in npc or a \code{\link[grid]{unit}} object
#' @param name The name of the last viewport
#' @return \code{2} - the number of pushed viewports
#'
#' @keywords internal
prPushMarginViewport <- function(bottom, left, top, right, name = "margin") {
  if (!is.unit(bottom)) {
    bottom <- unit(bottom, "npc")
  }

  if (missing(left) &&
    missing(top) &&
    missing(right)) {
    if (length(bottom) == 4) {
      left <- bottom[2]
      top <- bottom[3]
      right <- bottom[4]
      bottom <- bottom[1]
    } else {
      left <- bottom[1]
      top <- bottom[1]
      right <- bottom[1]
    }
  }

  if (!is.unit(top)) {
    top <- unit(top, "npc")
  }

  if (!is.unit(left)) {
    left <- unit(left, "npc")
  }

  if (!is.unit(right)) {
    right <- unit(right, "npc")
  }

  layout_name <- sprintf("margin_grid_%s", name)

  gl <- grid.layout(
    nrow = 3, ncol = 3,
    heights = unit.c(top, unit(1, "npc") - top - bottom, bottom),
    widths = unit.c(left, unit(1, "npc") - left - right, right)
  )

  pushViewport(viewport(layout = gl, name = layout_name))
  pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2, name = name))
  return(2)
}

#' Adds a title to the plot
#'
#' Adds the title and generates a new
#' main viewport below the title
#'
#' @param title The title as accepted by \code{\link[grid:grid.text]{textGrob}}
#' @param base_cex The base cex used for the plot
#' @param cex_mult The multiplier of the base - i.e. the increase of the
#'  text size for the title as compared to the general
#' @param fontface The type of fontface
#' @param space_below The space below, defaults to 1/5 of the title height
#' @return \code{NULL} The function does not return a value
#'
#' @keywords internal
prGridPlotTitle <- function(title,
                            base_cex,
                            cex_mult = 1.2,
                            fontface = "bold",
                            space_below = NULL) {
  titleGrob <- textGrob(title,
    just = "center",
    gp = gpar(
      fontface = fontface,
      cex = base_cex * cex_mult
    )
  )

  # The y/g/j letters are not included in the height
  gh <- unit(convertUnit(grobHeight(titleGrob), "mm", valueOnly = TRUE) * 1.5, "mm")
  if (is.null(space_below)) {
    space_below <- unit(convertUnit(gh, "mm", valueOnly = TRUE) / 3, "mm")
  } else if (!is.unit(space_below)) {
    space_below <- unit(space_below, "npc")
  }

  gl <- grid.layout(
    nrow = 3, ncol = 1,
    heights = unit.c(gh, space_below, unit(1, "npc") - space_below - gh)
  )

  pushViewport(viewport(layout = gl, name = "title_layout"))
  pushViewport(viewport(layout.pos.row = 1, name = "title"))
  grid.draw(titleGrob)
  upViewport()

  pushViewport(viewport(layout.pos.row = 3, name = "main"))
}

#' Just a simple acces to the gp$cex parameter
#'
#' @param x The text-grob of interest
#' @return \code{numeric} The cex value, 1 if no cex was present
#' @keywords internal
prGetTextGrobCex <- function(x) {
  cex <- 1
  if (!is.null(x$gp$cex)) {
    cex <- x$gp$cex
  }

  return(cex)
}

#' Collapses a vector for throwing errors
#'
#' The function collapses a vector into an output useful when throwing
#' errors, e.g. 1:3 becomes '1', '2', '3'
#'
#' @param x The vector
prPasteVec <- function(x) {
  sprintf("'%s'", paste(x, collapse = "', '"))
}

#' @title Coerce value to a unit
#' @description Ensure a value is returned as a grid `unit`. If `val` is
#'   already a `unit` it is returned unchanged; otherwise it's wrapped as
#'   `unit(..., "npc")`.
#' @param val A numeric or `unit` object.
#' @return A `unit` object.
#' @inheritParams grid::unit
#' @keywords internal
#' @noRd
prAsUnit <- function(val, units = "npc") {
  if (is.null(val)) {
    stop("Cannot convert NULL to a unit", call. = FALSE)
  }

  if (is.unit(val)) {
    return(val)
  }

  return(unit(val, units = units))
}

#' @title Convert height to millimetres
#' @description Convert a grid height `unit` (or numeric) to millimetres.
#' @param val A `unit` or numeric value representing a height.
#' @return Numeric value in millimetres.
#' @keywords internal
#' @noRd
prConvertHeightToMm <- function(val) {
  convertHeight(val, unitTo = "mm", valueOnly = TRUE)
}

#' @title Convert width to millimetres
#' @description Convert a grid width `unit` (or numeric) to millimetres.
#' @param val A `unit` or numeric value representing a width.
#' @return Numeric value in millimetres.
#' @keywords internal
#' @noRd
prConvertWidthToMm <- function(val) {
  convertWidth(val, unitTo = "mm", valueOnly = TRUE)
}

#' @title Convert millimetres to NPC
#' @description Convert a numeric in millimetres to a `unit` in `npc`.
#' @param mm_val Numeric millimetres value.
#' @param axis Character 'x' or 'y' to choose convertX/convertY.
#' @return A `unit` in `npc`.
#' @keywords internal
#' @noRd
prConvertMmToNpc <- function(mm_val, axis = c("x", "y")) {
  axis <- match.arg(axis)
  if (!is.numeric(mm_val) || length(mm_val) != 1 || is.na(mm_val)) {
    return(unit(NA_real_, "npc"))
  }
  conv <- if (axis == "x") convertX else convertY
  v <- tryCatch(conv(unit(mm_val, "mm"), "npc", valueOnly = TRUE), error = function(e) NA_real_)
  unit(v, "npc")
}

#' @title Extract numeric NPC position
#' @description Obtain a numeric NPC position from a `unit` or numeric-like
#'   input. Attempts `convertX/convertY` first and falls back to numeric
#'   coercion when conversion fails. Returns `NA_real_` on failure.
#' @param u A `unit` or numeric-like value.
#' @param axis Character, either "x" or "y" to choose `convertX` or
#'   `convertY` respectively.
#' @return Numeric in NPC units or `NA_real_`.
#' @keywords internal
#' @noRd
prGetNpcValue <- function(u, axis = c("x", "y")) {
  axis <- match.arg(axis)
  conv <- if (axis == "x") convertX else convertY
  v <- tryCatch(conv(u, "npc", valueOnly = TRUE), error = function(e) NA_real_)
  if (!is.na(v)) {
    return(v)
  }

  v2 <- suppressWarnings(as.numeric(u))
  if (!is.na(v2) && length(v2) == 1 && v2 >= -1 && v2 <= 2) {
    return(v2)
  }
  NA_real_
}

#' @title Obtain size in NPC units
#' @description Convert a width/height `unit` or numeric-like value to a
#'   numeric expressed in NPC. Uses `convertWidth/convertHeight` and falls
#'   back to numeric coercion if needed.
#' @param u A `unit` or numeric-like value representing a size.
#' @param axis Character, either "x" (width) or "y" (height).
#' @return Numeric size in NPC or `NA_real_`.
#' @keywords internal
#' @noRd
prGetNpcSize <- function(u, axis = c("x", "y")) {
  axis <- match.arg(axis)
  conv <- if (axis == "x") convertWidth else convertHeight
  v <- tryCatch(conv(u, "npc", valueOnly = TRUE), error = function(e) NA_real_)
  if (!is.na(v)) {
    return(v)
  }
  v2 <- suppressWarnings(as.numeric(u))
  if (!is.na(v2) && length(v2) == 1 && v2 >= -1 && v2 <= 2) {
    return(v2)
  }
  NA_real_
}
