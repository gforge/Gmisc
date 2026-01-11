#' Additional box shape helpers for `boxGrob`
#'
#' These helper functions provide alternative box drawing functions that can be
#' passed to `boxGrob(..., box_fn = ...)` or used via convenience wrappers
#' (`boxDiamondGrob`, `boxEllipseGrob`, `boxRackGrob`). They are intentionally
#' lightweight and implemented using base `grid` primitives.
#'
#' @inheritParams boxGrob
#' @details Shape functions are called with `x` and `y` in [0,1] inside the box viewport and with `box_gp` for styling; convenience wrappers match the `boxGrob` signature.
#' @return A `grid` grob that draws the requested shape inside the box viewport.
#' @rdname boxShapes
#' @name boxShapes
#' @family flowchart components
#' @seealso \code{\link{boxGrob}} for usage and the `box_fn` argument
#' @examples
#' # These functions are designed to be passed to `boxGrob(..., box_fn = ...)`.
#' grid::grid.newpage()
#' d <- boxGrob("Decision", box_fn = diamond_rounded_box_fn)
#' e <- boxGrob("Start", box_fn = ellipse_box_fn)
#' r <- boxGrob("Server", box_fn = rack_box_fn)
#' spreadHorizontal(list(d, e, r))
NULL


#' Diamond (rounded or sharp)
#'
#' Two diamond variants are provided: a rounded diamond (default) implemented
#' with `xsplineGrob` for slightly softened corners, and a sharp diamond
#' implemented with `polygonGrob` for strict straight edges.
#'
#' @rdname boxShapes
#' @export
diamond_rounded_box_fn <- function(x = .5, y = .5, gp = gpar()) {
  # Use slight inset from the viewport edge and a larger shape parameter
  # to avoid XSpline overshoot that can create small spikes when connectors
  # attach to the top/bottom of the box.
  inset <- 0.02
  grob <- xsplineGrob(
    x = c(.5, 1 - inset, .5, 0 + inset),
    y = c(1 - inset, .5, 0 + inset, .5),
    shape = rep(0.35, 4),
    open = FALSE,
    gp = gp
  )
  # Suggest additional padding for diamond shapes since text can sit close
  # to the acute corners; boxGrob will pick up this attribute and add it
  # to the text padding when sizing the box.
  attr(grob, "box_fn_padding") <- unit(3, "mm")
  grob
}

#' Sharp diamond (straight edges)
#'
#' @rdname boxShapes
#' @export
diamond_sharp_box_fn <- function(x = .5, y = .5, gp = gpar()) {
  inset <- 0.02
  grob <- polygonGrob(x = c(.5, 1 - inset, .5, 0 + inset), y = c(1 - inset, .5, 0 + inset, .5), gp = gp)
  # Recommend a little extra padding for the sharp diamond variant as well
  attr(grob, "box_fn_padding") <- unit(3, "mm")
  grob
}

#' Convenience: create a diamond box with same signature as `boxGrob`
#'
#' @param rounded Logical; use rounded diamond (TRUE) or sharp diamond (FALSE).
#' @rdname boxShapes
#' @export
boxDiamondGrob <- function(label, y = unit(.5, "npc"), x = unit(.5, "npc"),
                           width, height, just = "center", bjust = "center",
                           txt_gp = getOption("boxGrobTxt", default = gpar(color = "black", cex = 1)),
                           box_gp = getOption("boxGrob", default = gpar(fill = "white")),
                           rounded = TRUE,
                           name = NULL) {
  box_fn <- if (isTRUE(rounded)) diamond_rounded_box_fn else diamond_sharp_box_fn
  boxGrob(
    label = label, y = y, x = x, width = width, height = height,
    just = just, bjust = bjust, txt_gp = txt_gp, box_gp = box_gp,
    box_fn = box_fn, name = name
  )
}


#' Ellipse box
#'
#' Approximates an ellipse using a polygon with many points; it respects the
#' box viewport so it scales correctly.
#'
#' @rdname boxShapes
#' @export
ellipse_box_fn <- function(x = .5, y = .5, gp = gpar()) {
  t <- seq(0, 2 * pi, length.out = 80)
  xs <- .5 + .48 * cos(t)
  ys <- .5 + .36 * sin(t)
  polygonGrob(x = xs, y = ys, gp = gp)
}


#' @rdname boxShapes
#' @export
boxEllipseGrob <- function(label, y = unit(.5, "npc"), x = unit(.5, "npc"),
                           width, height, just = "center", bjust = "center",
                           txt_gp = getOption("boxGrobTxt", default = gpar(color = "black", cex = 1)),
                           box_gp = getOption("boxGrob", default = gpar(fill = "white")),
                           name = NULL) {
  boxGrob(
    label = label, y = y, x = x, width = width, height = height,
    just = just, bjust = bjust, txt_gp = txt_gp, box_gp = box_gp,
    box_fn = ellipse_box_fn, name = name
  )
}


#' Rack / server styled box
#'
#' A rectangular box with a slightly darker top band to mimic a server / disk rack
#' look. Implemented by composing a rounded rectangle and an overlaying top
#' strip. The strip is drawn without border to avoid double outlines.
#'
#' @rdname boxShapes
#' @export
rack_box_fn <- function(x = .5, y = .5, gp = gpar()) {
  # base rounded rectangle
  base <- roundrectGrob(x = .5, y = .5, r = unit(3, "mm"), gp = gp)

  # compute a top strip fill (lightened version of box fill); set col = NA
  strip_fill <- if (is.null(gp$fill)) "grey90" else lighten_color(gp$fill, 0.9)
  top_strip <- rectGrob(x = .5, y = unit(0.78, "npc"), width = unit(0.95, "npc"), height = unit(0.22, "npc"), gp = gpar(fill = strip_fill, col = NA))

  gTree(children = gList(base, top_strip))
}

#' @rdname boxShapes
#' @export
boxRackGrob <- function(label, y = unit(.5, "npc"), x = unit(.5, "npc"),
                        width, height, just = "center", bjust = "center",
                        txt_gp = getOption("boxGrobTxt", default = gpar(color = "black", cex = 1)),
                        box_gp = getOption("boxGrob", default = gpar(fill = "white")),
                        name = NULL) {
  boxGrob(
    label = label, y = y, x = x, width = width, height = height,
    just = just, bjust = bjust, txt_gp = txt_gp, box_gp = box_gp,
    box_fn = rack_box_fn, name = name
  )
}


#' Database / cylinder box
#'
#' Draws a cylinder-like database symbol (ellipse top, rectangular body, base ellipse).
#'
#' @rdname boxShapes
#' @export
database_box_fn <- function(x = .5, y = .5, gp = gpar()) {
  t <- seq(0, 2 * pi, length.out = 64)
  top_h <- 0.83
  top_rvx <- 0.42
  top_rvy <- 0.09
  xs_top <- .5 + top_rvx * cos(t)
  ys_top <- top_h + top_rvy * sin(t)

  bottom_h <- 0.17
  bottom_rvy <- 0.09
  xs_bot <- .5 + top_rvx * cos(t)
  ys_bot <- bottom_h + bottom_rvy * sin(t)

  body <- rectGrob(x = .5, y = .5, width = unit(0.84, "npc"), height = unit(0.66, "npc"), gp = gp)
  top <- polygonGrob(x = xs_top, y = ys_top, gp = gp)
  bot_fill <- polygonGrob(x = xs_bot, y = ys_bot, gp = gpar(fill = gp$fill, col = NA))
  # Outline bottom to give the cylinder edge (use gp$col if available, otherwise black)
  col_edge <- if (!is.null(gp$col)) gp$col else "black"
  bot_edge <- linesGrob(x = xs_bot, y = ys_bot, gp = gpar(col = col_edge))

  gTree(children = gList(body, bot_fill, top, bot_edge))
}

#' @rdname boxShapes
#' @export
boxDatabaseGrob <- function(label, y = unit(.5, "npc"), x = unit(.5, "npc"),
                            width, height, just = "center", bjust = "center",
                            txt_gp = getOption("boxGrobTxt", default = gpar(color = "black", cex = 1)),
                            box_gp = getOption("boxGrob", default = gpar(fill = "white")),
                            name = NULL) {
  boxGrob(
    label = label, y = y, x = x, width = width, height = height,
    just = just, bjust = bjust, txt_gp = txt_gp, box_gp = box_gp,
    box_fn = database_box_fn, name = name
  )
}


#' Document (wavy bottom) box
#'
#' A document icon with a wavy bottom edge, useful to indicate printed output.
#'
#' @rdname boxShapes
#' @export
document_box_fn <- function(x = .5, y = .5, gp = gpar()) {
  n <- 80
  xs <- seq(0.08, 0.92, length.out = n)
  # top edge and sides
  top_y <- 0.88
  bottom_y <- 0.12
  amp <- 0.035
  wave <- bottom_y + amp * sin(seq(0, 2 * pi, length.out = n))

  xpoly <- c(xs, rev(xs))
  ypoly <- c(rep(top_y, n), rev(wave))
  polygonGrob(x = xpoly, y = ypoly, gp = gp)
}

#' @rdname boxShapes
#' @export
boxDocumentGrob <- function(label, y = unit(.5, "npc"), x = unit(.5, "npc"),
                            width, height, just = "center", bjust = "center",
                            txt_gp = getOption("boxGrobTxt", default = gpar(color = "black", cex = 1)),
                            box_gp = getOption("boxGrob", default = gpar(fill = "white")),
                            name = NULL) {
  boxGrob(
    label = label, y = y, x = x, width = width, height = height,
    just = just, bjust = bjust, txt_gp = txt_gp, box_gp = box_gp,
    box_fn = document_box_fn, name = name
  )
}


#' Multiple documents (stack)
#'
#' Draws a stack of document shapes by repeating the same shape with offsets.
#'
#' @rdname boxShapes
#' @export
documents_box_fn <- function(x = .5, y = .5, gp = gpar()) {
  n <- 80
  xs <- seq(0.08, 0.92, length.out = n)
  top_y_base <- 0.88
  bottom_y_base <- 0.12
  amp <- 0.035

  # Stack pages with a small diagonal offset so the stack is visible.
  # Back pages are drawn first (larger offset), front page last (no offset).
  n_pages <- 3
  shifts_x <- seq(0.03, 0, length.out = n_pages)
  shifts_y <- seq(-0.02, 0, length.out = n_pages)

  docs <- lapply(seq_len(n_pages), function(i) {
    sx <- shifts_x[i]
    sy <- shifts_y[i]
    top_y <- top_y_base + sy
    bottom_y <- bottom_y_base + sy
    wave <- bottom_y + amp * sin(seq(0, 2 * pi, length.out = n))
    xpoly <- c(xs + sx, rev(xs + sx))
    ypoly <- c(rep(top_y, n), rev(wave))
    polygonGrob(x = xpoly, y = ypoly, gp = gp)
  })

  gTree(children = do.call(gList, docs))
}

#' @rdname boxShapes
#' @export
boxDocumentsGrob <- function(label, y = unit(.5, "npc"), x = unit(.5, "npc"),
                             width, height, just = "center", bjust = "center",
                             txt_gp = getOption("boxGrobTxt", default = gpar(color = "black", cex = 1)),
                             box_gp = getOption("boxGrob", default = gpar(fill = "white")),
                             name = NULL) {
  boxGrob(
    label = label, y = y, x = x, width = width, height = height,
    just = just, bjust = bjust, txt_gp = txt_gp, box_gp = box_gp,
    box_fn = documents_box_fn, name = name
  )
}


#' Tape / curved storage box
#'
#' Left/right curved edges to mimic tape-like icons.
#'
#' @rdname boxShapes
#' @export
tape_box_fn <- function(x = .5, y = .5, gp = gpar()) {
  # Draw a tape-like rectangle with larger curved end caps (Option A).
  # Compose a central rectangle and two semi-elliptical caps so the ends are
  # more prominent and visually balanced with the body.
  body <- rectGrob(x = .5, y = .5, width = unit(0.74, "npc"), height = unit(0.64, "npc"), gp = gp)

  t <- seq(-pi / 2, pi / 2, length.out = 120)

  # Make caps larger horizontally and slightly taller
  cap_x_radius <- 0.12
  cap_y_radius <- 0.36

  # left cap (semi-ellipse) centered near left edge
  left_center <- 0.08
  left_x <- left_center + cap_x_radius * cos(t)
  left_y <- 0.5 + cap_y_radius * sin(t)
  left_cap <- polygonGrob(x = c(left_x, left_x[1]), y = c(left_y, left_y[1]), gp = gp)

  # right cap (mirror) centered near right edge
  right_center <- 0.92
  right_x <- right_center - cap_x_radius * cos(t)
  right_y <- 0.5 + cap_y_radius * sin(t)
  right_cap <- polygonGrob(x = c(right_x, right_x[1]), y = c(right_y, right_y[1]), gp = gp)

  gTree(children = gList(body, left_cap, right_cap))
}

#' @rdname boxShapes
#' @export
boxTapeGrob <- function(label, y = unit(.5, "npc"), x = unit(.5, "npc"),
                        width, height, just = "center", bjust = "center",
                        txt_gp = getOption("boxGrobTxt", default = gpar(color = "black", cex = 1)),
                        box_gp = getOption("boxGrob", default = gpar(fill = "white")),
                        name = NULL) {
  boxGrob(
    label = label, y = y, x = x, width = width, height = height,
    just = just, bjust = bjust, txt_gp = txt_gp, box_gp = box_gp,
    box_fn = tape_box_fn, name = name
  )
}


#' Server (front-panel vents)
#'
#' A server-styled box with front-panel vent lines so it is visually distinct
#' from a rack (top strip) representation.
#'
#' @param x Horizontal position inside the box viewport (0-1).
#' @param y Vertical position inside the box viewport (0-1).
#' @param gp A `gpar` object with styling information (fill/col/etc).
#' @rdname boxShapes
#' @export
server_box_fn <- function(x = .5, y = .5, gp = gpar()) {
  # Base rounded rectangle
  base <- roundrectGrob(x = .5, y = .5, r = unit(3, "mm"), gp = gp)

  # vents: three small horizontal strips centered vertically on the front
  vent_fill <- (if (!is.null(gp$col)) gp$col else "#333333")
  vent_gp <- gpar(fill = "#FFFFFF", col = NA)

  vent_ys <- c(0.58, 0.5, 0.42)
  vents <- lapply(vent_ys, function(vy) {
    rectGrob(x = .5, y = vy, width = unit(0.6, "npc"), height = unit(0.05, "npc"), gp = gpar(fill = lighten_color(gp$fill, 0.92), col = NA))
  })

  gTree(children = do.call(gList, c(list(base), vents)))
}

#' Convenience: create a server-styled box
#'
#' @rdname boxShapes
#' @importFrom grDevices rgb
#' @export
boxServerGrob <- function(label, y = unit(.5, "npc"), x = unit(.5, "npc"),
                          width, height, just = "center", bjust = "center",
                          txt_gp = getOption("boxGrobTxt", default = gpar(color = "black", cex = 1)),
                          box_gp = getOption("boxGrob", default = gpar(fill = "white")),
                          name = NULL) {
  boxGrob(
    label = label, y = y, x = x, width = width, height = height,
    just = just, bjust = bjust, txt_gp = txt_gp, box_gp = box_gp,
    box_fn = server_box_fn, name = name
  )
}


# Small helper to lighten a color (returns the hex for grid::gpar)
lighten_color <- function(col, factor = 0.9) {
  # factor <1 -> darker, >1 -> lighter (we use >1 to lighten)
  if (is.null(col)) {
    return(col)
  }
  if (is.na(col)) {
    return(col)
  }
  tryCatch(
    {
      rgb_vals <- grDevices::col2rgb(col) / 255
      rgb((1 - factor) * rgb_vals[1, ] + factor, (1 - factor) * rgb_vals[2, ] + factor, (1 - factor) * rgb_vals[3, ] + factor)
    },
    error = function(e) col
  )
}
