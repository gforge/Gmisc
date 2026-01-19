#' Label connector grobs
#'
#' Place text labels at the midpoint of connector grobs returned by
#' `connectGrob()` (useful for multi-target connectors where `label=` isn't
#' available). By default the function draws the labels; set `draw = FALSE` to
#' only return the `textGrob` objects for programmatic use.
#'
#' @param con A list of connector grobs produced by `connectGrob()` (typically
#'   a `connect_boxes_list`).
#' @param labels A character vector of labels (recycled if necessary).
#' @param x_offset,y_offset `grid::unit` offsets added to the label position.
#' @param gp A `grid::gpar` object controlling text appearance.
#' @param bg_gp A `grid::gpar` object controlling label background appearance.
#' @param pad A `grid::unit` padding to add around text inside the background box.
#' @return A `Gmisc_connector_label` S3 object (invisible). Use `print()` to draw the labels.
#' @examples
#' # Attach labels to a previously-created connector and draw them
#' a <- boxDiamondGrob("D")
#' b <- boxEllipseGrob("L")
#' c <- boxServerGrob("S")
#' boxes <- list(decision = a, outcomes = list(b, c)) |>
#'   spreadHorizontal(
#'     .from = grid::unit(.1, "npc"),
#'     .to = grid::unit(.9, "npc"),
#'     .subelement = "outcomes"
#'   ) |>
#'   spreadVertical()
#'
#' connectGrob(boxes$decision, boxes$outcomes, type = "N") |>
#'   labelConnector(c("Local", "Server"))
#' @export
labelConnector <- function(
  con,
  labels,
  x_offset = unit(0, "mm"),
  y_offset = unit(0, "mm"),
  gp = gpar(cex = 0.9),
  bg_gp = gpar(fill = "white", col = NA),
  pad = unit(2, "mm")
) {
  if (!is.list(con)) stop("'con' must be a list of connector grobs")

  n <- length(con)
  if (n == 0) {
    out <- list(
      labels = character(0),
      texts = list(),
      bgs = list(),
      x = numeric(0),
      y = numeric(0),
      gp = gp,
      bg_gp = bg_gp,
      pad = pad,
      offsets = list(x_offset = x_offset, y_offset = y_offset)
    )
    class(out) <- "Gmisc_connector_label"
    return(invisible(out))
  }

  labels <- rep_len(as.character(labels), n)

  texts <- vector("list", n)
  bgs <- vector("list", n)
  xs <- numeric(n)
  ys <- numeric(n)

  for (i in seq_len(n)) {
    g <- con[[i]]
    if (is.null(attr(g, "line"))) stop("connector grob missing 'line' attribute")
    ln <- attr(g, "line")

    # convert coords to npc numeric values (works for mixed unit objects)
    xvals <- vapply(ln$x, \(u) prGetNpcValue(u, "x"), numeric(1))
    yvals <- vapply(ln$y, \(u) prGetNpcValue(u, "y"), numeric(1))

    # take midpoint (mean of numeric ranges) as label anchor
    x_mid <- mean(range(xvals, na.rm = TRUE))
    y_mid <- mean(range(yvals, na.rm = TRUE))

    x_unit <- unit(x_mid, "npc") + x_offset
    y_unit <- unit(y_mid, "npc") + y_offset

    tg <- textGrob(labels[i], x = x_unit, y = y_unit, gp = gp)

    # background rounded rect behind text
    w <- grobWidth(tg) + pad
    h <- grobHeight(tg) + pad
    bg <- roundrectGrob(x = x_unit, y = y_unit, width = w, height = h, r = unit(1, "mm"), gp = bg_gp)

    texts[[i]] <- tg
    bgs[[i]] <- bg
    xs[i] <- x_mid
    ys[i] <- y_mid
  }

  structure(
    list(
      labels = labels,
      texts = texts,
      bgs = bgs,
      x = xs,
      y = ys,
      gp = gp,
      bg_gp = bg_gp,
      pad = pad,
      offsets = list(x_offset = x_offset, y_offset = y_offset)
    ),
    class = c("Gmisc_connector_label", class(list()))
  )
}


#' Print a connector label object
#'
#' @param x A `Gmisc_connector_label` object
#' @param ... ignored
#' @export
print.Gmisc_connector_label <- function(x, ...) {
  # draw each background then text on top
  for (i in seq_along(x$texts)) {
    grid.draw(x$bgs[[i]])
    grid.draw(x$texts[[i]])
  }
  invisible(x)
}


#' Attach labels to a connector object
#'
#' Store labels and presentation options on a connector object so labels are
#' automatically drawn when the connector is printed.
#'
#' @param con A connector (a `connect_boxes` grob or `connect_boxes_list`).
#' @param labels Character vector of labels (recycled to match connector length).
#' @param gp A `gpar` for label text.
#' @param bg_gp A `gpar` for label background.
#' @param x_offset Unit offsets for label placement.
#' @param y_offset Unit offsets for label placement.
#' @return The connector object with attributes set (invisibly).
#' @export
setConnectorLabels <- function(
  con,
  labels,
  gp = gpar(cex = 0.9),
  bg_gp = gpar(fill = "white", col = NA),
  x_offset = unit(0, "mm"),
  y_offset = unit(0, "mm")
) {
  if (!is.list(con)) stop("'con' must be a connector list or single connector")
  n <- length(con)
  attr(con, "connector_labels") <- rep_len(as.character(labels), n)
  attr(con, "connector_label_gp") <- gp
  attr(con, "connector_label_bg_gp") <- bg_gp
  attr(con, "connector_label_offset") <- list(x_offset = x_offset, y_offset = y_offset)
  structure(
    con,
    class = "Gmisc_extended_connector_labels"
  )
}


#' @export
print.Gmisc_extended_connector_labels <- function(x, ...) {
  # Draw the underlying connector(s)
  if (is.list(x)) {
    for (g in x) grid.draw(g, ...)
  } else {
    grid.draw(x, ...)
  }

  # If labels are present, render them using the existing labelConnector helper
  labels <- attr(x, "connector_labels")
  if (!is.null(labels)) {
    gp <- attr(x, "connector_label_gp")
    if (is.null(gp)) gp <- gpar(cex = 0.9)
    bg_gp <- attr(x, "connector_label_bg_gp")
    if (is.null(bg_gp)) bg_gp <- gpar(fill = "white", col = NA)
    off <- attr(x, "connector_label_offset")
    if (is.null(off)) off <- list(x_offset = unit(0, "mm"), y_offset = unit(0, "mm"))

    lbl_obj <- labelConnector(
      x,
      labels = labels,
      x_offset = off$x_offset,
      y_offset = off$y_offset,
      gp = gp,
      bg_gp = bg_gp
    )
    print(lbl_obj)
  }

  invisible(x)
}
