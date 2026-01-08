#' Connect boxes with an arrow
#'
#' Creates one or more grobs that link boxes together. By default a single grob is
#' returned when connecting one start box to one end box. If \code{end} is a list
#' of boxes, a list of grobs is returned (one per end box).
#'
#' The connector geometry is stored in \code{attr(x, "line")} where \code{x} is the
#' returned grob (or each grob in the returned list). This can be used to draw
#' custom connectors by reading \code{attr(x, "line")$x} and \code{attr(x, "line")$y}
#' and creating your own \code{\link[grid:grid.lines]{linesGrob}}.
#'
#' For \code{type = "N"} and a multi-end connection, the split/bend height is shared
#' across all branches so that the horizontal segment occurs at the same y-position.
#'
#' @param start The start box.
#' @param end The end box, or a \code{list} of boxes for a one-to-many connection.
#' @param type How the boxes are connected:
#' \describe{
#'   \item{\code{"vertical"}}{Straight vertical connector between boxes.}
#'   \item{\code{"horizontal"}}{Straight horizontal connector between boxes.}
#'   \item{\code{"L"}}{Vertical then horizontal connector (right/left chosen automatically).}
#'   \item{\code{"-"}}{Straight horizontal connector at the end box y-position.}
#'   \item{\code{"Z"}}{Horizontal connector with two 90-degree turns (Z-shaped).}
#'   \item{\code{"N"}}{Vertical connector with one horizontal segment (N-shaped).}
#' }
#' @param subelmnt For split boxes (e.g. \code{boxPropGrob}), specifies whether to
#'   use the left or right sub-element x-coordinate when anchoring the connector.
#' @param lty_gp The \code{\link[grid]{gpar}} for the line. Set the global option
#'   \code{connectGrob} to customize all connectors.
#' @param arrow_obj The arrow specification according to \code{\link[grid]{arrow}}.
#'   Set the global option \code{connectGrobArrow} to customize all connectors.
#' @param split_pad For multi-end connections with \code{type = "N"}, the padding
#'   around the shared split/bend height. Numeric values are interpreted as millimeters.
#'
#' @return
#' If \code{end} is a single box, a \code{\link[grid]{grob}} with class
#' \code{"connect_boxes"}.
#'
#' If \code{end} is a list of boxes, a \code{list} of grobs (one per end box) with
#' class \code{"connect_boxes_list"}.
#'
#' @export
#' @importFrom checkmate assert_class
#' @family flowchart components
#' @rdname connect
#' @example inst/examples/connectGrob_example.R
connectGrob <- function(
    start,
    end,
    type = c("vertical", "horizontal", "L", "-", "Z", "N"),
    subelmnt = c("right", "left"),
    lty_gp = getOption("connectGrob", default = gpar(fill = "black")),
    arrow_obj = getOption("connectGrobArrow", default = arrow(ends = "last", type = "closed")),
    split_pad = unit(2, "mm")
) {
  type <- match.arg(type)
  
  if (prIsBoxList(end)) {
    return(prConnectMany(start, end, type, subelmnt, lty_gp, arrow_obj, split_pad = split_pad))
  }
  
  prConnect1(start, end, type, subelmnt, lty_gp, arrow_obj)
}


#' The print/plot calls the \code{\link[grid]{grid.draw}} function on the object
#' @param x The grob to print/plot
#' @param ... Passed to \code{\link[grid]{grid.draw}}
#' @rdname connect
#' @export
print.connect_boxes <- function(x, ...) {
  grid.draw(x, ...)
}

#' @rdname connect
#' @export
plot.connect_boxes <- print.connect_boxes


#' @rdname connect
#' @export
print.connect_boxes_list <- function(x, ...) {
  for (g in x) grid.draw(g, ...)
  invisible(x)
}

#' @rdname connect
#' @export
plot.connect_boxes_list <- print.connect_boxes_list

