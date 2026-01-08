#' Connect boxes with arrows
#'
#' Creates connectors between boxes.
#'
#' The function supports:
#'
#' - **One-to-one**: a single start box connected to a single end box.
#' - **One-to-many**: a single start box connected to multiple end boxes.
#' - **Many-to-one**: multiple start boxes connected to a single end box.
#'
#' Many-to-many connections are **not supported**.
#'
#' If either `start` or `end` is a list, a list of connector grobs is returned
#' (one per connection). Otherwise a single connector grob is returned.
#'
#' Each connector stores its computed geometry in `attr(x, "line")`
#' (or for each element when a list is returned). This can be reused to construct
#' custom connectors using the calculated coordinates.
#'
#' ## Connector types
#'
#' `type` controls the connector shape:
#'
#' - `"vertical"`: straight vertical connector
#' - `"horizontal"`: straight horizontal connector
#' - `"L"`: vertical then horizontal (direction chosen automatically)
#' - `"-"`: straight horizontal connector at the end box y-position
#' - `"Z"`: horizontal connector with two 90-degree turns
#' - `"N"`: vertical connector with one horizontal segment  
#'   When connecting to or from multiple boxes, all connectors share the same bend height.
#' - `"fan_in_top"`: many-to-one connector merging onto the *top edge* of the end box  
#'   Attachment points are evenly distributed along the edge (with optional `margin`),
#'   and all connectors share a common bend height.
#'
#' For `type = "N"` and `type = "fan_in_top"` with multi-box connections, a shared
#' bend position is computed so that the horizontal segment aligns visually across
#' all connectors.
#'
#' ## Split boxes
#'
#' When connecting to or from a `boxPropGrob`, `subelmnt` controls whether the left
#' or right sub-box x-coordinate is used as the anchor point.
#'
#' @param start A `boxGrob`/`boxPropGrob`, or a list of boxes (many-to-one).
#' @param end A `boxGrob`/`boxPropGrob`, or a list of boxes (one-to-many).
#' @param type Connector type, see Details.
#' @param subelmnt For split boxes, which sub-element to anchor to: `"left"` or `"right"`.
#' @param lty_gp A [grid::gpar()] controlling line appearance. Can also be set globally via
#'   `options(connectGrob = ...)`.
#' @param arrow_obj Arrow specification created with [grid::arrow()]. Can also be set globally via
#'   `options(connectGrobArrow = ...)`.
#' @param split_pad Padding around the shared bend point for multi-box connections.
#'   Numeric values are interpreted as millimeters.
#' @param margin For `type = "fan_in_top"`, the margin applied at the left and right ends of the
#'   end box top edge before distributing attachment points. Numeric values are interpreted
#'   as millimeters.
#'
#' @return
#' - One-to-one: a [grid::grob()] with class `"connect_boxes"`.
#' - One-to-many or many-to-one: a list of grobs with class `"connect_boxes_list"`.
#'
#' @family flowchart components
#' @export
#' @rdname connect
#' @example inst/examples/connectGrob_example.R
#' @md
connectGrob <- function(
    start,
    end,
    type = c("vertical", "horizontal", "L", "-", "Z", "N", "fan_in_top"),
    subelmnt = c("right", "left"),
    lty_gp = getOption("connectGrob", default = gpar(fill = "black")),
    arrow_obj = getOption("connectGrobArrow", default = arrow(ends = "last", type = "closed")),
    split_pad = unit(2, "mm"),
    margin = NULL
) {
  type <- match.arg(type)
  
  if (prIsBoxList(start) && prIsBoxList(end)) {
    stop("Both 'start' and 'end' cannot be lists (not implemented).", call. = FALSE)
  }
  
  if (prIsBoxList(start)) {
    if (type == "fan_in_top") {
      return(prConnectManyToOneFanTop(
        starts = start,
        end = end,
        subelmnt = subelmnt,
        lty_gp = lty_gp,
        arrow_obj = arrow_obj,
        margin = margin,
        split_pad = split_pad
      ))
    }
    return(prConnectManyToOne(start, end, type, subelmnt, lty_gp, arrow_obj, split_pad = split_pad))
  }
  
  if (prIsBoxList(end)) {
    return(prConnectOneToMany(start, end, type, subelmnt, lty_gp, arrow_obj, split_pad = split_pad))
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

