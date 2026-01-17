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
#' - `"fan_in_center"`: many-to-one connector that aggregates stems onto a horizontal
#'   bus (shared at the computed bend height) and then a single centered trunk with an
#'   arrow that points to the *center* of the end box. Useful when you want a single
#'   arrow to represent the merged flow (e.g. a middle trunk bus).
#'
#' For `type = "N"` and `type = "fan_in_top"` with multi-box connections, a shared
#' bend position is computed so that the horizontal segment aligns visually across
#' all connectors.
#'
#' ## Labels
#'
#' For one-to-one connectors you can add a text label (for example `"yes"` / `"no"`).
#' The label is placed near the midpoint of the connector.

#' The label is drawn with a white background for readability.
#' Use `label_pad` to control padding around the text and `label_offset` to move
#' the label away from the connector.
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
#' @param label Optional text label for one-to-one connectors (e.g. `"yes"` / `"no"`).
#'   Only supported when both `start` and `end` are single boxes.
#' @param label_gp A [grid::gpar()] controlling label appearance.
#' @param label_pos Where to place the label along the connector: `"mid"`, `"near_start"`, or `"near_end"`.
#' @param label_offset Offset for the label away from the connector line.
#' @param label_pad Padding inside the label background. Numeric values are interpreted
#'   as millimeters.
#' @param label_bg_gp A [grid::gpar()] controlling label background appearance.
#'   Defaults to a white background with no border.
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
  type = c("vertical", "horizontal", "L", "-", "Z", "N", "fan_in_top", "fan_in_center"),
  subelmnt = c("right", "left"),
  lty_gp = getOption("connectGrob", default = gpar(fill = "black")),
  arrow_obj = getOption("connectGrobArrow", default = arrow(ends = "last", type = "closed")),
  split_pad = unit(2, "mm"),
  margin = unit(2, "mm"),
  label = NULL,
  label_gp = grid::gpar(cex = 0.9),
  label_bg_gp = grid::gpar(fill = "white", col = NA),
  label_pad = unit(1.5, "mm"),
  label_pos = c("mid", "near_start", "near_end"),
  label_offset = unit(2, "mm")
) {
  type <- match.arg(type)
  label_pos <- match.arg(label_pos)

  # Normalize possible single-element wrapped lists so strategy detection
  # correctly identifies lists of boxes (one-to-many / many-to-one).
  start <- prFlattenBoxListIfNeeded(start)
  end <- prFlattenBoxListIfNeeded(end)

  # Also unwrap a first nested container when it clearly contains boxes so that
  # the connector functions operate on the inner boxes rather than an outer
  # grouping wrapper (this handles cases produced by spread/align pipelines).
  start <- prMaybeUnwrapFirstContainerBoxes(start)

  # If the *original* end is a list of group containers (each a list of boxes),
  # the intention is usually to connect to the group's first element (header).
  # Convert each container to its first boxed element *before* any unwrapping
  # logic runs so we don't accidentally drop other groups.
  if (is.list(end) && length(end) > 0 && all(vapply(end, is.list, logical(1)))) {
    is_container_of_boxes <- vapply(end, function(el) {
      length(el) > 0 && all(vapply(el, inherits, logical(1), "box"))
    }, logical(1))
    if (all(is_container_of_boxes)) {
      end <- lapply(end, function(el) el[[1]])
    }
  }

  end <- prMaybeUnwrapFirstContainerBoxes(end)

  # Ensure elements are normalized so single-element wrappers are unwrapped
  # (e.g., list(box, list(box), box) -> list(box, box, box)).
  start <- prNormalizeBoxElements(start)
  end <- prNormalizeBoxElements(end)

  # Collapse single-element lists containing a box into the bare box so that
  # passing `list(box)` does not accidentally make both `start` and `end` be
  # lists (which is an unsupported many-to-many case).
  start <- prCollapseSingleBoxList(start)
  end <- prCollapseSingleBoxList(end)


  # If any top-level elements of `end` are containers (lists) take their
  # first element as the representative header when that first element is a
  # box. This is a permissive rule that handles mixed inputs produced by
  # spread/align pipelines without failing.
  # Only treat a *multi-element* list as a set of groups; if `end` is a
  # single-element list (e.g., a boxed element wrapped as a list) we should
  # not unwrap it here as that is used by other connector strategies.
  if (!inherits(end, "box") && is.list(end) && length(end) > 1 && any(vapply(end, is.list, logical(1)))) {
    end <- lapply(end, function(el) {
      if (is.list(el) && length(el) > 0 && inherits(el[[1]], "box")) {
        return(el[[1]])
      }
      el
    })
  }

  # If we are connecting *many-to-one* (multiple starts to a single end) it is
  # common for layout pipelines to produce a container-like structure for the
  # end (for example when using `.subelement` or grouped spreads). If such a
  # container accidentally reached this point, attempt to find the most
  # appropriate boxed element to use as the end target (prefer the first
  # boxed child). This keeps many-to-one strategies robust to mixed inputs.
  if ((prIsBoxList(start) || inherits(start, "Gmisc_list_of_boxes")) && !inherits(end, "box") && is.list(end)) {
    box_pos <- which(vapply(end, inherits, logical(1), "box"))
    if (length(box_pos) >= 1) {
      end <- end[[box_pos[1]]]
    }
  }


  strategy <- prGetConnectorStrategy(start, end, type)
  prCalculateConnector(
    strategy,
    start = start,
    end = end,
    subelmnt = subelmnt,
    lty_gp = lty_gp,
    arrow_obj = arrow_obj,
    split_pad = split_pad,
    margin = margin,
    label = label,
    label_gp = label_gp,
    label_bg_gp = label_bg_gp,
    label_pad = label_pad,
    label_pos = label_pos,
    label_offset = label_offset
  )
}


#' The print/plot calls the \code{\link[grid]{grid.draw}} function on the object
#' @param x The grob to print/plot
#' @param ... Passed to \code{\link[grid]{grid.draw}}
#' @rdname connect
#' @export
print.connect_boxes <- function(x, ...) {
  grid.draw(x, ...)
  # If labels attached, draw them on top
  labels <- attr(x, "connector_labels")
  if (!is.null(labels)) {
    gp <- attr(x, "connector_label_gp")
    if (is.null(gp)) gp <- gpar(cex = 0.9)
    bg_gp <- attr(x, "connector_label_bg_gp")
    if (is.null(bg_gp)) bg_gp <- gpar(fill = "white", col = NA)
    off <- attr(x, "connector_label_offset")
    if (is.null(off)) off <- list(x_offset = unit(0, "mm"), y_offset = unit(0, "mm"))
    labelConnector(list(x), labels = labels, x_offset = off$x_offset, y_offset = off$y_offset, gp = gp, bg_gp = bg_gp)
  }

  invisible(x)
}

#' @rdname connect
#' @export
plot.connect_boxes <- print.connect_boxes


#' @rdname connect
#' @export
print.connect_boxes_list <- function(x, ...) {
  for (g in x) grid.draw(g, ...)
  # If labels attached, draw them on top
  labels <- attr(x, "connector_labels")
  if (!is.null(labels)) {
    gp <- attr(x, "connector_label_gp")
    if (is.null(gp)) gp <- gpar(cex = 0.9)
    bg_gp <- attr(x, "connector_label_bg_gp")
    if (is.null(bg_gp)) bg_gp <- gpar(fill = "white", col = NA)
    off <- attr(x, "connector_label_offset")
    if (is.null(off)) off <- list(x_offset = unit(0, "mm"), y_offset = unit(0, "mm"))
    labelConnector(x, labels = labels, x_offset = off$x_offset, y_offset = off$y_offset, gp = gp, bg_gp = bg_gp)
  }

  invisible(x)
}

#' @rdname connect
#' @export
plot.connect_boxes_list <- print.connect_boxes_list
