#' Determine if start boxes are positioned above the end box
#'
#' Internal helper that compares the mean vertical position of the `starts` boxes
#' with the `end` box y position. Values are converted to millimetres for a
#' device-independent comparison.
#'
#' @param starts A `list` of `boxGrob` objects representing the start boxes.
#' @param end A `boxGrob` object representing the end box.
#' @return `TRUE` if the mean y of `starts` is above `end`'s y, otherwise `FALSE`.
#' @keywords internal
#' @noRd
prStartsAbove <- function(starts, end) {
  s_coords <- lapply(starts, coords)
  e <- coords(end)
  y_mm <- function(u) convertY(u, unitTo = "mm", valueOnly = TRUE)
  mean(vapply(s_coords, function(s) y_mm(s$y), numeric(1))) > y_mm(e$y)
}

#' Determine whether an N connector can use a straight center branch
#'
#' For an N (shared-bend) layout, return TRUE when there are an odd number of
#' boxes and the middle box is aligned with the counterpart (within `tolerance`
#' fraction of the counterpart's width). This applies to both many-to-one and
#' one-to-many scenarios.
#'
#' @param items A list of boxes (starts or ends depending on context).
#' @param target The counterpart box (end for many-to-one, start for one-to-many).
#' @param tolerance Fraction of `target` width for allowable offset (default 0.1).
#' @return `TRUE` if the N connector should place a straight centered branch.
#' @keywords internal
#' @noRd
prNShouldUseCenteredBranch <- function(items, target, tolerance = 0.1) {
  n <- length(items)
  if (n %% 2 == 0) {
    return(FALSE)
  }
  coords_items <- lapply(items, coords)
  x_vals <- vapply(coords_items, function(s) convertX(s$x, "npc", valueOnly = TRUE), numeric(1))
  ord <- order(x_vals)
  middle_idx_sorted <- ord[(n + 1) / 2]
  middle_x <- x_vals[middle_idx_sorted]
  targ_x <- convertX(prConvert2Coords(target)$x, "npc", valueOnly = TRUE)
  targ_w <- convertWidth(prConvert2Coords(target)$width, "npc", valueOnly = TRUE)
  abs(middle_x - targ_x) <= tolerance * targ_w
}

#' Calculate bend Y coordinate for fan-in connectors
#'
#' Compute a suitable vertical bend point for fan-in connectors such that all
#' branches share a visually-aligned horizontal level. The function computes the
#' minimum vertical clearance required between starts and the end box and clamps
#' the bend point with `split_pad` to avoid overlapping the end box.
#'
#' @param starts A `list` of start `boxGrob` objects.
#' @param end A single end `boxGrob` object.
#' @param margin A `grid::unit` used when computing attachment slots (unused here,
#'   but kept for API symmetry). Numeric values are interpreted as millimetres.
#' @param split_pad Padding to enforce around the shared bend point. Numeric
#'   values are interpreted as millimetres.
#' @return A `grid::unit` giving the y-coordinate of the shared bend point.
#' @keywords internal
#' @noRd
prCalculateBendY <- function(starts, end, margin = unit(2, "mm"), split_pad = unit(0, "mm")) {
  s_coords <- lapply(starts, coords)
  e <- coords(end)

  # Calculate the minimum vertical distance between each start box and end box
  d_min <- Reduce(unit.pmin, lapply(s_coords, function(s) distance(s, e, type = "v", half = TRUE)))

  # Determine if the start boxes are above the end box
  starts_above <- prStartsAbove(starts, end)

  if (starts_above) {
    bend_y <- e$top + d_min
    bend_y <- unit.pmax(bend_y, e$top + split_pad)
  } else {
    bend_y <- e$top - d_min
    bend_y <- unit.pmin(bend_y, e$top - split_pad)
  }

  return(bend_y)
}

#' Calculate evenly spaced attachment X positions on end box top
#'
#' Given a target `end` box and a number of `starts`, calculate `n` attachment
#' x positions along the end box top edge, applying `margin`. The returned
#' positions are converted to `npc` units to make the layout robust when the
#' plotting device or viewport is resized.
#'
#' @param starts A `list` of start boxes (only length is used).
#' @param end A `boxGrob` for which top-edge slots are required.
#' @param margin Margin applied to the left/right ends of the top edge. Numeric
#'   values are interpreted as millimetres.
#' @return A `unit` vector of length `length(starts)` with attachment x-positions
#'   in `npc` units.
#' @keywords internal
#' @noRd
pcCalculateXPositions <- function(starts, end, margin = unit(2, "mm")) {
  e <- coords(end)
  n <- length(starts)
  xs_end <- prEdgeSlots(e$left, e$right, n = n, margin = margin)
  # Convert attachment x positions to npc units for stable resizing
  xs_end <- convertX(xs_end, unitTo = "npc")
  return(xs_end)
}

#' Generate connector line grobs for fan-in-on-top
#'
#' Create the stem and trunk line grobs that visually connect `starts` to the
#' top edge of `end`. Stems are vertical segments from each start to the shared
#' `bend_y`, and trunks are diagonal segments from the bend to the respective
#' end attachment slots. The returned list contains grobs with class
#' `connect_boxes` and the combined result has class `connect_boxes_list`.
#'
#' @param starts A `list` of start `boxGrob` objects.
#' @param end The end `boxGrob` object.
#' @param bend_y A `unit` giving the common bend y-coordinate.
#' @param xs_end A `unit` vector (npc) with target x-positions on the end box top.
#' @param lty_gp A `grid::gpar` describing line appearance.
#' @param arrow_obj A `grid::arrow` object for arrowheads on trunks.
#' @param subelmnt Optional sub-element specifier for `boxPropGrob` anchors.
#' @return A `list` of `connect_boxes` grobs with class `connect_boxes_list`.
#' @keywords internal
#' @noRd
prGenerateLines <- function(starts, end, bend_y, xs_end, lty_gp, arrow_obj, subelmnt) {
  s_coords <- lapply(starts, coords)
  e <- coords(end)

  sub_prefix <- if (missing(subelmnt)) "" else paste0(match.arg(subelmnt, c("right", "left")), "_")
  x_at <- function(pos, side = c("left", "right", "x")) {
    side <- match.arg(side)
    if (side == "x" && !is.null(pos[[paste0(sub_prefix, side)]])) {
      return(pos[[paste0(sub_prefix, side)]])
    }
    pos[[side]]
  }

  starts_above <- prStartsAbove(starts, end)

  mk <- function(x, y, arrow = NULL) {
    ux <- unit.c(x)
    uy <- unit.c(y)
    line <- list(x = ux, y = uy)
    g <- grid::linesGrob(x = ux, y = uy, gp = lty_gp, arrow = arrow)
    structure(g, line = line, class = c("connect_boxes", class(g)))
  }

  # 1) stems: each start -> bend_y (no arrow)
  stem_grobs <- lapply(seq_along(s_coords), function(i) {
    s <- s_coords[[i]]
    x0 <- x_at(s, "x")
    # Ensure x0 is an npc unit for consistent behavior on resize
    if (!inherits(x0, "unit")) {
      x0 <- unit(x0, "npc")
    } else {
      x0 <- convertX(x0, unitTo = "npc")
    }
    y0 <- if (starts_above) s$bottom else s$top

    mk(
      x = unit.c(x0, x0),
      y = unit.c(y0, bend_y),
      arrow = NULL
    )
  })

  # 2) trunks: assign end slots to starts by their left->right order so
  #    trunks meet the end's top edge in a visually consistent way. Also, when
  #    there is an odd number of starts and the middle one is aligned with the
  #    end (within tolerance) make that branch target the end center (straight).
  starts_x_units <- lapply(s_coords, function(s) x_at(s, "x"))
  starts_x_vals <- vapply(starts_x_units, function(u) convertX(u, "npc", valueOnly = TRUE), numeric(1))
  xs_end_vals <- convertX(xs_end, "npc", valueOnly = TRUE)
  ord <- order(starts_x_vals)

  # map left-to-right end slots to starts
  assigned_end_vals <- numeric(length(starts_x_vals))
  assigned_end_vals[ord] <- xs_end_vals

  # If centered straight branch desired, override middle assigned slot
  centered <- prNShouldUseCenteredBranch(starts, end)
  if (centered) {
    mid_sorted_idx <- ord[(length(ord) + 1) / 2]
    assigned_end_vals[mid_sorted_idx] <- convertX(prConvert2Coords(end)$x, "npc", valueOnly = TRUE)
  }

  trunk_grobs <- lapply(seq_along(starts_x_units), function(i) {
    x_start <- starts_x_units[[i]]
    x_end <- unit(assigned_end_vals[i], "npc")

    # Convert start to npc unit as well
    if (!inherits(x_start, "unit")) {
      x_start <- unit(x_start, "npc")
    } else {
      x_start <- convertX(x_start, unitTo = "npc")
    }

    mk(
      x = unit.c(x_start, x_end),
      y = unit.c(bend_y, e$top + unit(0.5, "mm")),
      arrow = arrow_obj
    )
  })

  structure(c(stem_grobs, trunk_grobs),
    class = c("connect_boxes_list", "list")
  )
}


#' Create a fan-in connector merging onto the top of an end box
#'
#' High-level helper that coordinates the calculation of the bend point and the
#' attachment positions and then generates the line grobs that join multiple
#' `starts` to a single `end` box using the fan-in-on-top layout.
#'
#' @param starts A `list` of `boxGrob` starts.
#' @param end A single `boxGrob` end target.
#' @param subelmnt Optional sub-element specifier for split boxes (`"left"` or
#'   `"right"`).
#' @param lty_gp A `grid::gpar` controlling line appearance.
#' @param arrow_obj A `grid::arrow` object for arrowheads.
#' @param margin Margin (mm) applied when computing end attachment slots.
#' @param split_pad Padding around the shared bend point (mm) used to avoid
#'   overlapping the end box.
#' @return A `connect_boxes_list` (list of grobs) representing the connector.
#' @keywords internal
#' @noRd
prConnectManyToOneFanTop <- function(
  starts,
  end,
  subelmnt,
  lty_gp,
  arrow_obj,
  margin = unit(2, "mm"),
  split_pad = unit(0, "mm")
) {
  assert_class(end, "box")
  assert_class(lty_gp, "gpar")
  assert_class(arrow_obj, "arrow")
  if (is.numeric(margin)) margin <- unit(margin, "mm")
  if (is.numeric(split_pad)) split_pad <- unit(split_pad, "mm")

  bend_y <- prCalculateBendY(starts, end, margin = margin, split_pad = split_pad)
  xs_end <- pcCalculateXPositions(starts, end, margin)
  prGenerateLines(starts, end, bend_y, xs_end, lty_gp, arrow_obj, subelmnt)
}
