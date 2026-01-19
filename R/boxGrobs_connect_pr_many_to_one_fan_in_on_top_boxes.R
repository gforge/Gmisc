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
    structure(g, line = line, class = unique(c("connect_boxes", class(g))))
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
  starts_x_units <- lapply(s_coords, \(s) x_at(s, "x"))
  starts_x_vals <- vapply(starts_x_units, \(u) prGetNpcValue(u, "x"), numeric(1))
  xs_end_vals <- vapply(xs_end, \(u) prGetNpcValue(u, "x"), numeric(1))
  ord <- order(starts_x_vals)

  # map left-to-right end slots to starts
  assigned_end_vals <- numeric(length(starts_x_vals))
  assigned_end_vals[ord] <- xs_end_vals

  # If centered straight branch desired, override middle assigned slot
  centered <- prNShouldUseCenteredBranch(starts, end)
  if (centered) {
    mid_sorted_idx <- ord[(length(ord) + 1) / 2]
    targ_center <- prGetNpcValue(prConvert2Coords(end)$x, "x")
    # Prefer the target center when available; otherwise fall back to the
    # start's x coordinate to guarantee a vertical trunk when conversions fail.
    if (!is.na(targ_center)) {
      assigned_end_vals[mid_sorted_idx] <- targ_center
    } else if (!is.na(starts_x_vals[mid_sorted_idx])) {
      assigned_end_vals[mid_sorted_idx] <- starts_x_vals[mid_sorted_idx]
    }
  }

  trunk_grobs <- lapply(seq_along(starts_x_units), function(i) {
    x_start <- starts_x_units[[i]]
    x_end <- unit(assigned_end_vals[i], "npc")

    # If this is the centered branch, force the trunk to be vertical by
    # using the assigned end value for both start and end x coordinates.
    if (centered && i == mid_sorted_idx) {
      x_start <- x_end
    } else {
      # Convert start to npc unit as well. If it's a unit-like object, use
      # prGetNpcValue to obtain the numeric npc and wrap back to a unit.
      if (!inherits(x_start, "unit")) {
        x_start <- unit(x_start, "npc")
      } else {
        x_start <- prGetNpcValue(x_start, "x") |> unit("npc")
      }
    }

    mk(
      x = unit.c(x_start, x_end),
      y = unit.c(bend_y, e$top + unit(0.5, "mm")),
      arrow = arrow_obj
    )
  })

  # If we used a centered straight branch, make the corresponding stem
  # vertical as well by matching its x coords to the assigned trunk x value.
  if (centered) {
    mid_sorted_idx <- ord[(length(ord) + 1) / 2]
    x_val_npc <- assigned_end_vals[mid_sorted_idx]
    if (!is.na(x_val_npc)) {
      x_unit <- unit(x_val_npc, "npc")
      old_stem <- stem_grobs[[mid_sorted_idx]]
      y_vals <- attr(old_stem, "line")$y
      stem_grobs[[mid_sorted_idx]] <- mk(x = unit.c(x_unit, x_unit), y = y_vals, arrow = NULL)
    }
  }

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
  # If `end` is container-like, attempt to find a reasonable boxed target.
  if (!inherits(end, "box")) {
    fb <- prFindFirstBox(end)
    if (!is.null(fb)) end <- fb
  }

  assert_class(end, "box")
  assert_class(lty_gp, "gpar")
  assert_class(arrow_obj, "arrow")
  if (is.numeric(margin)) margin <- unit(margin, "mm")
  if (is.numeric(split_pad)) split_pad <- unit(split_pad, "mm")

  bend_y <- prCalculateBendY(starts, end, edge = "top", margin = margin, split_pad = split_pad)
  xs_end <- pcCalculateXPositions(starts, end, margin)
  prGenerateLines(starts, end, bend_y, xs_end, lty_gp, arrow_obj, subelmnt)
}
