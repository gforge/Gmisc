# Internal helpers for fan_in_center connector -------------------------------------

# Compute bus extent based on starts x positions and end box center
prComputeFanCenterBus <- function(starts, end, subelmnt, margin = unit(2, "mm"), pad_mm = unit(1, "mm")) {
  s_coords <- lapply(starts, coords)
  e_coords <- prConvert2Coords(end)

  # subelement prefix for boxPropGrob anchors
  sub_prefix <- if (missing(subelmnt)) "" else paste0(match.arg(subelmnt, c("right", "left")), "_")
  x_at <- function(pos) {
    if (!is.null(pos[[paste0(sub_prefix, "x")]])) {
      return(pos[[paste0(sub_prefix, "x")]])
    }
    pos[["x"]]
  }

  # Get actual start x positions (not evenly-spaced slots) using the
  # centralized safe conversion helper. This avoids repeating tryCatch/as.numeric
  # patterns and provides a predictable NA fallback when conversion fails.
  starts_x_vals <- vapply(s_coords, \(s) prGetNpcValue(x_at(s), "x"), numeric(1))

  # Try to read end center x; fall back to sensible defaults if conversion
  # fails. Prefer end center, then span of valid starts, then center (0.5).
  end_center_x <- prGetNpcValue(e_coords$x, "x")

  valid_x <- starts_x_vals[!is.na(starts_x_vals)]
  if (is.na(end_center_x)) {
    if (length(valid_x) > 0) {
      end_center_x <- mean(range(valid_x))
    } else {
      end_center_x <- 0.5
    }
  }

  if (length(valid_x) == 0) {
    # No valid start x coordinates: use a small span around the end center
    bus_min_val <- max(0, end_center_x - 0.02)
    bus_max_val <- min(1, end_center_x + 0.02)
  } else {
    bus_min_val <- min(valid_x)
    bus_max_val <- max(valid_x)
  }

  list(
    bus_min = unit(bus_min_val, "npc"),
    bus_max = unit(bus_max_val, "npc"),
    trunk_x = unit(end_center_x, "npc"),
    starts_x_vals = starts_x_vals
  )
}

# Make vertical stem grobs for each start to bend_y
prMakeFanCenterStems <- function(starts, end, bend_y, starts_above, subelmnt, lty_gp) {
  s_coords <- lapply(starts, coords)
  sub_prefix <- if (missing(subelmnt)) "" else paste0(match.arg(subelmnt, c("right", "left")), "_")
  x_at <- function(pos) {
    if (!is.null(pos[[paste0(sub_prefix, "x")]])) {
      return(pos[[paste0(sub_prefix, "x")]])
    }
    pos[["x"]]
  }
  mk <- function(x, y) {
    ux <- unit.c(x)
    uy <- unit.c(y)
    g <- grid::linesGrob(x = ux, y = uy, gp = lty_gp)
    structure(g, line = list(x = ux, y = uy), class = unique(c("connect_boxes", class(g))))
  }
  lapply(seq_along(s_coords), function(i) {
    s <- s_coords[[i]]
    x0 <- x_at(s)
    if (!inherits(x0, "unit")) {
      x0 <- unit(x0, "npc")
    } else {
      x0 <- unit(prGetNpcValue(x0, "x"), "npc")
    }
    y0 <- if (starts_above) s$bottom else s$top
    mk(unit.c(x0, x0), unit.c(y0, bend_y))
  })
}

# Make the horizontal bus grob
prMakeFanCenterBus <- function(bus_min, bus_max, bend_y, lty_gp) {
  ux <- unit.c(bus_min, bus_max)
  uy <- unit.c(bend_y, bend_y)
  g <- grid::linesGrob(x = ux, y = uy, gp = lty_gp)
  structure(g, line = list(x = ux, y = uy), class = unique(c("connect_boxes", class(g))))
}

# Make a vertical trunk grob from trunk_x at bend_y to the end box center
prMakeFanCenterTrunk <- function(trunk_x, bend_y, end, starts_above, arrow_obj, lty_gp) {
  ecoords <- prConvert2Coords(end)

  # target y is box edge (top or bottom) plus small offset
  if (starts_above) {
    targ_y <- ecoords$top + unit(0.5, "mm")
  } else {
    targ_y <- ecoords$bottom - unit(0.5, "mm")
  }

  ux <- unit.c(trunk_x, trunk_x)
  uy <- unit.c(bend_y, targ_y)
  g <- grid::linesGrob(x = ux, y = uy, gp = lty_gp, arrow = arrow_obj)
  structure(g, line = list(x = ux, y = uy), class = unique(c("connect_boxes", class(g))))
}


#' Generate connector for fan-in-to-center
#'
#' Create a fan-in connector that merges stems onto a horizontal bus at a
#' shared bend height, and then a single centered trunk with an arrow pointing
#' to the center of the end box. Useful when a single arrow should represent
#' the merged flow.
#'
#' @param starts A list of start `boxGrob` objects.
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
prConnectManyToOneFanCenter <- function(
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

  # Calculate shared bend point
  bend_y <- prCalculateBendY(starts, end, margin = margin, split_pad = split_pad)

  # Compute bus extent and trunk x position (end box center)
  bus_res <- prComputeFanCenterBus(starts, end, subelmnt, margin = margin, pad_mm = unit(1, "mm"))
  bus_min <- bus_res$bus_min
  bus_max <- bus_res$bus_max
  trunk_x <- bus_res$trunk_x

  # Determine vertical relationship
  starts_above <- prStartsAbove(starts, end)

  # Make stems, bus and trunk
  stem_grobs <- prMakeFanCenterStems(starts, end, bend_y, starts_above, subelmnt, lty_gp)
  bus_grob <- prMakeFanCenterBus(bus_min, bus_max, bend_y, lty_gp)
  trunk_grob <- prMakeFanCenterTrunk(trunk_x, bend_y, end, starts_above, arrow_obj, lty_gp)

  structure(c(stem_grobs, list(bus_grob), list(trunk_grob)), class = c("connect_boxes_list", "list"))
}
