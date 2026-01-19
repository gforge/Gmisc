#' Default One-to-Many connector
#' @noRd
prConnectOneToManyStandard <- function(
  start,
  ends,
  type,
  subelmnt,
  lty_gp,
  arrow_obj
) {
  grobs <- lapply(ends, function(e) prConnect1(start, e, type, subelmnt, lty_gp, arrow_obj))
  structure(grobs, class = c("connect_boxes_list", "list"))
}

#' One-to-Many N connector (shared bend)
#' @noRd
prConnectOneToManyN <- function(
  start,
  ends,
  type,
  subelmnt,
  lty_gp,
  arrow_obj,
  split_pad = unit(2, "mm") # only used for shared-bend types
) {
  assert_class(start, "box")
  if (is.numeric(split_pad)) split_pad <- unit(split_pad, "mm")
  assert_class(split_pad, "unit")

  s <- coords(start)
  end_coords <- lapply(ends, coords)

  # Determine vertical parameters (bend height and attachment edges)
  v_params <- prGetOneToManyNVerticalParameters(s, end_coords, split_pad)

  # Determine horizontal parameters (assigned slots on end boxes)
  x_params <- prGetOneToManyNAssignedX(start, s, ends, end_coords, subelmnt)

  # Create the list of grobs
  grobs <- prMakeOneToManyNGrobs(
    s = s,
    end_coords = end_coords,
    v_params = v_params,
    x_params = x_params,
    lty_gp = lty_gp,
    arrow_obj = arrow_obj
  )

  structure(grobs, class = c("connect_boxes_list", "list"))
}


#' Calculate vertical parameters for One-to-Many N connector
#' @noRd
prGetOneToManyNVerticalParameters <- function(s, end_coords, split_pad) {
  # Use explicit conversion helper for clarity
  s_y <- prConvertHeightToMm(s$y)
  e_y <- vapply(end_coords, \(e) prConvertHeightToMm(e$y), numeric(1))

  dir_down <- s_y > max(e_y)
  dir_up <- s_y < min(e_y)
  if (!dir_down && !dir_up) dir_down <- mean(e_y) < s_y

  d <- lapply(end_coords, function(e) distance(s, e, type = "v", half = TRUE))
  d_min <- Reduce(unit.pmin, d)

  if (dir_down) {
    # midpoint relative to the closest end
    bend_y <- s$bottom - d_min
    bend_y <- unit.pmax(bend_y, unit(0, "npc"))
    bend_y <- unit.pmin(bend_y, s$bottom - split_pad)
    start_attach <- s$bottom
    end_attach_fn <- function(e) e$top
  } else {
    bend_y <- s$top + d_min
    bend_y <- unit.pmin(bend_y, unit(1, "npc"))
    bend_y <- unit.pmax(bend_y, s$top + split_pad)
    start_attach <- s$top
    end_attach_fn <- function(e) e$bottom
  }

  list(
    bend_y = bend_y,
    start_attach = start_attach,
    end_attach_fn = end_attach_fn
  )
}


#' Calculate assigned X positions for One-to-Many N connector
#' @noRd
prGetOneToManyNAssignedX <- function(start, s, ends, end_coords, subelmnt) {
  # subelement x selection (for split boxes)
  sub_prefix <- if (missing(subelmnt)) "" else paste0(match.arg(subelmnt, c("right", "left")), "_")
  x_at <- function(pos, side = c("left", "right", "x")) {
    side <- match.arg(side)
    if (side == "x" && !is.null(pos[[paste0(sub_prefix, side)]])) {
      return(pos[[paste0(sub_prefix, side)]])
    }
    pos[[side]]
  }

  # Map ends left->right and, if centered alignment holds, make middle end align with start
  end_x_vals <- vapply(end_coords, \(e) prGetNpcValue(x_at(e, "x"), "x"), numeric(1))
  # Use shared assignment helper for stable ordering
  slots_res <- prAssignSlots(end_x_vals, end_x_vals)
  assigned_end_vals <- slots_res$assigned
  ord <- slots_res$ord

  centered <- prNShouldUseCenteredBranch(ends, start)
  mid_sorted_idx <- NA
  if (centered) {
    mid_sorted_idx <- slots_res$mid_idx
    assigned_end_vals[mid_sorted_idx] <- prGetNpcValue(s$x, "x")
  }

  list(
    assigned_end_vals = assigned_end_vals,
    mid_sorted_idx = mid_sorted_idx,
    centered = centered,
    x_at_fn = x_at
  )
}


#' Create the list of grobs for One-to-Many N connector
#' @noRd
prMakeOneToManyNGrobs <- function(s, end_coords, v_params, x_params, lty_gp, arrow_obj) {
  lapply(seq_along(end_coords), function(i) {
    e <- end_coords[[i]]
    x_end <- unit(x_params$assigned_end_vals[i], "npc")

    # For the centered straight branch, force the start x coords to match the
    # assigned end position so the trunk is perfectly vertical.
    if (x_params$centered && i == x_params$mid_sorted_idx) {
      x_start0 <- x_end
    } else {
      x_start0 <- x_params$x_at_fn(s, "x")
    }

    line <- list(
      x = unit.c(x_start0, x_start0, x_end, x_end),
      y = unit.c(v_params$start_attach, v_params$bend_y, v_params$bend_y, v_params$end_attach_fn(e))
    )
    lg <- grid::linesGrob(x = line$x, y = line$y, gp = lty_gp, arrow = arrow_obj)
    structure(lg, line = line, class = c("connect_boxes", class(lg)))
  })
}
