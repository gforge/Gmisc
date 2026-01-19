#' Default Many-to-One connector
#'
#' Loops over starts and connects each to the end using prConnect1.
#' @noRd
prConnectManyToOneStandard <- function(
  starts,
  end,
  type,
  subelmnt,
  lty_gp,
  arrow_obj
) {
  grobs <- lapply(starts, function(s) prConnect1(s, end, type, subelmnt, lty_gp, arrow_obj))
  structure(grobs, class = c("connect_boxes_list", "list"))
}

#' Many-to-One N connector (shared bend)
#' @noRd
prConnectManyToOneN <- function(
  starts,
  end,
  type,
  subelmnt,
  lty_gp,
  arrow_obj,
  split_pad = unit(2, "mm")
) {
  # If `end` is container-like, attempt to find a reasonable boxed target.
  if (!inherits(end, "box")) {
    fb <- prFindFirstBox(end)
    if (!is.null(fb)) end <- fb
  }

  assert_class(end, "box")

  s_coords <- lapply(starts, coords)
  e <- coords(end)

  # Determine vertical parameters (bend height and attachment edges)
  v_params <- prGetManyToOneNVerticalParameters(starts, end, split_pad)

  # Determine horizontal parameters (assigned slots on the end box)
  x_params <- prGetManyToOneNAssignedX(starts, s_coords, e, subelmnt)

  # Create the list of grobs
  grobs <- prMakeManyToOneNGrobs(
    s_coords = s_coords,
    v_params = v_params,
    x_params = x_params,
    lty_gp = lty_gp,
    arrow_obj = arrow_obj
  )

  structure(grobs, class = c("connect_boxes_list", "list"))
}


#' Calculate vertical parameters for Many-to-One N connector
#' @noRd
prGetManyToOneNVerticalParameters <- function(starts, end, split_pad) {
  # Shared bend point calculation
  bend_y <- prCalculateBendY(starts, end, split_pad = split_pad)

  # Attachment edges
  starts_above <- prStartsAbove(starts, end)
  e <- coords(end)
  if (starts_above) {
    end_attach <- e$top
    start_attach_fn <- function(s) s$bottom
  } else {
    end_attach <- e$bottom
    start_attach_fn <- function(s) s$top
  }

  list(
    bend_y = bend_y,
    end_attach = end_attach,
    start_attach_fn = start_attach_fn
  )
}


#' Calculate assigned X positions for Many-to-One N connector
#' @noRd
prGetManyToOneNAssignedX <- function(starts, s_coords, e, subelmnt) {
  # subelement x selection (for split boxes)
  sub_prefix <- if (missing(subelmnt)) "" else paste0(match.arg(subelmnt, c("right", "left")), "_")
  x_at <- function(pos, side = c("left", "right", "x")) {
    side <- match.arg(side)
    if (side == "x" && !is.null(pos[[paste0(sub_prefix, side)]])) {
      return(pos[[paste0(sub_prefix, side)]])
    }
    pos[[side]]
  }

  # Map end attachment slots to starts by left->right order so central alignment
  # can be detected and handled (center straight branch when appropriate).
  starts_x_vals <- vapply(s_coords, function(s) prGetNpcValue(x_at(s, "x"), "x"), numeric(1))
  xs_end_vals <- vapply(prEdgeSlots(e$left, e$right, n = length(starts)), function(u) prGetNpcValue(u, "x"), numeric(1))
  # Use shared assignment helper for stable mapping from starts -> end slots
  slots_res <- prAssignSlots(starts_x_vals, xs_end_vals)
  assigned_end_vals <- slots_res$assigned
  ord <- slots_res$ord

  # If centered straight branch desired, make the middle start attach to end center
  centered <- prNShouldUseCenteredBranch(starts, e)
  mid_sorted_idx <- NA
  if (centered) {
    mid_sorted_idx <- slots_res$mid_idx
    assigned_end_vals[mid_sorted_idx] <- prGetNpcValue(prConvert2Coords(e)$x, "x")
  }

  list(
    assigned_end_vals = assigned_end_vals,
    mid_sorted_idx = mid_sorted_idx,
    centered = centered,
    x_at_fn = x_at
  )
}


#' Create the list of grobs for Many-to-One N connector
#' @noRd
prMakeManyToOneNGrobs <- function(s_coords, v_params, x_params, lty_gp, arrow_obj) {
  lapply(seq_along(s_coords), function(i) {
    s <- s_coords[[i]]
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
      y = unit.c(v_params$start_attach_fn(s), v_params$bend_y, v_params$bend_y, v_params$end_attach)
    )
    lg <- linesGrob(x = line$x, y = line$y, gp = lty_gp, arrow = arrow_obj)
    structure(lg, line = line, class = unique(c("connect_boxes", class(lg))))
  })
}
