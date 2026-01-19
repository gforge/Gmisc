prIsBoxList <- function(x) {
  is.list(x) && length(x) > 0 && all(vapply(x, inherits, logical(1), "box"))
}

# If a list is a single-element wrapper containing a list-of-boxes, unwrap it.
# For example, a `Gmisc_list_of_boxes` object sometimes appears wrapped such
# that the actual boxes are one level deeper. This helper returns the unwrapped
# list-of-boxes when appropriate, or the original object otherwise.
prFlattenBoxListIfNeeded <- function(x) {
  if (length(x) == 1 && is.list(x[[1]]) && all(vapply(x[[1]], inherits, logical(1), "box"))) {
    return(x[[1]])
  }
  x
}

# If the first element of `x` is itself a list-of-boxes and there are other
# top-level elements too, unwrap to the first container. This handles cases
# like `list(list(box1, box2), other = list())` where the user likely passed
# a grouped element as the intended `start`/`end`.
prMaybeUnwrapFirstContainerBoxes <- function(x) {
  if (is.list(x) && length(x) > 1 && is.list(x[[1]]) && all(vapply(x[[1]], inherits, logical(1), "box"))) {
    return(x[[1]])
  }
  x
}

# Normalize list elements so that single-element wrapper elements that
# contain a box are unwrapped. This handles mixed lists like
# `list(box, list(box), box)` which should be treated as a list of boxes.
prNormalizeBoxElements <- function(x) {
  if (!is.list(x)) {
    return(x)
  }
  for (i in seq_along(x)) {
    if (is.list(x[[i]]) && length(x[[i]]) == 1 && inherits(x[[i]][[1]], "box")) {
      x[[i]] <- x[[i]][[1]]
    }
  }
  x
}

# Collapse a single-element list that is itself a boxed element into the bare box.
# This ensures that `connectGrob()` treats `list(box)` as a single `box` input
# rather than as a one-element list-of-boxes (which otherwise may cause both
# start and end to be lists and trigger the unsupported both-list error).
prCollapseSingleBoxList <- function(x) {
  if (is.list(x) && length(x) == 1 && inherits(x[[1]], "box")) {
    return(x[[1]])
  }
  x
}

prEdgeSlots <- function(left, right, n, margin = unit(0, "mm")) {
  if (is.numeric(margin)) margin <- unit(margin, "mm")
  stopifnot(inherits(margin, "unit"))
  stopifnot(n >= 1)

  leftWithMargin <- left + margin
  rightMinusMargin <- right - margin
  if (n == 1) {
    return(unit.c((leftWithMargin + rightMinusMargin) / 2))
  }

  step <- (rightMinusMargin - leftWithMargin) / (n + 1)
  xs <- leftWithMargin + step * seq_len(n)
  unit.c(xs)
}

# Find the first boxed element in a possibly nested container. Useful for
# making many-to-one connectors robust to inputs where the intended `end`
# was wrapped in container lists by layout pipelines.
prFindFirstBox <- function(x) {
  if (inherits(x, "box")) {
    return(x)
  }
  if (!is.list(x)) {
    return(NULL)
  }
  for (el in x) {
    if (inherits(el, "box")) {
      return(el)
    }
    if (is.list(el)) {
      found <- prFindFirstBox(el)
      if (!is.null(found)) {
        return(found)
      }
    }
  }
  NULL
}

#' Determine if start boxes are positioned above the end box
#'
#' Internal helper that compares the mean vertical position of the `starts` boxes
#' with the `end` box y position. Values are converted to millimetres for a
#' device-independent comparison.
#'
#' @param starts A `list` of `boxGrob` objects or a single `boxGrob`.
#' @param end A `boxGrob` object.
#' @return `TRUE` if the mean y of `starts` is above `end`'s y, otherwise `FALSE`.
#' @keywords internal
#' @noRd
prStartsAbove <- function(starts, end) {
  if (inherits(starts, "box")) starts <- list(starts)
  s_coords <- lapply(starts, coords)
  e <- coords(end)
  mean(vapply(s_coords, function(s) prConvertHeightToMm(s$y), numeric(1))) > prConvertHeightToMm(e$y)
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

  # Use centralized helper for robust npc extraction
  x_vals <- vapply(coords_items, function(s) prGetNpcValue(s$x, "x"), numeric(1))
  ord <- order(x_vals)
  middle_idx_sorted <- ord[(n + 1) / 2]
  middle_x <- x_vals[middle_idx_sorted]

  targ_coords <- prConvert2Coords(target)
  targ_x <- prGetNpcValue(targ_coords$x, "x")

  # Prefer converting declared width (npc numeric). If that fails (e.g. units
  # not resolvable in the current device/context) fall back to left/right
  # edge difference using `prGetNpcValue`. Finally default to 1 so lack of a
  # convertible width doesn't prevent centered-branch behavior.
  targ_w <- prGetNpcSize(targ_coords$width, "x")
  if (is.na(targ_w) || targ_w <= 0) {
    left <- prGetNpcValue(targ_coords$left, "x")
    right <- prGetNpcValue(targ_coords$right, "x")
    if (!is.na(left) && !is.na(right)) {
      targ_w <- right - left
    } else {
      targ_w <- 1
    }
  }

  abs(middle_x - targ_x) <= tolerance * targ_w
}

# Assign end-slot positions to start positions in a stable, shared way.
# Returns list with `assigned` numeric (length n), `ord` ordering indices,
# and `mid_idx` the index of the chosen center start.
prAssignSlots <- function(starts_x_vals, xs_end_vals) {
  n <- length(starts_x_vals)
  if (n == 0L) {
    return(list(assigned = numeric(0), ord = integer(0), mid_idx = integer(0)))
  }
  ord <- order(starts_x_vals)
  assigned <- numeric(n)
  assigned[ord] <- xs_end_vals
  mid_pos <- ceiling(n / 2)
  mid_idx <- ord[mid_pos]
  list(assigned = assigned, ord = ord, mid_idx = mid_idx)
}

#' Calculate bend Y coordinate for shared-bend connectors
#'
#' Compute a suitable vertical bend point for shared-bend connectors.
#' The function computes the minimum vertical distance between starts and the
#' end box and returns the midpoint, clamped by `split_pad`.
#'
#' @param starts A `list` of start `boxGrob` objects.
#' @param end A single end `boxGrob` object.
#' @param edge Which edge of the end box to use for distance calculation and
#'   clamping: `"auto"` (closest), `"top"`, or `"bottom"`.
#' @param margin A `grid::unit` (unused, kept for API compatibility).
#' @param split_pad Padding to enforce around the shared bend point.
#' @return A `grid::unit` giving the y-coordinate of the shared bend point.
#' @keywords internal
#' @noRd
prCalculateBendY <- function(
  starts,
  end,
  edge = c("auto", "top", "bottom"),
  margin = unit(2, "mm"), split_pad = unit(0, "mm")
) {
  edge <- match.arg(edge)
  s_coords <- lapply(starts, coords)
  e <- coords(end)

  # Calculate the minimum vertical distance between each start box and end box
  d_min <- Reduce(unit.pmin, lapply(s_coords, function(s) distance(s, e, type = "v", half = TRUE)))

  # Ensure d_min is safely mixed with npc coordinates by converting to npc
  # before arithmetic with e$top/e$bottom. `distance()` may return device
  # units (mm) so converting avoids unit-mix errors.
  d_min_npc <- tryCatch(convertY(d_min, unitTo = "npc"), error = function(e) NA)
  if (inherits(d_min_npc, "unit")) {
    d_min_use <- d_min_npc
  } else {
    # Fall back: coerce numeric or NA to a small npc offset
    d_min_num <- if (is.numeric(d_min)) d_min else NA_real_
    if (is.na(d_min_num)) d_min_use <- unit(0.01, "npc") else d_min_use <- unit(d_min_num, "npc")
  }

  # Determine if the start boxes are above the end box
  starts_above <- prStartsAbove(starts, end)

  if (edge == "top" || (edge == "auto" && starts_above)) {
    # Bend is between starts bottom and end top
    bend_y <- e$top + d_min_use
    bend_y <- unit.pmax(bend_y, e$top + split_pad)
  } else {
    # Bend is between starts top and end bottom
    bend_y <- e$bottom - d_min_use
    bend_y <- unit.pmin(bend_y, e$bottom - split_pad)
  }

  return(bend_y)
}
