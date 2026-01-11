prConnectManyToOne <- function(
  starts,
  end,
  type,
  subelmnt,
  lty_gp,
  arrow_obj,
  split_pad = unit(2, "mm")
) {
  assert_class(end, "box")

  # Default: just connect each start to the same end
  if (type != "N") {
    grobs <- lapply(starts, function(s) prConnect1(s, end, type, subelmnt, lty_gp, arrow_obj))
    return(structure(grobs, class = c("connect_boxes_list", "list")))
  }

  # N: shared bend (merge at same height)
  # Mirror of one-to-many logic:
  # - compute d_min = min(distance(start_i, end, type="v", half=TRUE))
  # - set bend_y = edge midpoint corresponding to that shortest connection
  #
  # Implementation idea:
  #   e <- coords(end)
  #   s_coords <- lapply(starts, coords)
  #   d_min <- Reduce(unit.pmin, lapply(s_coords, \(s) distance(s, e, type="v", half=TRUE)))
  #   Determine direction based on relative y positions (starts above end => merge above end; etc)
  #   Then construct each line with shared bend_y.
  #
  # (Use the same shared-bend code path as prConnectMany(), but swapping roles.)
  #
  # For now, implement by reusing prConnectMany() by flipping roles:
  #   connect end -> starts with N, but with arrow direction wrong.
  # Better: implement explicitly, same as your fan-out N but start list instead.

  s_coords <- lapply(starts, coords)
  e <- coords(end)

  y_mm <- function(u) convertHeight(u, "mm", valueOnly = TRUE)
  e_y <- y_mm(e$y)
  s_y <- vapply(s_coords, function(s) y_mm(s$y), numeric(1))

  dir_down <- mean(s_y) > e_y # starts are below end => lines go up; above => go down? pick consistent
  # I'd actually decide per-box in rendering, but for a shared bend pick a global direction:
  dir_down <- e_y > mean(s_y) # end above starts => incoming branches go up to end

  d_min <- Reduce(unit.pmin, lapply(s_coords, function(s) distance(s, e, type = "v", half = TRUE)))

  if (dir_down) {
    # end is above starts, so incoming branches go up; bend between end bottom and closest start top
    bend_y <- e$bottom - d_min
    bend_y <- unit.pmax(bend_y, unit(0, "npc"))
    bend_y <- unit.pmin(bend_y, e$bottom - split_pad)
    end_attach <- e$bottom
    start_attach <- function(s) s$top
  } else {
    bend_y <- e$top + d_min
    bend_y <- unit.pmin(bend_y, unit(1, "npc"))
    bend_y <- unit.pmax(bend_y, e$top + split_pad)
    end_attach <- e$top
    start_attach <- function(s) s$bottom
  }

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
  starts_x_vals <- vapply(s_coords, function(s) convertX(x_at(s, "x"), "npc", valueOnly = TRUE), numeric(1))
  xs_end_vals <- convertX(prEdgeSlots(e$left, e$right, n = length(starts)), "npc", valueOnly = TRUE)
  ord <- order(starts_x_vals)
  assigned_end_vals <- numeric(length(starts_x_vals))
  assigned_end_vals[ord] <- xs_end_vals

  # If centered straight branch desired, make the middle start attach to end center
  centered <- prNShouldUseCenteredBranch(starts, e)
  if (centered) {
    mid_sorted_idx <- ord[(length(ord) + 1) / 2]
    assigned_end_vals[mid_sorted_idx] <- convertX(prConvert2Coords(e)$x, "npc", valueOnly = TRUE)
  }

  grobs <- lapply(seq_along(s_coords), function(i) {
    s <- s_coords[[i]]

    x_end <- unit(assigned_end_vals[i], "npc")

    # For the centered straight branch, force the start x coords to match the
    # assigned end position so the trunk is perfectly vertical.
    if (centered && i == mid_sorted_idx) {
      x_start0 <- x_end
    } else {
      x_start0 <- x_at(s, "x")
    }

    line <- list(
      x = unit.c(x_start0, x_start0, x_end, x_end),
      y = unit.c(start_attach(s), bend_y, bend_y, end_attach)
    )
    lg <- linesGrob(x = line$x, y = line$y, gp = lty_gp, arrow = arrow_obj)
    structure(lg, line = line, class = c("connect_boxes", class(lg)))
  })

  structure(grobs, class = c("connect_boxes_list", "list"))
}
