# Function to calculate the Y-coordinate for the bend point
prCalculateBendY <- function(starts, end, margin = unit(2, "mm"), split_pad = unit(0, "mm")) {
  s_coords <- lapply(starts, coords)
  e <- coords(end)
  
  # Calculate the minimum vertical distance between each start box and end box
  d_min <- Reduce(unit.pmin, lapply(s_coords, function(s) distance(s, e, type = "v", half = TRUE)))
  
  # Determine if the start boxes are above the end box
  y_mm <- function(u) convertHeight(u, "mm", valueOnly = TRUE)
  starts_above <- mean(vapply(s_coords, function(s) y_mm(s$y), numeric(1))) > y_mm(e$y)
  
  if (starts_above) {
    bend_y <- e$top + d_min
    bend_y <- unit.pmax(bend_y, e$top + split_pad)
  } else {
    bend_y <- e$top - d_min
    bend_y <- unit.pmin(bend_y, e$top - split_pad)
  }
  
  return(bend_y)
}

# Function to calculate X positions on top of the end box
pcCalculateXPositions <- function(starts, end, margin = unit(2, "mm")) {
  e <- coords(end)
  n <- length(starts)
  xs_end <- prEdgeSlots(e$left, e$right, n = n, margin = margin)
  return(xs_end)
}

prGenerateLines <- function(starts, end, bend_y, xs_end, lty_gp, arrow_obj, subelmnt) {
  s_coords <- lapply(starts, coords)
  e <- coords(end)
  
  sub_prefix <- if (missing(subelmnt)) "" else paste0(match.arg(subelmnt, c("right", "left")), "_")
  x_at <- function(pos, side = c("left", "right", "x")) {
    side <- match.arg(side)
    if (side == "x" && !is.null(pos[[paste0(sub_prefix, side)]])) return(pos[[paste0(sub_prefix, side)]])
    pos[[side]]
  }
  
  x_mm <- function(u) convertX(u, unitTo = "mm", valueOnly = TRUE)
  y_mm <- function(u) convertY(u, unitTo = "mm", valueOnly = TRUE)
  
  starts_above <- mean(vapply(s_coords, function(s) y_mm(s$y), numeric(1))) > y_mm(e$y)
  
  mk <- function(x, y, arrow = NULL) {
    line <- list(x = x, y = y)
    g <- grid::linesGrob(x = x, y = y, gp = lty_gp, arrow = arrow)
    structure(g, line = line, class = c("connect_boxes", class(g)))
  }
  
  # 1) stems: each start -> bend_y (no arrow)
  stem_grobs <- lapply(seq_along(s_coords), function(i) {
    s <- s_coords[[i]]
    x0 <- x_at(s, "x")
    y0 <- if (starts_above) s$bottom else s$top
    
    mk(
      x = unit.c(x0, x0),
      y = unit.c(y0, bend_y),
      arrow = NULL
    )
  })
  
  # 2) shared horizontal bus (span starts AND end slots)
  xs_bus_mm <- c(
    vapply(s_coords, function(s) x_mm(x_at(s, "x")), numeric(1)),
    vapply(seq_along(xs_end), function(i) x_mm(xs_end[i]), numeric(1))
  )
  bus_left  <- unit(min(xs_bus_mm), "mm")
  bus_right <- unit(max(xs_bus_mm), "mm")
  
  bus_grob <- mk(
    x = unit.c(bus_left, bus_right),
    y = unit.c(bend_y, bend_y),
    arrow = NULL
  )
  
  # 3) trunk: bus -> end$top (arrow). Use end center by default.
  trunk_x <- e$x
  trunk_grob <- mk(
    x = unit.c(trunk_x, trunk_x),
    y = unit.c(bend_y, e$top + unit(0.5, "mm")),
    arrow = arrow_obj
  )
  
  structure(c(stem_grobs, list(bus_grob, trunk_grob)),
            class = c("connect_boxes_list", "list"))
}


# Refactored main function
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
