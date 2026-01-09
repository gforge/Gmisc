prConnectOneToMany <- function(
    start,
    ends,
    type,
    subelmnt,
    lty_gp,
    arrow_obj,
    split_pad = unit(2, "mm")  # only used for shared-bend types
) {
  assert_class(start, "box")
  if (is.numeric(split_pad)) split_pad <- unit(split_pad, "mm")
  assert_class(split_pad, "unit")
  
  # Default: just call the single connector for each end
  if (type != "N") {
    grobs <- lapply(ends, function(e) prConnect1(start, e, type, subelmnt, lty_gp, arrow_obj))
    return(structure(grobs, class = c("connect_boxes_list", "list")))
  }
  
  # Special: shared bend height for N (use midpoint of the shortest edge gap)
  s <- coords(start)
  end_coords <- lapply(ends, coords)
  
  y_mm <- function(u) convertHeight(u, "mm", valueOnly = TRUE)
  
  s_y <- y_mm(s$y)
  e_y <- vapply(end_coords, function(e) y_mm(e$y), numeric(1))
  
  dir_down <- s_y > max(e_y)
  dir_up   <- s_y < min(e_y)
  if (!dir_down && !dir_up) dir_down <- mean(e_y) < s_y
  
  # Compute half-gaps (same metric as single N)
  if (dir_down) {
    d <- lapply(end_coords, function(e) distance(s, e, type = "v", half = TRUE))
    d_min <- Reduce(unit.pmin, d)
    
    # midpoint relative to the closest end
    bend_y <- s$bottom - d_min
    
    # optional clamp so we never end up "past" start/beyond ends if layout is odd
    bend_y <- unit.pmax(bend_y, unit(0, "npc"))
    bend_y <- unit.pmin(bend_y, s$bottom - split_pad)
    
    start_attach <- s$bottom
    end_attach <- function(e) e$top
  } else {
    d <- lapply(end_coords, function(e) distance(s, e, type = "v", half = TRUE))
    d_min <- Reduce(unit.pmin, d)
    
    bend_y <- s$top + d_min
    
    bend_y <- unit.pmin(bend_y, unit(1, "npc"))
    bend_y <- unit.pmax(bend_y, s$top + split_pad)
    
    start_attach <- s$top
    end_attach <- function(e) e$bottom
  }
  
  # subelement x picking (same semantics as single)
  if (missing(subelmnt)) {
    sub_prefix <- ""
  } else {
    sub_prefix <- sprintf("%s_", match.arg(subelmnt, c("right", "left")))
  }
  x_at <- function(pos, side = c("left", "right", "x")) {
    side <- match.arg(side)
    if (side == "x" && !is.null(pos[[paste0(sub_prefix, side)]])) return(pos[[paste0(sub_prefix, side)]])
    pos[[side]]
  }
  
  grobs <- lapply(end_coords, function(e) {
    line <- list(
      x = unit.c(x_at(s, "x"), x_at(s, "x"), x_at(e, "x"), x_at(e, "x")),
      y = unit.c(start_attach, bend_y, bend_y, end_attach(e))
    )
    lg <- linesGrob(x = line$x, y = line$y, gp = lty_gp, arrow = arrow_obj)
    structure(lg, line = line, class = c("connect_boxes", class(lg)))
  })
  
  structure(grobs, class = c("connect_boxes_list", "list"))
}
