prConnect1 <- function(
    start,
    end,
    type,
    subelmnt,
    lty_gp,
    arrow_obj
) {
  assert_class(start, "box")
  assert_class(end, "box")
  assert_class(lty_gp, "gpar")
  assert_class(arrow_obj, "arrow")
  
  start <- coords(start)
  end <- coords(end)
  
  if (missing(subelmnt)) {
    subelmnt <- ""
  } else {
    subelmnt <- sprintf("%s_", match.arg(subelmnt, c("right", "left")))
  }
  
  getX4elmnt <- function(elmnt, side = c("left", "right", "x")) {
    side <- match.arg(side)
    if (side == "x" && !is.null(elmnt[[sprintf("%s%s", subelmnt, side)]])) {
      return(elmnt[[sprintf("%s%s", subelmnt, side)]])
    }
    elmnt[[side]]
  }
  
  # FIX: x comparisons must use convertWidth, y comparisons use convertHeight
  x_mm <- function(u) convertWidth(u, unitTo = "mm", valueOnly = TRUE)
  y_mm <- function(u) convertHeight(u, unitTo = "mm", valueOnly = TRUE)
  
  line <- list()
  
  if (type %in% c("L", "-")) {
    if (type == "-") {
      line$y <- unit.c(end$y, end$y, end$y)
    } else {
      line$y <- unit.c(start$bottom, end$y, end$y)
    }
    if (x_mm(getX4elmnt(start, "x")) < x_mm(getX4elmnt(end, "x"))) {
      line$x <- unit.c(getX4elmnt(start, "x"), getX4elmnt(start, "x"), end$left)
    } else {
      line$x <- unit.c(getX4elmnt(start, "x"), getX4elmnt(start, "x"), end$right)
    }
    
  } else if (type == "Z") {
    if (x_mm(start$x) < x_mm(end$x)) {
      line$x <- unit.c(
        start$right,
        start$right + distance(start, end, type = "h", half = TRUE),
        start$right + distance(start, end, type = "h", half = TRUE),
        end$left
      )
    } else {
      line$x <- unit.c(
        start$left,
        start$left - distance(start, end, type = "h", half = TRUE),
        start$left - distance(start, end, type = "h", half = TRUE),
        end$right
      )
    }
    line$y <- unit.c(start$y, start$y, end$y, end$y)
    
  } else if (type == "N") {
    dist_y <- distance(start, end, type = "v", half = TRUE)
    if (y_mm(start$y) < y_mm(end$y)) {
      line$y <- unit.c(start$top, start$top + dist_y, start$top + dist_y, end$bottom)
    } else {
      line$y <- unit.c(start$bottom, start$bottom - dist_y, start$bottom - dist_y, end$top)
    }
    line$x <- unit.c(
      getX4elmnt(start, "x"),
      getX4elmnt(start, "x"),
      getX4elmnt(end, "x"),
      getX4elmnt(end, "x")
    )
    
  } else if (type == "vertical") {
    line$x <- unit.c(getX4elmnt(start, "x"), getX4elmnt(end, "x"))
    if (y_mm(start$y) < y_mm(end$y)) {
      line$y <- unit.c(start$top, end$bottom)
    } else {
      line$y <- unit.c(start$bottom, end$top)
    }
    
  } else { # horizontal
    line$y <- unit.c(start$y, end$y)
    if (x_mm(getX4elmnt(start, "x")) < x_mm(getX4elmnt(end, "x"))) {
      line$x <- unit.c(start$right, end$left)
    } else {
      line$x <- unit.c(start$left, end$right)
    }
  }
  
  lg <- linesGrob(x = line$x, y = line$y, gp = lty_gp, arrow = arrow_obj)
  structure(lg, line = line, class = c("connect_boxes", class(lg)))
}
