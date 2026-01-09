prLineMidpoint <- function(line) {
  stopifnot(is.list(line), inherits(line$x, "unit"), inherits(line$y, "unit"))
  stopifnot(length(line$x) == length(line$y), length(line$x) >= 2)
  
  x <- convertWidth(line$x,  "mm", valueOnly = TRUE)
  y <- convertHeight(line$y, "mm", valueOnly = TRUE)
  
  dx <- diff(x)
  dy <- diff(y)
  seg <- sqrt(dx^2 + dy^2)
  total <- sum(seg)
  
  if (!is.finite(total) || total <= 0) {
    # Fallback: first point
    return(list(x = unit(x[1], "mm"), y = unit(y[1], "mm")))
  }
  
  half <- total / 2
  cs <- c(0, cumsum(seg))
  i <- max(which(cs <= half))
  
  if (i >= length(seg)) {
    return(list(x = unit(x[length(x)], "mm"), y = unit(y[length(y)], "mm")))
  }
  
  t <- (half - cs[i]) / seg[i]
  xm <- x[i] + t * (x[i + 1] - x[i])
  ym <- y[i] + t * (y[i + 1] - y[i])
  
  list(x = unit(xm, "mm"), y = unit(ym, "mm"))
}

prLineLabelPos <- function(line, prefer = c("auto", "horizontal", "longest")) {
  prefer <- match.arg(prefer)
  stopifnot(is.list(line), inherits(line$x, "unit"), inherits(line$y, "unit"))
  
  xs <- convertX(line$x, unitTo = "npc", valueOnly = TRUE)
  ys <- convertY(line$y, unitTo = "npc", valueOnly = TRUE)
  
  if (length(xs) != length(ys) || length(xs) < 2) {
    stop("Invalid line geometry for label placement", call. = FALSE)
  }
  
  # segment i is between i and i+1
  dx <- abs(diff(xs))
  dy <- abs(diff(ys))
  
  is_h <- dy < 1e-12
  is_v <- dx < 1e-12
  
  pick <- function() {
    if (prefer %in% c("auto", "horizontal") && any(is_h)) {
      # if there are multiple horizontals: pick the longest (in npc)
      return(which.max(dx * is_h))
    }
    if (prefer %in% c("auto", "longest")) {
      return(which.max(dx + dy))
    }
    # fallback: first segment
    1L
  }
  
  i <- pick()
  x_mid <- (xs[i] + xs[i + 1]) / 2
  y_mid <- (ys[i] + ys[i + 1]) / 2
  
  list(x = unit(x_mid, "npc"), y = unit(y_mid, "npc"))
}

prLabelWithBackground <- function(label, x, y,
                                  label_gp = gpar(cex = 1),
                                  pad = unit(1.5, "mm"),
                                  bg_gp = gpar(fill = "white", col = NA)) {
  tg <- textGrob(label, x = x, y = y, just = "center", gp = label_gp)
  
  w <- grobWidth(tg) + 2 * pad
  h <- grobHeight(tg) + 2 * pad
  
  bg <- rectGrob(x = x, y = y, width = w, height = h, just = "center", gp = bg_gp)
  
  grobTree(bg, tg)
}


prConnect1 <- function(
    start,
    end,
    type,
    subelmnt,
    lty_gp,
    arrow_obj,
    label = NULL,
    label_gp = grid::gpar(cex = 0.9),
    label_pos = c("mid", "near_start", "near_end"),
    label_offset = unit(2, "mm"),
    label_pad = unit(1.5, "mm")
) {
  assert_class(start, "box")
  assert_class(end, "box")
  assert_class(lty_gp, "gpar")
  assert_class(arrow_obj, "arrow")
  if (!is.null(label)) assert_class(label_gp, "gpar")
  
  label_pos <- match.arg(label_pos)
  
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
  
  lg <- grid::linesGrob(x = line$x, y = line$y, gp = lty_gp, arrow = arrow_obj)

  
  if (!is.null(label) && nzchar(label)) {
    mid <- prLineLabelPos(line, prefer = "auto")
    lbl <- prLabelWithBackground(
      label = label,
      x = mid$x,
      y = mid$y,
      label_gp = label_gp,
      pad = label_pad,
      bg_gp = gpar(fill = "white", col = NA)
    )
    
    gt <- grobTree(lg, lbl)
    
    return(structure(
      gt,
      line = line,
      class = c("connect_boxes", class(gt))
    ))
  }
  
  structure(lg, line = line, class = c("connect_boxes", class(lg)))
}
