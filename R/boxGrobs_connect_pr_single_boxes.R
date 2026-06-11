prLineMidpoint <- function(line) {
  stopifnot(is.list(line), inherits(line$x, "unit"), inherits(line$y, "unit"))
  stopifnot(length(line$x) == length(line$y), length(line$x) >= 2)

  x <- prConvertWidthToMm(line$x)
  y <- prConvertHeightToMm(line$y)

  dx <- diff(x)
  dy <- diff(y)
  seg <- sqrt(dx^2 + dy^2)
  total <- sum(seg)

  if (!is.finite(total) || total <= 0) {
    # Fallback: first point (return as npc units)
    return(list(x = prConvertMmToNpc(x[1], "x"), y = prConvertMmToNpc(y[1], "y")))
  }

  half <- total / 2
  cs <- c(0, cumsum(seg))
  i <- max(which(cs <= half))

  # If the selected segment has zero or non-finite length, fall back to the
  # start point of that segment. Otherwise compute the fractional position
  # along the segment (works for single-segment lines as well).
  if (!is.finite(seg[i]) || seg[i] <= 0) {
    return(list(x = prConvertMmToNpc(x[i], "x"), y = prConvertMmToNpc(y[i], "y")))
  }

  t <- (half - cs[i]) / seg[i]
  xm <- x[i] + t * (x[i + 1] - x[i])
  ym <- y[i] + t * (y[i + 1] - y[i])

  list(x = prConvertMmToNpc(xm, "x"), y = prConvertMmToNpc(ym, "y"))
}

prLineLabelPos <- function(line, prefer = c("auto", "horizontal", "longest")) {
  prefer <- match.arg(prefer)
  stopifnot(is.list(line), inherits(line$x, "unit"), inherits(line$y, "unit"))

  xs <- vapply(line$x, \(u) prGetNpcValue(u, "x"), numeric(1))
  ys <- vapply(line$y, \(u) prGetNpcValue(u, "y"), numeric(1))

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
  smooth = FALSE,
  corner_radius = unit(3, "mm"),
  side = c("auto", "left", "right"),
  end_side = c("auto", "left", "right"),
  side_fan_in_route = c("outside", "edge"),
  side_fan_in_offset = unit(5, "mm"),
  label = NULL,
  label_gp = grid::gpar(cex = 0.9),
  label_bg_gp = grid::gpar(fill = "white", col = NA),
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
  side <- match.arg(side)
  end_side <- match.arg(end_side)
  side_fan_in_route <- match.arg(side_fan_in_route)

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

  line <- list(x = NULL, y = NULL)
  if (type %in% c("L", "-")) {
    if (type == "-") {
      line$y <- unit.c(end$y, end$y, end$y)
    } else {
      line$y <- unit.c(start$bottom, end$y, end$y)
    }
    start_x_mm    <- prConvertWidthToMm(getX4elmnt(start, "x"))
    end_x_mm      <- prConvertWidthToMm(getX4elmnt(end,   "x"))
    end_right_mm  <- prConvertWidthToMm(end$right)
    end_left_mm   <- prConvertWidthToMm(end$left)
    if (start_x_mm < end_x_mm) {
      # Start is to the left of end centre → approach from the left
      h_end <- end$left
      if (prConvertWidthToMm(h_end) <= start_x_mm) {
        # Overlap: end's left edge crosses start.x (box is wide).
        # Fall back to end centre so the horizontal segment still goes rightward.
        h_end <- getX4elmnt(end, "x")
      }
    } else if (end_right_mm < start_x_mm) {
      # Normal right-approach: end's right edge is clearly left of start → use it
      h_end <- end$right
    } else {
      # Overlap: end's right edge crosses start.x (box is wide).
      # Fall back to end centre so the horizontal segment still goes leftward.
      h_end <- getX4elmnt(end, "x")
    }
    line$x <- unit.c(getX4elmnt(start, "x"), getX4elmnt(start, "x"), h_end)
  } else if (type == "Z") {
    if (prConvertWidthToMm(start$x) < prConvertWidthToMm(end$x)) {
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
    if (prConvertHeightToMm(start$y) < prConvertHeightToMm(end$y)) {
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
    if (prConvertHeightToMm(start$y) < prConvertHeightToMm(end$y)) {
      line$y <- unit.c(start$top, end$bottom)
    } else {
      line$y <- unit.c(start$bottom, end$top)
    }
  } else if (type == "side") {
    # Horizontal-first exit: leave from a selected start side, travel vertically
    # outside/alongside the flow, then enter the selected side of the end box.
    end_is_right <- prConvertWidthToMm(getX4elmnt(end, "x")) > prConvertWidthToMm(start$x)
    exit_x <- if (side == "right" || (side == "auto" && end_is_right)) {
      start$right
    } else {
      start$left
    }

    start_is_left <- prConvertWidthToMm(start$x) < prConvertWidthToMm(getX4elmnt(end, "x"))
    entry_x <- if (end_side == "left" || (end_side == "auto" && start_is_left)) {
      end$left
    } else {
      end$right
    }
    line$x <- unit.c(exit_x, exit_x, entry_x)
    line$y <- unit.c(start$y, end$y, end$y)
  } else { # horizontal
    line$y <- unit.c(start$y, end$y)
    if (prConvertWidthToMm(getX4elmnt(start, "x")) < prConvertWidthToMm(getX4elmnt(end, "x"))) {
      line$x <- unit.c(start$right, end$left)
    } else {
      line$x <- unit.c(start$left, end$right)
    }
  }

  lg <- prRenderLine(
    x = line$x, y = line$y,
    smooth = smooth, corner_radius = corner_radius,
    gp = lty_gp, arrow = arrow_obj
  )


  if (!is.null(label) && nzchar(label)) {
    mid <- prLineLabelPos(line, prefer = "auto")
    lbl <- prLabelWithBackground(
      label = label,
      x = mid$x,
      y = mid$y,
      label_gp = label_gp,
      pad = label_pad,
      bg_gp = label_bg_gp
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
