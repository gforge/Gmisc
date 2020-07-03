#' Connect boxes with an arrow
#'
#' The function creates a grob that links two boxes together. It looks for
#' which side it should attach the arrow, e.g. if the start is on top of
#' the bottom it should attach to the bottom edge of ther start box and then
#' to the top at the end.
#'
#' The exact positions of the line is stored at the \code{attr(..., "line")}.
#' If you want to draw your own custom line all you need to do is check which
#' \code{attr(my_line, "line")$x} and \code{attr(my_line, "line")$y} you want
#' to attach to and then create your own custom \code{\link[grid:grid.lines]{linesGrob}}.
#'
#' @param start The start box
#' @param end The end box
#' @param type How the boxes are stacked. The \code{L} alternative generates a
#'  straight line up/down and then turns to righT/left for connecting with the end.
#'  The \code{-} generates a straight horizontal arrow. The \code{Z} creates a
#'  horizontal line that looks like a \code{Z} with 90 degree turns. The option
#'  \code{N} allows for vertical lines.
#' @param subelmnt If we have a split box we can specify the right/left x as the
#'  connector point.
#' @param lty_gp The \code{\link[grid]{gpar}} for the line. Set
#' \code{connectGrob} option if you want to customize all the arrows at once.
#' @param arrow_obj The arrow spec according to \code{\link[grid]{arrow}}. Set
#' \code{connectGrobArrow} option if you want to customize all the arrows at once.
#'
#' @return grob with an arrow
#' @export
#'
#' @importFrom checkmate assert_class
#' @family flowchart components
#' @rdname connect
#' @example inst/examples/connectGrob_example.R
connectGrob <- function(
                        start,
                        end,
                        type = c("vertical", "horizontal", "L", "-", "Z", "N"),
                        subelmnt = c("right", "left"),
                        lty_gp = getOption("connectGrob",
                          default = gpar(fill = "black")
                        ),
                        arrow_obj = getOption("connectGrobArrow",
                          default = arrow(ends = "last", type = "closed")
                        )) {
  assert_class(start, "box")
  assert_class(end, "box")
  assert_class(lty_gp, "gpar")
  assert_class(arrow_obj, "arrow")

  # We use the coordinates provided with the boxes
  start <- coords(start)
  end <- coords(end)

  type <- match.arg(type)
  if (missing(subelmnt)) {
    subelmnt <- ""
  } else {
    subelmnt <- sprintf("%s_", match.arg(subelmnt))
  }
  getX4elmnt <- function(elmnt, side = c("left", "right", "x")) {
    side <- match.arg(side)
    if (side == "x" && !is.null(elmnt[[sprintf("%s%s", subelmnt, side)]])) {
      return(elmnt[[sprintf("%s%s", subelmnt, side)]])
    } else {
      return(elmnt[[side]])
    }
  }
  line <- list()
  cnvrt <- function(val) {
    convertHeight(val, unitTo = "mm", valueOnly = TRUE)
  }
  if (type %in% c("L", "-")) {
    if (type == "-") {
      line$y <- unit.c(end$y, end$y, end$y)
    } else {
      line$y <- unit.c(start$bottom, end$y, end$y)
    }
    if (cnvrt(getX4elmnt(start, "x")) < cnvrt(getX4elmnt(end, "x"))) {
      line$x <- unit.c(getX4elmnt(start, "x"), getX4elmnt(start, "x"), end$left)
    } else {
      line$x <- unit.c(getX4elmnt(start, "x"), getX4elmnt(start, "x"), end$right)
    }
  } else if (type == "Z") {
    if (prCnvrtX(start$x) < prCnvrtX(end$x)) {
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

    line$y <- unit.c(
      start$y,
      start$y,
      end$y,
      end$y
    )
  } else if (type == "N") {
    dist_y <- distance(start, end, type = "v", half = TRUE)
    if (prCnvrtY(start$y) < prCnvrtY(end$y)) {
      line$y <- unit.c(
        start$top,
        start$top + dist_y,
        start$top + dist_y,
        end$bottom
      )
    } else {
      line$y <- unit.c(
        start$bottom,
        start$bottom - dist_y,
        start$bottom - dist_y,
        end$top
      )
    }

    line$x <- unit.c(
      getX4elmnt(start, "x"),
      getX4elmnt(start, "x"),
      getX4elmnt(end, "x"),
      getX4elmnt(end, "x")
    )
  } else if (type == "vertical") {
    line$x <- unit.c(getX4elmnt(start, "x"), getX4elmnt(end, "x"))
    if (cnvrt(start$y) < cnvrt(end$y)) {
      line$y <- unit.c(start$top, end$bottom)
    } else {
      line$y <- unit.c(start$bottom, end$top)
    }
  } else {
    line$y <- unit.c(start$y, end$y)
    if (cnvrt(getX4elmnt(start, "x")) < cnvrt(getX4elmnt(end, "x"))) {
      line$x <- unit.c(start$right, end$left)
    } else {
      line$x <- unit.c(start$left, end$right)
    }
  }

  lg <- linesGrob(
    x = line$x,
    y = line$y,
    gp = lty_gp,
    arrow = arrow_obj
  )
  structure(lg,
    line = line,
    class = c("connect_boxes", class(lg))
  )
}

#' The print/plot calls the \code{\link[grid]{grid.draw}} function on the object
#' @param x The grob to print/plot
#' @param ... Passed to \code{\link[grid]{grid.draw}}
#' @rdname connect
#' @export
print.connect_boxes <- function(x, ...) {
  grid.draw(x, ...)
}

#' @rdname connect
#' @export
plot.connect_boxes <- print.connect_boxes
