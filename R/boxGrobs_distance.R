
#' Get the distance between grid objects
#'
#' Retrieves the distance between two boxes as absolute \code{"mm"} units. The function also
#' accepts \code{\link{coords}} objects as well as a \code{\link[grid]{unit}} or a numeric input.
#'
#' @param box1 The first \code{\link{boxGrob}}. Can also be a \code{\link{coords}}
#'  object, a \code{\link[grid]{unit}} or a numeric. The latter is evaluated to
#'  a \code{unit} with \code{units="npc"}.
#' @param box2 The second object to calculate the distance to. Same type as for \code{box1}.
#' @param type Whether we should retrieve the vertical, horizontal or euclidean distance
#' @param half If set to true it returns half the distance. This is convenient
#'  when positioning boxes between each other.
#' @param center Calculate the distance from the center of each object
#' @return A \code{unit} in \code{"mm"} with an absolute value. The attribute
#'  \code{positive} indicates the direction of the value, i.e. if it is \code{TRUE} the
#'  distance was calculated from the first to the second, otherwise it is \code{FALSE}.
#'  For \code{euclidean} distance the \code{positive} attribute is \code{NA}. There is also the
#'  \code{from} and \code{to} attributes that has the coordinates that were used for the
#'  calculations, for \code{euclidean} distance this is \code{NA}.
#'
#' @family flowchart components
#' @importFrom checkmate assert_class assert checkNumeric checkClass checkTRUE
#' @importFrom grid is.unit
#' @export
#' @examples
#' box1 <- boxGrob("A test box", y = .8)
#' box2 <- boxGrob("Another test box", y = .2)
#' distance(box1, box2, "v")
#' @rdname distance
distance <- function(box1,
                     box2,
                     type = c("vertical", "horizontal", "euclidean"),
                     half = FALSE,
                     center = FALSE) {
  assert_input <- function(v) {
    assert(
      checkClass(v, "box"),
      checkClass(v, "coords"),
      checkNumeric(v),
      checkTRUE(is.unit(v))
    )
  }


  type <- match.arg(type)
  if (missing(box2) && is.list(box1) && length(box1) == 2) {
    box2 <- box1[[2]]
    box1 <- box1[[1]]
  }
  assert_input(box1)
  assert_input(box2)
  box_coords1 <- prConvert2Coords(box1)
  box_coords2 <- prConvert2Coords(box2)

  type = match.arg(type)
  converter_fn <- ifelse(type == "horizontal", prCnvrtX, prCnvrtY)
  from = NA
  to = NA
  if (type == "vertical") {
    if (converter_fn(box_coords1$y) > converter_fn(box_coords2$y)) {
      if (center) {
        from <- box_coords1$y
        to <- box_coords2$y
      } else {
        from <- box_coords1$bottom
        to <- box_coords2$top
      }
    } else {
      if (center) {
        from <- box_coords1$y
        to <- box_coords2$y
      } else {
        from <- box_coords1$top
        to <- box_coords2$bottom
      }
    }
    ret <- converter_fn(to) - converter_fn(from)
  } else if (type == "horizontal") {
    if (prCnvrtX(box_coords1$x) < prCnvrtX(box_coords2$x)) {
      if (center) {
        from <- box_coords1$x
        to <- box_coords2$x
      } else {
        from <- box_coords1$right
        to <- box_coords2$left
      }
    } else {
      if (center) {
        from <- box_coords1$x
        to <- box_coords2$x
      } else {
        from <- box_coords1$left
        to <- box_coords2$right
      }
    }
    ret <- converter_fn(to) - converter_fn(from)
  } else if (type == "euclidean") {
    ydist <- distance(box1 = box1, box2 = box2, type = "vertical", center = center)
    xdist <- distance(box1 = box1, box2 = box2, type = "horizontal", center = center)
    ret <- sqrt(prCnvrtY(ydist)^2 + prCnvrtX(xdist)^2)
  } else {
    stop("Unreachable code")
  }

  if (ret < 0) {
    positive <- FALSE
    ret <- -1 * ret
  } else {
    positive <- TRUE
  }

  if (half) {
    ret <- ret / 2
  }

  ret <- unit(ret, "mm")
  structure(
    ret,
    class = c("Gmisc_unit", class(ret)),
    positive = positive,
    from = from,
    to = to,
    type = type,
    box_coords1 = box_coords1,
    box_coords2 = box_coords2,
    center = center)
}

#' @rdname distance
#' @param x A unit with from the \code{distance} function
#' @param ... Passed on to print
#' @export
print.Gmisc_unit <- function(x, ...) {
  base_txt <- as.character(x)
  repr <- paste(
    base_txt,
    paste0(" - positive = ", as.character(attr(x, "positive"))),
    paste0(" - from ", as.character(attr(x, "from"))),
    paste0(" - to   ", as.character(attr(x, "to"))),
    paste0(" - type: ", as.character(attr(x, "type"))),
    paste0(" - center: ", as.character(attr(x, "center"))),
    "",
    sep = "\n")
  cat(repr)
  invisible(x)
}
