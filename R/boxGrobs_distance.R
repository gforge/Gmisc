
#' Get the distance between boxes
#'
#' Retrieves the distance between two boxes as absolute \code{"mm"} units.
#'
#' @param box1 The first boxGrob
#' @param box2 The second boxGrob
#' @param type Wheter we should retrieve the vertical or horizontal difference
#' @param half If set to true it returns half the distance. This is convenient
#'  when postioning boxes between eachother.
#' @return a unit with \code{"mm"}
#'
#' @importFrom checkmate assert_class assert checkString checkNumeric checkClass
#' @export
#' @examples
#' box1 <- boxGrob("A test box", y = .8)
#' box2 <- boxGrob("Another test box", y = .2)
#' distance(box1, box2, "v")
distance <- function(box1, box2, type = c("vertical", "horizontal"), half = FALSE) {
  assert(
    checkClass(box1, "box"),
    checkClass(box1, "coords")
  )
  assert(
    checkClass(box2, "box"),
    checkClass(box2, "coords")
  )
  type <- match.arg(type)
  if (!inherits(box1, "coords")) {
    box1 <- coords(box1)
  }

  if (!inherits(box2, "coords")) {
    box2 <- coords(box2)
  }

  if (type == "vertical") {
    if (prCnvrtY(box1$y) > prCnvrtY(box2$y)) {
      ret <-
        (prCnvrtY(box1$bottom) - prCnvrtY(box2$top))
    } else {
      ret <-
        (prCnvrtY(box2$bottom) - prCnvrtY(box1$top))
    }
  } else {
    if (prCnvrtX(box1$x) < prCnvrtX(box2$x)) {
      ret <-
        (prCnvrtX(box2$left) - prCnvrtX(box1$right))
    } else {
      ret <-
        (prCnvrtX(box1$left) - prCnvrtX(box2$right))
    }
  }
  if (half) {
    ret <- ret / 2
  }

  return(unit(ret, "mm"))
}
