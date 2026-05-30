#' Align boxes (S3)
#'
#' Align a list of boxes vertically or horizontally. Designed for piping (`|>`).
#'
#' @param x A `list` of boxes.
#' @param ... Arguments passed to [`alignVertical`] or [`alignHorizontal`].
#' @param axis Orientation: `"y"`/`"vertical"` or `"x"`/`"horizontal"`.
#'
#' @return The aligned list of boxes (class `Gmisc_list_of_boxes`).
#' @export
#' @family flowchart components
align <- function(x, ...) {
  UseMethod("align")
}

#' @export
#' @rdname align
align.default <- function(x, ...) {
  if (is.list(x) && !inherits(x, "box")) {
    return(align(prConvertListToBoxList(x), ...))
  }
  stop("align() requires a list of boxes.")
}

#' @export
#' @rdname align
align.list <- function(x, ...) {
  align.default(x, ...)
}

#' @export
#' @rdname align
align.Gmisc_list_of_boxes <- function(x, ..., axis = c("y", "x", "vertical", "horizontal")) {
  axis <- match.arg(axis)

  if (axis %in% c("y", "vertical")) {
    return(alignVertical(x, ...))
  } else {
    return(alignHorizontal(x, ...))
  }
}
