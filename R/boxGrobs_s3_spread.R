#' Spread boxes (S3)
#'
#' Spread a list of boxes vertically or horizontally. Designed for piping (`|>`).
#'
#' @param x A `list` of boxes.
#' @param ... Arguments passed to [`spreadVertical`] or [`spreadHorizontal`].
#' @param axis Orientation: `"y"`/`"vertical"` or `"x"`/`"horizontal"`.
#'
#' @return The list of boxes with updated positions (class `Gmisc_list_of_boxes`).
#' @export
#' @family flowchart components
spread <- function(x, ...) {
  UseMethod("spread")
}

#' @export
#' @rdname spread
spread.default <- function(x, ...) {
  if (is.list(x) && !inherits(x, "box")) {
    return(spread(prConvertListToBoxList(x), ...))
  }
  stop("spread() requires a list of boxes.")
}

#' @export
#' @rdname spread
spread.list <- function(x, ...) {
  spread.default(x, ...)
}

#' @export
#' @rdname spread
spread.Gmisc_list_of_boxes <- function(x, ..., axis = c("y", "x", "vertical", "horizontal")) {
  axis <- match.arg(axis)

  args <- list(...)
  call_args <- c(x, args)

  if (axis %in% c("y", "vertical")) {
    return(do.call(spreadVertical, call_args))
  } else {
    return(do.call(spreadHorizontal, call_args))
  }
}
