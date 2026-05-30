#' Move boxes (S3)
#'
#' Move a box or list of boxes. A wrapper for [`moveBox`] that supports
#' piping of `Gmisc_list_of_boxes` while preserving attributes.
#'
#' @param element A `list` of boxes.
#' @param ... Arguments passed to [`moveBox`].
#'
#' @return The moved list of boxes.
#' @export
#' @family flowchart components
move <- function(element, ...) UseMethod("move")

#' @export
#' @rdname move
move.default <- function(element, ...) {
  moveBox(element, ...)
}

#' @export
#' @rdname move
#' @param subelement The name of the element to move (maps to `subelement` in `moveBox`)
move.Gmisc_list_of_boxes <- function(element, subelement = NULL, ...) {
  # Capture attributes
  attrs <- attributes(element)

  args <- list(...)
  # Backward compatibility for 'name' if used (though it was just added in dev)
  if ("name" %in% names(args)) {
    # warning("Argument 'name' is deprecated, please use 'subelement' instead.")
    if (is.null(subelement)) subelement <- args$name
    args$name <- NULL
  }

  if (!is.null(subelement)) {
    args$subelement <- subelement
  }

  ret <- do.call(moveBox, c(list(element = element), args))

  # Restore attributes (specifically connections)
  # We only restore if they are missing in the result
  if (is.null(attr(ret, "connections")) && !is.null(attrs$connections)) {
    attr(ret, "connections") <- attrs$connections
  }

  prExtendClass(ret, "Gmisc_list_of_boxes")
}
