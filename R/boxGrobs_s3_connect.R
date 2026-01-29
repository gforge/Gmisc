#' Connect boxes (S3)
#'
#' A convenient way to connect boxes in a `Gmisc_list_of_boxes` or
#' simple `list` context, designed for piping (`|>`).
#'
#' @param x A `list` of boxes (will be converted to `Gmisc_list_of_boxes` if needed).
#' @param from The name (string) or index of the start box in `x`. Multiple values allowed.
#' @param to The name (string) or index of the end box in `x`. Multiple values allowed.
#' @param ... Arguments passed on to [`connectGrob`].
#'
#' @return The original list `x` (upgraded to `Gmisc_list_of_boxes`) with a new
#'   connection appended to its `"connections"` attribute. When printed, these
#'   connections are drawn.
#'
#' @seealso [`connectGrob`]
#' @export
#' @family flowchart components
connect <- function(x, ...) UseMethod("connect")

#' @export
#' @rdname connect
connect.default <- function(x, ...) {
  # If it's a list (but not yet class-extended), treat it as a box list
  if (is.list(x) && !inherits(x, "box")) {
    return(connect(prConvertListToBoxList(x), ...))
  }
  stop("connect() requires a list of boxes (Gmisc_list_of_boxes).")
}

#' @export
#' @rdname connect
connect.Gmisc_list_of_boxes <- function(x, from = NULL, to = NULL, ...) {
  args <- list(...)

  if (is.null(from) || is.null(to)) {
    stop("You must provide both 'from' and 'to' arguments.")
  }

  # Helper to resolve box objects from x using names/indices
  resolve_box <- function(ref) {
    if (is.character(ref)) {
      if (all(ref %in% names(x))) {
        return(x[ref])
      }

      resolved <- lapply(ref, function(r) {
        if (r %in% names(x)) {
          return(x[[r]])
        }

        if (grepl("$", r, fixed = TRUE)) {
          path <- strsplit(r, "$", fixed = TRUE)[[1]]
          el <- get_list_element_by_path(x, path)
          if (!is.null(el)) {
            return(el)
          }
        }

        stop("Could not find box named: ", r)
      })
      return(resolved)
    } else if (is.numeric(ref)) {
      return(x[ref])
    } else if (inherits(ref, "box") || (is.list(ref) && all(vapply(ref, inherits, logical(1), "box")))) {
      # Use directly if passed as object
      return(ref)
    }
    stop("Invalid from/to selector: must be name, index, or box object.")
  }

  start_boxes <- resolve_box(from)
  end_boxes <- resolve_box(to)

  if (length(start_boxes) == 1) start_boxes <- start_boxes[[1]]
  if (length(end_boxes) == 1) end_boxes <- end_boxes[[1]]

  # Create the connection grob
  # Filter args to remove legacy ones if any remain
  call_args <- c(list(start = start_boxes, end = end_boxes), args)
  # Note: args might still contain .from/.to if we didn't remove them from list(...)
  # But we constructed 'args' from list(...) and removed them.
  # Wait, 'args' is a local variable. '...' is passed to connectGrob via do.call?
  # No, I should use do.call with cleaned args.

  cg <- do.call(connectGrob, call_args)

  # Append to attributes
  current_conns <- attr(x, "connections")
  if (is.null(current_conns)) current_conns <- list()

  current_conns <- c(current_conns, list(cg))

  attr(x, "connections") <- current_conns
  x
}
