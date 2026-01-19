#' Create a flowchart object
#'
#' This function initializes a flowchart object, which is essentially a list of boxes
#' with the class `Gmisc_list_of_boxes`. It serves as the starting point for the
#' S3 pipeline, allowing for fluent chaining of [`align`], [`spread`], [`move`],
#' and [`connect`] operations.
#'
#' @param ... Either a set of [`boxGrob`] objects, text strings, or lists of these.
#'  Text strings will be automatically wrapped in [`boxGrob`] (or `default_box_fn`).
#' @param default_box_fn The function to use for converting text/content into a box
#'  (default: [`boxGrob`]).
#'
#' @return A `list` of class `Gmisc_list_of_boxes`.
#' @family flowchart components
#' @export
#' @examples
#' library(grid)
#' grid.newpage()
#'
#' # Create a flowchart with auto-conversion of text
#' fc <- flowchart(
#'   start = "Start",
#'   process = "Process",
#'   end = "End"
#' )
#'
#' # Use the pipeline
#' fc |>
#'   align(axis = "y") |>
#'   connect("start", "process", type = "vertical") |>
#'   connect("process", "end", type = "vertical") |>
#'   print()
flowchart <- function(..., default_box_fn = boxGrob) {
  args <- list(...)

  # If a single unnamed argument is passed and it's a list (but not a box),
  # treat it as the list itself rather than wrapping it
  if (length(args) == 1 &&
    is.list(args[[1]]) &&
    !inherits(args[[1]], "box") &&
    is.null(names(args))) {
    args <- args[[1]]
  }

  process_element <- function(el) {
    if (inherits(el, "box")) {
      return(el)
    }

    if (is.list(el)) {
      return(lapply(el, process_element))
    }

    # Assume content for a new box
    # Convert to box using default function
    default_box_fn(el)
  }

  converted_args <- lapply(args, process_element)
  prConvertListToBoxList(converted_args)
}
