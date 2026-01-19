prConvertListToBoxList <- function(x) {
  if (inherits(x, "box")) {
    x <- list(x)
  }

  if (!is.list(x)) {
    stop("Input must be a list of boxes.")
  }

  # Validate recursively
  validate_structure <- function(el, path_prefix = "") {
    if (inherits(el, "box")) {
      return(NULL)
    }

    if (is.list(el)) {
      invalid_found <- lapply(seq_along(el), function(i) {
        nm <- names(el)[i]
        current_label <- if (!is.null(nm) && nzchar(nm)) nm else i

        # Construct path for error message
        current_path <- if (nzchar(path_prefix)) {
          paste0(path_prefix, "$", current_label)
        } else {
          as.character(current_label)
        }

        validate_structure(el[[i]], current_path)
      })
      return(unlist(invalid_found))
    }

    # If not a box and not a list, it is invalid. Return the path to this element.
    return(path_prefix)
  }

  invalid_elements <- validate_structure(x)

  if (length(invalid_elements) > 0) {
    stop(
      "The following elements are not valid boxes or lists of boxes: ",
      paste(invalid_elements, collapse = ", ")
    )
  }

  if (!inherits(x, "Gmisc_list_of_boxes")) {
    x <- prExtendClass(x, "Gmisc_list_of_boxes")
  }
  x
}
