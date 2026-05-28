#' Equalize box widths
#'
#' Sets selected boxes to a shared width. This is useful when aligning flowchart
#' levels so corresponding boxes have consistent visual width and center points.
#'
#' @param x A list of boxes (or nested lists of boxes). Can also be a single box.
#' @param subelement Optional target(s) inside `x`.
#'  Can be a single path (e.g. `c("groups", 1)`) or a list of paths
#'  (e.g. `list(c("groups", 1), c("groups2", 1))`).
#'  If `NULL`, all top-level boxes in `x` are used.
#' @param width Optional width to apply. If `NULL`, the maximum current width
#'  among selected boxes is used.
#'
#' @return The updated object with equalized widths.
#' @export
#' @family flowchart components
#' @examples
#' fc <- flowchart(
#'   groups = list("Group 1", "Group 2"),
#'   groups2 = list("Analysed", "Analysed")
#' )
#'
#' fc |>
#'   equalizeWidths(subelement = list(c("groups", 1),
#'                                    c("groups2", 1),
#'                                    c("groups", 2),
#'                                    c("groups2", 2)))
#'
#' # Global fixed width via explicit argument
#' fc |> equalizeWidths(subelement = "groups", width = grid::unit(30, "mm"))
#' @md
equalizeWidths <- function(x, subelement = NULL, width = NULL) {
  if (inherits(x, "box")) {
    if (is.null(width)) {
      return(x)
    }
    return(prSetBoxDimensions(x, width = width))
  }

  if (is.list(x) && !inherits(x, "Gmisc_list_of_boxes")) {
    x <- prConvertListToBoxList(x)
  }

  if (!inherits(x, "Gmisc_list_of_boxes")) {
    stop("equalizeWidths() requires a box or list of boxes.", call. = FALSE)
  }

  if (!is.null(width) && is.numeric(width)) {
    width <- unit(width, "mm")
  }
  if (!is.null(width) && !inherits(width, "unit")) {
    stop("`width` must be a unit or numeric.", call. = FALSE)
  }

  resolve_paths <- function(obj, subelement) {
    if (is.null(subelement)) {
      idx <- which(vapply(obj, inherits, logical(1), "box"))
      return(lapply(idx, as.list))
    }
    if (is.list(subelement) && all(vapply(subelement, is.atomic, logical(1)))) {
      return(subelement)
    }
    list(subelement)
  }

  fetch_target <- function(obj, path) {
    target <- get_list_element_by_path(obj, path)
    if (!is.null(target)) {
      return(list(target = target, first_container = FALSE))
    }

    if (length(obj) > 0 && is.list(obj[[1]]) && !inherits(obj[[1]], "box")) {
      target2 <- get_list_element_by_path(obj[[1]], path)
      if (!is.null(target2)) {
        return(list(target = target2, first_container = TRUE))
      }
    }

    list(target = NULL, first_container = FALSE)
  }

  paths <- resolve_paths(x, subelement)
  if (length(paths) == 0) {
    return(x)
  }

  selected <- list()
  selected_meta <- list()

  for (i in seq_along(paths)) {
    res <- fetch_target(x, paths[[i]])
    if (is.null(res$target)) {
      stop("The subelement '", paste(paths[[i]], collapse = " -> "), "' was not found in the provided boxes.", call. = FALSE)
    }
    if (inherits(res$target, "box")) {
      selected[[length(selected) + 1]] <- res$target
      selected_meta[[length(selected_meta) + 1]] <- list(
        path = paths[[i]],
        first_container = res$first_container
      )
      next
    }

    if (prIsBoxList(res$target)) {
      for (j in seq_along(res$target)) {
        selected[[length(selected) + 1]] <- res$target[[j]]
        selected_meta[[length(selected_meta) + 1]] <- list(
          path = c(paths[[i]], j),
          first_container = res$first_container
        )
      }
      next
    }

    stop("The subelement '", paste(paths[[i]], collapse = " -> "), "' is not a box or a list of boxes.", call. = FALSE)
  }

  target_width <- width
  if (is.null(target_width)) {
    widths_mm <- vapply(selected, function(b) {
      prConvertWidthToMm(coords(b)$width)
    }, numeric(1))
    target_width <- unit(max(widths_mm), "mm")
  }

  for (i in seq_along(selected)) {
    updated <- prSetBoxDimensions(selected[[i]], width = target_width)
    if (isTRUE(selected_meta[[i]]$first_container)) {
      x[[1]] <- set_list_element_by_path(x[[1]], selected_meta[[i]]$path, updated)
    } else {
      x <- set_list_element_by_path(x, selected_meta[[i]]$path, updated)
    }
  }

  prExtendClass(x, "Gmisc_list_of_boxes")
}

prSetBoxDimensions <- function(element, width = NULL, height = NULL) {
  assert_class(element, "box")

  if (is.null(width) && is.null(height)) {
    return(element)
  }

  vp_args <- attr(element, "viewport_data")
  assert_list(vp_args)

  if (!is.null(width)) {
    if (is.numeric(width)) width <- unit(width, "mm")
    if (!inherits(width, "unit")) stop("`width` must be a unit or numeric.", call. = FALSE)
    vp_args$width <- width
  }

  if (!is.null(height)) {
    if (is.numeric(height)) height <- unit(height, "mm")
    if (!inherits(height, "unit")) stop("`height` must be a unit or numeric.", call. = FALSE)
    vp_args$height <- height
  }

  gl <- editGrob(element, vp = do.call(viewport, vp_args))
  attr(gl, "viewport_data") <- vp_args
  attr(gl, "coords") <- prCreateBoxCoordinates(
    viewport_data = vp_args,
    extra_coordinate_functions = attr(element, "extra_coordinate_functions")
  )
  gl
}
