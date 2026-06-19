#' Reference a flowchart box position
#'
#' Creates a position reference that can be used anywhere a `move()`/`moveBox()`
#' coordinate accepts a `grid::unit()`. When `reference` is a path, the position
#' is resolved against the current box list at move time.
#'
#' @param reference A box, a `coords()` object, a list of boxes, or a path into
#'   a flowchart list such as `"groups"` or `c("groups", 1)`.
#' @param position Which position to extract. For `type = "x"` use `"center"`,
#'   `"left"`, or `"right"`. For `type = "y"` use `"center"`, `"top"`, or
#'   `"bottom"`.
#' @param type Axis to extract: `"x"` or `"y"`.
#'
#' @return A `grid::unit()` when `reference` is already a box/coords object, or
#'   a deferred position reference when `reference` is a flowchart path.
#' @export
#' @family flowchart components
#' @examples
#' fc <- flowchart(groups = list("A", "B"), ex = list("X", "Y")) |>
#'   spread(axis = "x", subelement = "groups")
#'
#' fc |>
#'   move(subelement = c("ex", 1),
#'        x = position(c("groups", 1), position = "center", type = "x") +
#'          grid::unit(5, "mm"))
position <- function(reference, position = "center", type = c("x", "y")) {
  UseMethod("position")
}

#' @export
#' @rdname position
position.default <- function(reference, position = "center", type = c("x", "y")) {
  type <- match.arg(type)
  position <- match.arg(position, c("center", "left", "right", "top", "bottom"))
  prValidatePositionAxis(position = position, type = type)

  if (is.list(reference) && !is.null(reference) && length(reference) > 0 &&
      all(vapply(reference, function(x) inherits(x, "box") || inherits(x, "coords") || is.list(x), logical(1)))) {
    return(prSelectPosition(prConvert2Coords(reference), position = position, type = type))
  }

  ref <- structure(
    list(reference = reference, position = position, type = type),
    class = "Gmisc_position_ref"
  )
  structure(list(list(0, ref, 0L)), class = c("unit", "unit_v2"))
}

#' @export
#' @rdname position
position.box <- function(reference, position = "center", type = c("x", "y")) {
  type <- match.arg(type)
  position <- match.arg(position, c("center", "left", "right", "top", "bottom"))
  prValidatePositionAxis(position = position, type = type)
  prSelectPosition(coords(reference), position = position, type = type)
}

#' @export
#' @rdname position
position.coords <- function(reference, position = "center", type = c("x", "y")) {
  type <- match.arg(type)
  position <- match.arg(position, c("center", "left", "right", "top", "bottom"))
  prValidatePositionAxis(position = position, type = type)
  prSelectPosition(reference, position = position, type = type)
}

prValidatePositionAxis <- function(position, type) {
  if (type == "x" && !position %in% c("center", "left", "right")) {
    stop("For type = 'x', `position` must be 'center', 'left', or 'right'.", call. = FALSE)
  }
  if (type == "y" && !position %in% c("center", "top", "bottom")) {
    stop("For type = 'y', `position` must be 'center', 'top', or 'bottom'.", call. = FALSE)
  }
}

prSelectPosition <- function(coords, position, type) {
  if (position == "center") {
    return(coords[[type]])
  }
  coords[[position]]
}

prUnitAsExpressionPart <- function(unit_value) {
  raw <- unclass(unit_value)
  if (is.list(raw)) {
    if (length(raw) != 1) {
      stop("Expected a length-1 unit value for position reference resolution.", call. = FALSE)
    }
    return(raw[[1]])
  }
  list(as.numeric(raw), NULL, as.integer(attr(raw, "unit")))
}

prResolvePositionValue <- function(value, element) {
  if (!inherits(value, "unit")) {
    return(value)
  }

  resolve_ref <- function(ref) {
    target <- if (inherits(ref$reference, "box") || inherits(ref$reference, "coords")) {
      ref$reference
    } else {
      get_list_element_by_path(element, ref$reference)
    }

    if (is.null(target)) {
      stop(
        "The position reference '",
        paste(ref$reference, collapse = " -> "),
        "' was not found in the provided boxes.",
        call. = FALSE
      )
    }

    prSelectPosition(prConvert2Coords(target), position = ref$position, type = ref$type)
  }

  resolve_unit <- function(u) {
    if (!inherits(u, "unit_v2")) {
      return(u)
    }

    parts <- unclass(u)
    if (!is.list(parts)) {
      return(u)
    }
    changed <- FALSE

    parts <- lapply(parts, function(part) {
      data <- part[[2]]
      if (inherits(data, "Gmisc_position_ref")) {
        changed <<- TRUE
        return(prUnitAsExpressionPart(resolve_ref(data)))
      }
      if (inherits(data, "unit")) {
        part[[2]] <- resolve_unit(data)
        changed <<- TRUE
      }
      part
    })

    if (!changed) {
      return(u)
    }
    structure(parts, class = c("unit", "unit_v2"))
  }

  resolve_unit(value)
}
