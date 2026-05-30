#' Append to list of boxes (S3)
#'
#' Append an element to a list of boxes, preserving attributes/class.
#' Masks `base::append` to support S3 dispatch.
#'
#' @param x A `Gmisc_list_of_boxes` or `list`.
#' @param values The object(s) to append. If a single `boxGrob` is provided, it is wrapped in a list.
#' @param after Subscript, after which the values are to be appended.
#'
#' @return Updated list of boxes.
#' @export
#' @family flowchart components
append <- function(x, values, after = length(x)) {
  UseMethod("append")
}

#' @export
#' @rdname append
append.default <- function(x, values, after = length(x)) {
  base::append(x, values, after)
}

#' @export
#' @rdname append
append.Gmisc_list_of_boxes <- function(x, values, after = length(x)) {
  if (inherits(values, "box")) {
    values <- list(values)
  }

  ret <- base::append(x, values, after)

  # Restore attributes
  special_attrs <- setdiff(names(attributes(x)), c("names", "class"))
  for (at in special_attrs) {
    if (is.null(attr(ret, at))) {
      attr(ret, at) <- attr(x, at)
    }
  }

  prConvertListToBoxList(ret)
}

#' Insert element into a list of boxes (S3)
#'
#' Inserts a box into a list of boxes and positions it relative to the
#' surrounding boxes.
#'
#' @param x A `Gmisc_list_of_boxes`.
#' @param element The new box object to insert.
#' @param ... Not used.
#' @param after The name or index of the box after which to insert.
#' @param before The name or index of the box before which to insert.
#'
#' @return The updated list of boxes with the new element inserted.
#' @export
#' @family flowchart components
insert <- function(x, element, ..., after = NULL, before = NULL) {
  UseMethod("insert")
}

#' @export
#' @rdname insert
insert.default <- function(x, element, ..., after = NULL, before = NULL) {
  if (is.list(x) && !inherits(x, "box")) {
    return(insert(prConvertListToBoxList(x), element, ..., after = after, before = before))
  }
  stop("insert() expects a list of boxes as first argument")
}

#' @export
#' @rdname insert
insert.Gmisc_list_of_boxes <- function(x, element, ..., after = NULL, before = NULL) {
  if (!xor(is.null(after), is.null(before))) {
    stop("You must specify either 'after' or 'before' (but not both).")
  }

  element_name <- NULL
  if (inherits(element, "list") && length(element) == 1 && inherits(element[[1]], "box")) {
    element_name <- names(element)
    element <- element[[1]]
  }
  if (!inherits(element, "box")) {
    stop("inserted element must be a box")
  }

  # Find insertion index
  idx <- NULL
  if (!is.null(after)) {
    if (is.character(after)) {
      idx <- match(after, names(x))
      if (is.na(idx)) stop("Could not find box named '", after, "'")
    } else {
      idx <- after
    }
  } else {
    if (is.character(before)) {
      match_idx <- match(before, names(x))
      if (is.na(match_idx)) stop("Could not find box named '", before, "'")
      idx <- match_idx - 1
    } else {
      idx <- before - 1
    }
  }

  # Calculate bounds for positioning
  # If inserting after box A (index idx):
  # - Prev box is x[[idx]]
  # - Next box is x[[idx+1]] (if exists)

  # If inserting before box B (index match_idx):
  # - Prev box is x[[match_idx-1]] -> x[[idx]]
  # - Next box is x[[match_idx]] -> x[[idx+1]]

  prev_box <- if (idx > 0 && idx <= length(x)) x[[idx]] else NULL
  next_box <- if (idx + 1 <= length(x)) x[[idx + 1]] else NULL

  if (!is.null(prev_box) && !is.null(next_box)) {
    # Position between
    pc <- coords(prev_box)
    nc <- coords(next_box)

    # Determine orientation?
    # Heuristic: mostly different X -> horizontal split
    # Mostly different Y -> vertical split

    dx <- abs(convertX(pc$x - nc$x, "mm", valueOnly = TRUE))
    dy <- abs(convertY(pc$y - nc$y, "mm", valueOnly = TRUE))

    new_x <- unit(0.5, "npc") # default if fail
    new_y <- unit(0.5, "npc") # default if fail

    # Midpoint calculation
    # X
    px <- convertX(pc$x, "npc", valueOnly = FALSE)
    nx <- convertX(nc$x, "npc", valueOnly = FALSE)
    new_x <- px + (nx - px) * 0.5

    # Y
    py <- convertY(pc$y, "npc", valueOnly = FALSE)
    ny <- convertY(nc$y, "npc", valueOnly = FALSE)
    new_y <- py + (ny - py) * 0.5

    element <- moveBox(element, x = new_x, y = new_y, space = "absolute", just = "center")
  } else if (!is.null(prev_box)) {
    # Only prev box exists (append at end or start with no next)
    # Just place valid coords if not present?
    # User implies "mid position", if no two boxes, can't calc mid.
    # Maybe just keep existing coords if element has them, or default offset?
    # For now, do nothing if we can't interpolate.
  } else if (!is.null(next_box)) {
    # Only next box exists
  }

  # Perform insertion
  to_ins <- list(element)
  if (!is.null(element_name)) names(to_ins) <- element_name
  append(x, to_ins, after = idx)
}
