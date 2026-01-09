#' Align boxes
#'
#' Aligns a set of [`boxGrob`]/[`boxPropGrob`] according to the first positional argument.
#'
#' @param reference A [`boxGrob`]/[`boxPropGrob`]/[`coords`] object or a [`unit`][grid::unit] or a
#'  numerical value that can be converted into a [`unit`][grid::unit] of `npc` type.
#' @param ... A set of boxes.
#' @param .position How to align the boxes, differs slightly for vertical and horizontal alignment
#'  see the accepted arguments
#' @param .subelement If a `list` of boxes is provided, this parameter can be used
#'   to target a specific element (by name or index) for the alignment operation.
#'   The function will then return the original list with the targeted element
#'   replaced by its aligned version.
#' @return `list` with the boxes that are to be aligned
#'
#' @md
#' @export
#' @name align
#' @family flowchart components
#' @example inst/examples/alignBox_ex.R
#' @rdname align
alignVertical <- function(reference, ..., .position = c('center', 'top', 'bottom'), .subelement = NULL) {
  position = match.arg(.position)
  
  boxes2align <- list(...)
  if (length(boxes2align) == 1 && is.list(boxes2align) && !inherits(boxes2align, "box")) {
    boxes2align <- boxes2align[[1]]
  }

  if (!is.null(.subelement)) {
    boxes2align[[.subelement]] <- alignVertical(reference = reference,
                                                 boxes2align[[.subelement]],
                                                 .position = position)
    return(structure(boxes2align, class = c("Gmisc_list_of_boxes", class(boxes2align))))
  }

  ref_positions <- prConvert2Coords(reference)
  assert_list(boxes2align, min.len = 1)
  for (box in boxes2align) {
    if (!inherits(box, 'box') && !is.list(box)) {
      stop("Element must be a box or a list of boxes")
    }
  }

  ret <- sapply(boxes2align,
                FUN = function(box, ref_pos) {
                  box_pos <- prConvert2Coords(box)
                  if (position == "center") {
                    new_y <- ref_pos$y
                  } else if (position == "bottom") {
                    new_y <- ref_pos$bottom + box_pos$half_height
                  } else if (position == "top") {
                    new_y <- ref_pos$top - box_pos$half_height
                  } else {
                    # Should be unreachable to get here
                    stop("Invalid position: ", position)
                  }
                  moveBox(box, y = new_y, just = c(NA, "center"))
                },
                ref_pos = ref_positions,
                simplify = FALSE)

  structure(
    ret,
    class = c("Gmisc_list_of_boxes", class(ret)))
}

#' @rdname align
#' @param .sub_position When the box is a [`boxPropGrob`] it not only has the general `.positions` but
#'  also `left` and `right` which can be viewed as separate boxes that have simply been merged.
#' @md
#' @export
alignHorizontal <- function(reference, ..., .position = c('center', 'left', 'right'), .sub_position = c('none', 'left', 'right'), .subelement = NULL) {
  position = match.arg(.position)
  sub_position = match.arg(sub_position)
  
  boxes2align <- list(...)
  if (length(boxes2align) == 1 && is.list(boxes2align) && !inherits(boxes2align, "box")) {
    boxes2align <- boxes2align[[1]]
  }

  if (!is.null(.subelement)) {
    boxes2align[[.subelement]] <- alignHorizontal(reference = reference,
                                                   boxes2align[[.subelement]],
                                                   .position = position,
                                                   .sub_position = sub_position)
    return(structure(boxes2align, class = c("Gmisc_list_of_boxes", class(boxes2align))))
  }

  assert_list(boxes2align, min.len = 1)
  for (box in boxes2align) {
    if (!inherits(box, 'box') && !is.list(box)) {
      stop("Element must be a box or a list of boxes")
    }
  }

  ref_positions <- prConvert2Coords(reference)
  if (sub_position != "none") {
    if (sub_position == 'left') {
      assert_class(ref_positions$left_x, "unit")
      ref_positions$x <- ref_positions$left_x
      ref_positions$right <- ref_positions$prop_x
    } else {
      assert_class(ref_positions$right_x, "unit")
      ref_positions$x <- ref_positions$right_x
      ref_positions$left <- ref_positions$prop_x
    }
  }

  ret <- sapply(boxes2align,
                FUN = function(box, ref_pos) {
                  box_pos <- prConvert2Coords(box)
                  if (position == "center") {
                    new_x <- ref_pos$x
                  } else if (position == "left") {
                    new_x <- ref_pos$left + box_pos$half_width
                  } else if (position == "right") {
                    new_x <- ref_pos$right - box_pos$half_width
                  } else {
                    # Should be unreachable to get here
                    stop("Invalid position: ", position)
                  }

                  moveBox(box, x = new_x, just = "center")
                },
                ref_pos = ref_positions,
                simplify = FALSE)

  structure(
    ret,
    class = c("Gmisc_list_of_boxes", class(ret)))
}
