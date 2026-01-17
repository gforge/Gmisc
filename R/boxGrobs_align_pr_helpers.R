# Internal helpers for align functions ----------------------------------------

# Normalize and validate the boxes argument for align* functions

# Helper: Is the object a list whose first element is itself a list
# but not a single box or grob? This captures the common "piped list"
# pattern where the caller supplied a list that got wrapped as an element
# inside `...` (e.g., `my_list |> alignHorizontal(reference=...)`).
prIsNestedNonBoxList <- function(x) {
    is.list(x) && length(x) >= 1 && is.list(x[[1]]) && !inherits(x[[1]], "box") && !is.grob(x[[1]])
}

# Helper: Is this a single-element wrapper (length == 1) that should be unwrapped
prIsSingleElementWrappedList <- function(x) {
    length(x) == 1 && prIsNestedNonBoxList(x)
}

# Helper: Does the list contain multiple top-level elements and the first
# element is itself a nested list container (not a box/grob)? This is used
# as the condition for searching inside the first container when resolving
# `.subelement` paths (we only do this when there are multiple top-level
# elements to avoid changing behavior for single-element inputs).
prHasNestedFirstContainer <- function(x) {
    is.list(x) && length(x) > 1 && is.list(x[[1]]) && !inherits(x[[1]], "box") && !is.grob(x[[1]])
}

prNormalizeAndValidateBoxes <- function(boxes2align) {
    # If a single element list contains another list of boxes (e.g., result of piping),
    # unwrap it unless the inner element is itself a box/grob.
    if (prIsSingleElementWrappedList(boxes2align)) {
        boxes2align <- boxes2align[[1]]
    }

    # Ensure it's a list of at least one element
    assert_list(boxes2align, min.len = 1)

    # Validate members
    for (box in boxes2align) {
        if (!inherits(box, "box") && !is.list(box) && !is.grob(box)) {
            stop("Element must be a box or a list of boxes")
        }
    }

    boxes2align
}

# Resolve a reference that may be provided as a path into boxes2align
prResolveReference <- function(reference, boxes2align) {
    if (is.atomic(reference) && length(boxes2align) > 0 && !inherits(reference, "box")) {
        maybe_ref <- get_list_element_by_path(boxes2align, reference)
        if (is.null(maybe_ref) && prIsNestedNonBoxList(boxes2align)) {
            maybe_ref <- get_list_element_by_path(boxes2align[[1]], reference)
        }
        if (!is.null(maybe_ref)) {
            return(maybe_ref)
        }
        # If reference is numeric but not a valid index, treat as coordinate (do not error).
        # Be careful: `grid::unit` objects may appear numeric-ish to some predicates but
        # are not numeric scalars and do not support `==` comparisons against numbers.
        # Ensure we only do numeric comparisons on bare numerics (not 'unit' objects).
        if (is.numeric(reference) && !inherits(reference, "unit") && length(reference) == 1 && (reference == 0 || reference > length(boxes2align))) {
            return(reference)
        }
    }
    reference
}

# Apply alignment given resolved boxes and ref positions
prApplyAlign <- function(boxes2align, ref_positions, position, axis = c("vertical", "horizontal")) {
    axis <- match.arg(axis)
    if (axis == "vertical") {
        ret <- lapply(boxes2align, function(box) {
            box_pos <- prConvert2Coords(box)
            if (position == "center") {
                new_y <- ref_positions$y
            } else if (position == "bottom") {
                new_y <- ref_positions$bottom + box_pos$half_height
            } else if (position == "top") {
                new_y <- ref_positions$top - box_pos$half_height
            } else {
                stop("Invalid position: ", position)
            }
            moveBox(box, y = new_y, just = c(NA, "center"))
        })
    } else {
        ret <- lapply(boxes2align, function(box) {
            box_pos <- prConvert2Coords(box)
            if (position == "center") {
                new_x <- ref_positions$x
            } else if (position == "left") {
                new_x <- ref_positions$left + box_pos$half_width
            } else if (position == "right") {
                new_x <- ref_positions$right - box_pos$half_width
            } else {
                stop("Invalid position: ", position)
            }
            moveBox(box, x = new_x, just = "center")
        })
    }
    prExtendClass(ret, "Gmisc_list_of_boxes")
}
