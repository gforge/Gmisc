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
# `subelement` paths (we only do this when there are multiple top-level
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
        prAssertBoxOrListOfBoxes(box)
    }

    boxes2align
}

prAssertBoxOrListOfBoxes <- function(box) {
    if (!inherits(box, "box") && !is.list(box) && !is.grob(box)) {
        if (inherits(box, "character")) {
            stop("Element must be a box or a list of boxes, got character: '", box, "'", call. = FALSE)
        }

        if (inherits(box, "numeric")) {
            stop("Element must be a box or a list of boxes, got numeric: ", box, call. = FALSE)
        }

        stop("Element must be a box or a list of boxes, got object of class ", paste(class(box), collapse = ", "), call. = FALSE)
    }
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

prValidateReferencePair <- function(references) {
    if (!is.list(references) || length(references) != 2) {
        stop("`references` must be a list containing exactly two references.", call. = FALSE)
    }
}

prResolveReferenceStrict <- function(reference, boxes2align) {
    resolved <- prResolveReference(reference, boxes2align)

    if (identical(resolved, reference) &&
        (is.atomic(reference) || is.list(reference)) &&
        !inherits(reference, "box") &&
        !inherits(reference, "coords") &&
        !inherits(reference, "unit") &&
        !is.numeric(reference)) {
        stop(
            "The reference '",
            paste(reference, collapse = " -> "),
            "' was not found in the provided boxes.",
            call. = FALSE
        )
    }

    resolved
}

prResolveAlignReferenceArgs <- function(reference, references, boxes2align, axis = c("vertical", "horizontal")) {
    axis <- match.arg(axis)

    if (is.null(references)) {
        return(list(
            reference = prResolveReference(reference, boxes2align),
            references = NULL
        ))
    }

    list(
        reference = NULL,
        references = lapply(references, prResolveReferenceStrict, boxes2align = boxes2align)
    )
}

prResolveAlignReference <- function(reference, references, boxes2align, axis = c("vertical", "horizontal")) {
    axis <- match.arg(axis)

    if (is.null(references)) {
        if (is.null(reference)) {
            reference <- boxes2align[[1]]
        }
        return(prResolveReference(reference, boxes2align))
    }

    resolved <- prResolveAlignReferenceArgs(
        reference = reference,
        references = references,
        boxes2align = boxes2align,
        axis = axis
    )$references

    prReferencePairMidpoint(resolved, axis = axis)
}

prReferencePairMidpoint <- function(references, axis = c("vertical", "horizontal")) {
    axis <- match.arg(axis)
    coords_pair <- lapply(references, prConvert2Coords)

    c1 <- coords_pair[[1]]
    c2 <- coords_pair[[2]]

    if (axis == "vertical") {
        y <- c1$y + (c2$y - c1$y) * 0.5
        return(structure(list(
            x = unit(0.5, "npc"),
            y = y,
            top = y,
            bottom = y,
            left = unit(0.5, "npc"),
            right = unit(0.5, "npc"),
            width = unit(0, "npc"),
            height = unit(0, "npc"),
            half_width = unit(0, "npc"),
            half_height = unit(0, "npc")
        ), class = c("coords", "box_coords", "list")))
    }

    x <- c1$x + (c2$x - c1$x) * 0.5
    structure(list(
        x = x,
        y = unit(0.5, "npc"),
        top = unit(0.5, "npc"),
        bottom = unit(0.5, "npc"),
        left = x,
        right = x,
        width = unit(0, "npc"),
        height = unit(0, "npc"),
        half_width = unit(0, "npc"),
        half_height = unit(0, "npc")
    ), class = c("coords", "box_coords", "list"))
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
