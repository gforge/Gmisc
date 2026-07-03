#' Align boxes
#'
#' Aligns a set of [`boxGrob`]/[`boxPropGrob`] according to the first positional argument.
#'
#' @param reference A [`boxGrob`]/[`boxPropGrob`]/[`coords`] object or a [`unit`][grid::unit] or a
#'  numerical value that can be converted into a [`unit`][grid::unit] of `npc` type. If a numeric scalar is
#'  provided and is `0` or greater than the number of boxes, it is treated as a coordinate (no error is raised).
#' @param references Optional pair of references. When supplied, the alignment
#'   reference is the midpoint between the two references on the selected axis.
#'   This is useful for centering side boxes, such as excluded patients, between
#'   the main boxes before and after them. Each reference can be anything
#'   accepted by `reference`, including a path such as `"assessed"` or
#'   `c("arms", 1)`.
#' @param ... A set of boxes.
#' @param position How to align the boxes, differs slightly for vertical and horizontal alignment
#'  see the accepted arguments
#' @param subelement If a `list` of boxes is provided, this parameter can be used
#'   to target a specific element (by name or index), a deep path into nested
#'   lists (e.g., `c("detail", 1)`), or a regex selector created with
#'   [stringr::regex()] that is matched against top-level names
#'   (e.g. `stringr::regex("^groups")`). You can also provide multiple targets
#'   as a list of paths or selectors (e.g.,
#'   `list(c("detail", 1), stringr::regex("^followup"))`).
#'   Bare character strings are always treated as literal paths.
#'   When a list of boxes is *piped* into `alignVertical()`/`alignHorizontal()` and a named `reference` is
#'   provided (e.g. `my_boxes |> alignHorizontal(reference = c("grp","sub"), .subelement = ...)`), the
#'   function will unwrap a single-element wrapper so nested `.subelement` targets are found as expected.
#'   The function will return the original list with the targeted element(s)
#'   replaced by their aligned version(s).
#' @return `list` with the boxes that are to be aligned
#'
#' @md
#' @export
#' @name align
#' @family flowchart components
#' @example inst/examples/alignBox_ex.R
#' @rdname align


alignVertical <- function(reference = NULL, ..., position = c("center", "top", "bottom"), subelement = NULL, references = NULL) {
  position <- match.arg(position)

  boxes2align <- list(...)


  # Handle pipeline where the piped object becomes the positional `reference`
  # but the user also provided a named `reference` (it ends up in `...`).
  # In that case, move the positional `reference` into `boxes2align` and use
  # the named `reference` value as the actual reference.
  if (!is.null(names(boxes2align)) &&
    "reference" %in% names(boxes2align) &&
    is.list(reference) &&
    !inherits(reference, "box")) {
    named_ref <- boxes2align$reference
    boxes2align$reference <- NULL
    boxes2align <- reference
    reference <- named_ref
  }

  # Support piping a list into alignVertical() while passing a positional
  # reference path, e.g. `boxes |> alignVertical("groups", subelement = "groups2")`.
  # In this shape, the piped object is captured as `reference` and the first
  # positional `...` argument is the intended reference.
  if (is.list(reference) &&
    !inherits(reference, "box") &&
    length(boxes2align) >= 1 &&
    is.null(names(boxes2align)) &&
    !inherits(boxes2align[[1]], "box") &&
    (is.atomic(boxes2align[[1]]) || is.list(boxes2align[[1]]))) {
    ref_arg <- boxes2align[[1]]
    boxes2align <- reference
    reference <- ref_arg
  }

  # Support piping a list into alignVertical(): if the user pipes a list as the
  # first argument and provides no '...' boxes, treat that list as the set of
  # boxes to align and use its first element as the reference.
  if (length(boxes2align) == 0 && is.list(reference) && !inherits(reference, "box")) {
    boxes2align <- reference
    reference <- if (is.null(references)) boxes2align[[1]] else NULL
  }

  # If a list of boxes was piped into '...' as a single element (e.g., using
  # the base pipe with a named 'reference' argument), unwrap it so callers
  # can pass either `list_of_boxes |> alignVertical(...)` or `alignVertical(list_of_boxes)`.
  if (prIsSingleElementWrappedList(boxes2align)) {
    boxes2align <- boxes2align[[1]]
  }

  if (!is.null(references)) {
    if (!is.null(reference)) {
      stop("Use either `reference` or `references`, not both.", call. = FALSE)
    }
    prValidateReferencePair(references)
  }

  if (!is.null(subelement)) {
    # Normalize into a list of paths (each path is an atomic vector)
    paths <- prResolveSubelementSelector(subelement, boxes2align)

    for (path in paths) {
      # Find target using helper (search top-level and first nested container)
      # Try finding the target directly (top-level)
      target <- get_list_element_by_path(boxes2align, path)
      container_is_first <- FALSE

      # If not found, try inside the common nested first element
      if (is.null(target) && prHasNestedFirstContainer(boxes2align)) {
        target <- get_list_element_by_path(boxes2align[[1]], path)
        container_is_first <- TRUE
      }

      if (is.null(target)) {
        stop("The subelement '", paste(path, collapse = " -> "), "' was not found in the provided boxes.",
          call. = FALSE
        )
      }

      ref_for_call <- prResolveAlignReference(
        reference = reference,
        references = references,
        boxes2align = boxes2align,
        axis = "vertical"
      )

      target_boxes <- if (inherits(target, "box")) list(target) else target
      aligned <- prApplyAlign(
        boxes2align = prNormalizeAndValidateBoxes(target_boxes),
        ref_positions = prConvert2Coords(ref_for_call),
        position = position,
        axis = "vertical"
      )

      # If the target was a single box, the recursive call returns a
      # single-element list-of-boxes; unwrap it so we don't insert a
      # list wrapper into the parent structure (which can create
      # self-referential/cyclic lists in subsequent operations).
      if (is.list(aligned) && length(aligned) == 1 && inherits(target, "box") && inherits(aligned[[1]], "box")) {
        aligned <- aligned[[1]]
      }

      if (container_is_first) {
        boxes2align[[1]] <- set_list_element_by_path(boxes2align[[1]], path, aligned)
      } else {
        boxes2align <- set_list_element_by_path(boxes2align, path, aligned)
      }
    }

    return(prExtendClass(boxes2align, "Gmisc_list_of_boxes"))
  }

  # Normalize and validate boxes now that subelement processing is done
  boxes2align <- prNormalizeAndValidateBoxes(boxes2align)

  reference <- prResolveAlignReference(
    reference = reference,
    references = references,
    boxes2align = boxes2align,
    axis = "vertical"
  )
  ref_positions <- prConvert2Coords(reference)

  # Apply vertical alignment
  return(prApplyAlign(boxes2align, ref_positions, position, axis = "vertical"))
}


#' @rdname align
#' @param sub_position When the box is a [`boxPropGrob`] it not only has the general `.positions` but
#'  also `left` and `right` which can be viewed as separate boxes that have simply been merged.
#' @md
#' @export
alignHorizontal <- function(
  reference = NULL,
  ...,
  position = c("center", "left", "right"),
  sub_position = c("none", "left", "right"),
  subelement = NULL,
  references = NULL
) {
  position <- match.arg(position)
  sub_position <- match.arg(sub_position)

  boxes2align <- list(...)


  # Handle pipeline where the piped object becomes the positional `reference`
  # but the user also provided a named `reference` (it ends up in `...`).
  # In that case, move the positional `reference` into `boxes2align` and use
  # the named `reference` value as the actual reference.
  if (!is.null(names(boxes2align)) && "reference" %in% names(boxes2align) && is.list(reference) && !inherits(reference, "box")) {
    named_ref <- boxes2align$reference
    boxes2align$reference <- NULL
    boxes2align <- reference
    reference <- named_ref
  }

  # Support piping a list into alignHorizontal() while passing a positional
  # reference path, e.g. `boxes |> alignHorizontal("groups", subelement = "groups2")`.
  if (is.list(reference) &&
    !inherits(reference, "box") &&
    length(boxes2align) >= 1 &&
    is.null(names(boxes2align)) &&
    !inherits(boxes2align[[1]], "box") &&
    (is.atomic(boxes2align[[1]]) || is.list(boxes2align[[1]]))) {
    ref_arg <- boxes2align[[1]]
    boxes2align <- reference
    reference <- ref_arg
  }

  # Support piping a list into alignHorizontal(): if the user pipes a list as the
  # first argument and provides no '...' boxes, treat that list as the set of
  # boxes to align and use its first element as the reference.
  if (length(boxes2align) == 0 && is.list(reference) && !inherits(reference, "box")) {
    boxes2align <- reference
    reference <- if (is.null(references)) boxes2align[[1]] else NULL
  }

  # If a list of boxes was piped into '...' as a single element (e.g., using
  # the base pipe with a named 'reference' argument), unwrap it so callers
  # can pass either `list_of_boxes |> alignHorizontal(...)` or `alignHorizontal(list_of_boxes)`.
  if (prIsSingleElementWrappedList(boxes2align)) {
    boxes2align <- boxes2align[[1]]
  }

  if (!is.null(references)) {
    if (!is.null(reference)) {
      stop("Use either `reference` or `references`, not both.", call. = FALSE)
    }
    prValidateReferencePair(references)
  }

  if (!is.null(subelement)) {
    # Normalize into a list of paths (each path is an atomic vector)
    paths <- prResolveSubelementSelector(subelement, boxes2align)

    for (path in paths) {
      # Find target using helper (search top-level and first nested container)
      res <- prFindSubelementTarget(boxes2align, path)
      target <- res$target
      container_is_first <- res$container_is_first

      if (is.null(target)) {
        stop("The subelement '", paste(path, collapse = " -> "), "' was not found in the provided boxes.",
          call. = FALSE
        )
      }

      ref_for_call <- prResolveAlignReference(
        reference = reference,
        references = references,
        boxes2align = boxes2align,
        axis = "horizontal"
      )
      ref_positions <- prConvert2Coords(ref_for_call)
      if (sub_position != "none") {
        if (sub_position == "left") {
          assert_class(ref_positions$left_x, "unit")
          ref_positions$x <- ref_positions$left_x
          ref_positions$right <- ref_positions$prop_x
        } else {
          assert_class(ref_positions$right_x, "unit")
          ref_positions$x <- ref_positions$right_x
          ref_positions$left <- ref_positions$prop_x
        }
      }

      target_boxes <- if (inherits(target, "box")) list(target) else target
      aligned <- prApplyAlign(
        boxes2align = prNormalizeAndValidateBoxes(target_boxes),
        ref_positions = ref_positions,
        position = position,
        axis = "horizontal"
      )

      # Unwrap single-element aligned box when target is a single box to
      # avoid inserting an unnecessary list wrapper into the parent.
      if (is.list(aligned) && length(aligned) == 1 && inherits(target, "box") && inherits(aligned[[1]], "box")) {
        aligned <- aligned[[1]]
      }

      if (container_is_first) {
        boxes2align[[1]] <- set_list_element_by_path(boxes2align[[1]], path, aligned)
      } else {
        boxes2align <- set_list_element_by_path(boxes2align, path, aligned)
      }
    }

    return(prExtendClass(boxes2align, "Gmisc_list_of_boxes"))
  }

  # Normalize and validate boxes now that subelement processing is done
  boxes2align <- prNormalizeAndValidateBoxes(boxes2align)

  reference <- prResolveAlignReference(
    reference = reference,
    references = references,
    boxes2align = boxes2align,
    axis = "horizontal"
  )
  ref_positions <- prConvert2Coords(reference)
  if (sub_position != "none") {
    if (sub_position == "left") {
      assert_class(ref_positions$left_x, "unit")
      ref_positions$x <- ref_positions$left_x
      ref_positions$right <- ref_positions$prop_x
    } else {
      assert_class(ref_positions$right_x, "unit")
      ref_positions$x <- ref_positions$right_x
      ref_positions$left <- ref_positions$prop_x
    }
  }

  # Apply horizontal alignment
  return(prApplyAlign(boxes2align, ref_positions, position, axis = "horizontal"))
}
