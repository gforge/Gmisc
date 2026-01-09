#' Spread boxes
#'
#' Spreads a set of [`boxGrob`]/[`boxPropGrob`] in either horizontal or vertical
#' direction within a given span.
#'
#' The span can be defined explicitly using `.from` / `.to`, or implicitly by the
#' current viewport. Numeric values are interpreted as proportions of the viewport
#' (`npc` units).
#'
#' @param ... A set of boxes to spread. Can also be a \code{list} of boxes.
#' @param .from Starting point of the span. Can be a box, a coordinate/unit, or a
#'   numeric value interpreted as `npc`. If only `.from` is provided, `.to` defaults
#'   to `1 npc`.
#' @param .to Ending point of the span. Can be a box, a coordinate/unit, or a
#'   numeric value interpreted as `npc`. If only `.to` is provided, `.from` defaults
#'   to `0 npc`.
#' @param .margin Optional padding applied at both ends of the span. Can be a
#'   [`grid::unit`] or a numeric value interpreted as `npc`. Applied whether the
#'   span comes from `.from` / `.to` or the viewport.
#' @param .type If `between`, the space *between* boxes is identical. If `center`,
#'   the centers of the boxes are equally distributed across the span.
#' @param .subelement If a `list` of boxes is provided, this parameter can be used
#'   to target a specific element (by name or index) for the spreading operation.
#'   The function will then return the original list with the targeted element
#'   replaced by its spread version.
#'
#' @return A `list` with the boxes that have been spread.
#'
#' @md
#' @name spread
#' @family flowchart components
#' @example inst/examples/spreadBox_ex.R


#' @rdname spread
#' @export
spreadVertical <- function(..., .from = NULL, .to = NULL, .margin = unit(0, "npc"),
                           .type = c('between', 'center'), .subelement = NULL) {
  type <- match.arg(.type)
  
  .margin <- prAsNpc(.margin)
  ft <- prNormalizeFromTo(.from, .to)
  .from <- ft$from
  .to   <- ft$to

  boxes2spread <- list(...)
  if (length(boxes2spread) == 1 && is.list(boxes2spread) && !inherits(boxes2spread, "box")) {
    boxes2spread <- boxes2spread[[1]]
  }

  if (!is.null(.subelement)) {
    boxes2spread[[.subelement]] <- spreadVertical(boxes2spread[[.subelement]],
                                                   .from = .from,
                                                   .to = .to,
                                                   .margin = .margin,
                                                   .type = type)
    return(structure(boxes2spread, class = c("Gmisc_list_of_boxes", class(boxes2spread))))
  }

  assert_list(boxes2spread, min.len = 1)
  for (box in boxes2spread) {
    if (!inherits(box, "box") && !is.list(box)) {
      stop("Element must be a box or a list of boxes")
    }
  }
  
  span_info <- prGetSpanSpace(
    boxes2spread = boxes2spread,
    .from = .from,
    .to = .to,
    .margin = .margin,
    type = type,
    orientation = "vertical"
  )
  
  new_y_distances <- prGetNewDistances(
    span_info = span_info,
    type = type,
    convert_to_axis_fn = convertY,
    orientation = "vertical"
  )
  
  prApplyBoxSpread(
    boxes2spread = boxes2spread,
    distances = new_y_distances,
    move_fn = function(box, pos) {
      moveBox(box, y = pos, space = "absolute", just = c(NA, "center"))
    }
  )
}

#' @rdname spread
#' @export
spreadHorizontal <- function(..., .from = NULL, .to = NULL, .margin = unit(0, "npc"),
                             .type = c('between', 'center'), .subelement = NULL) {
  type <- match.arg(.type)
  
  .margin <- prAsNpc(.margin)
  ft <- prNormalizeFromTo(.from, .to)
  .from <- ft$from
  .to   <- ft$to
  
  boxes2spread <- list(...)
  if (length(boxes2spread) == 1 && is.list(boxes2spread) && !inherits(boxes2spread, "box")) {
    boxes2spread <- boxes2spread[[1]]
  }

  if (!is.null(.subelement)) {
    boxes2spread[[.subelement]] <- spreadHorizontal(boxes2spread[[.subelement]],
                                                     .from = .from,
                                                     .to = .to,
                                                     .margin = .margin,
                                                     .type = type)
    return(structure(boxes2spread, class = c("Gmisc_list_of_boxes", class(boxes2spread))))
  }

  assert_list(boxes2spread, min.len = 1)
  for (box in boxes2spread) {
    if (!inherits(box, "box") && !is.list(box)) {
      stop("Element must be a box or a list of boxes")
    }
  }
  
  span_info <- prGetSpanSpace(
    boxes2spread = boxes2spread,
    .from = .from,
    .to = .to,
    .margin = .margin,
    type = type,
    orientation = "horizontal"
  )
  
  new_x_distances <- prGetNewDistances(
    span_info = span_info,
    type = type,
    convert_to_axis_fn = convertX,
    orientation = "horizontal"
  )
  
  prApplyBoxSpread(
    boxes2spread = boxes2spread,
    distances = new_x_distances,
    move_fn = function(box, pos) {
      moveBox(box, x = pos, space = "absolute", just = "center")
    }
  )
}

prGetSpanSpace <- function(
    boxes2spread,
    .from,
    .to,
    .margin = unit(0, "npc"),
    type,
    orientation = c("vertical", "horizontal")
) {
  orientation <- match.arg(orientation)
  type_size_key <- ifelse(orientation == "vertical", "height", "width")
  type_half_size_key <- paste0("half_", type_size_key)
  
  # ---- input checks / normalization ----
  stopifnot(is.list(boxes2spread), length(boxes2spread) >= 1)
  for (b in boxes2spread) {
    if (!inherits(b, "box") && !is.list(b)) {
      stop("Element must be a box or a list of boxes")
    }
  }

  if (missing(type) || is.null(type)) stop("`type` must be provided.", call. = FALSE)
  if (!type %in% c("between", "center")) stop("`type` must be 'between' or 'center'.", call. = FALSE)
  
  if (is.numeric(.margin)) {
    if (length(.margin) != 1) stop("`.margin` must be a scalar.", call. = FALSE)
    .margin <- unit(.margin, "npc")
  }
  if (!inherits(.margin, "unit")) stop("`.margin` must be a grid::unit or numeric.", call. = FALSE)
  
  to_npc <- function(u) {
    if (orientation == "horizontal") convertX(u, unitTo = "npc") else convertY(u, unitTo = "npc")
  }
  
  # Normalize margin into npc and ensure non-negative
  margin_npc <- to_npc(.margin)
  if (convertWidth(margin_npc, "npc", valueOnly = TRUE) < 0) {
    stop("`.margin` must be >= 0.", call. = FALSE)
  }
  
  # If only one endpoint is provided, default the other (wrappers may already do this)
  if (!is.null(.from) && is.null(.to)) .to <- unit(1, "npc")
  if (is.null(.from) && !is.null(.to)) .from <- unit(0, "npc")
  
  dist_sign <- NA
  include_first <- TRUE
  include_last <- TRUE
  
  # ---- compute span endpoints and in-between boxes ----
  if (!is.null(.from) || !is.null(.to)) {
    stopifnot(!is.null(.from), !is.null(.to))
    
    dist <- distance(box1 = .from, box2 = .to, type = orientation)
    dist_sign <- ifelse(isTRUE(attr(dist, "positive")), 1, -1)
    start_pos <- attr(dist, "from")
    end_pos <- attr(dist, "to")
    
    # Anchor at .from: either a box or a coordinate/unit
    if (inherits(.from, "box") || is.list(.from)) {
      first <- .from
      include_first <- FALSE
      if (type == "center") start_pos <- start_pos - dist_sign * prConvert2Coords(first)[[type_half_size_key]]
    } else {
      first <- boxes2spread[[1]]
      boxes2spread <- if (length(boxes2spread) == 1) list() else boxes2spread[-1]
      start_pos <- start_pos + dist_sign * prConvert2Coords(first)[[type_size_key]]
      dist <- dist - prConvert2Coords(first)[[type_size_key]]
    }
    
    # Anchor at .to: either a box or a coordinate/unit
    if (inherits(.to, "box") || is.list(.to)) {
      last <- .to
      include_last <- FALSE
      if (type == "center") end_pos <- end_pos + dist_sign * prConvert2Coords(last)[[type_half_size_key]]
    } else {
      if (length(boxes2spread) == 0) {
        stop("No boxes left to place between `.from` and `.to`.", call. = FALSE)
      }
      last <- tail(boxes2spread, 1)[[1]]
      boxes2spread <- if (length(boxes2spread) == 1) list() else boxes2spread[-length(boxes2spread)]
      end_pos <- end_pos - dist_sign * prConvert2Coords(last)[[type_size_key]]
      dist <- dist - prConvert2Coords(last)[[type_size_key]]
    }
    
    boxes_in_between <- boxes2spread
    
  } else {
    # Viewport mode
    if (length(boxes2spread) <= 1) stop("Can't spread a single box.", call. = FALSE)
    
    first <- boxes2spread[[1]]
    last <- tail(boxes2spread, 1)[[1]]
    
    if (orientation == "vertical") {
      start_pos <- unit(1, "npc") - margin_npc - prConvert2Coords(first)[[type_size_key]]
      end_pos   <- unit(0, "npc") + margin_npc + prConvert2Coords(last)[[type_size_key]]
    } else {
      start_pos <- unit(0, "npc") + margin_npc + prConvert2Coords(first)[[type_size_key]]
      end_pos   <- unit(1, "npc") - margin_npc + prConvert2Coords(last)[[type_size_key]]
    }
    
    dist <- distance(box1 = start_pos, box2 = end_pos, type = orientation)
    dist_sign <- ifelse(isTRUE(attr(dist, "positive")), 1, -1)
    
    boxes_in_between <- if (length(boxes2spread) > 2) {
      boxes2spread[2:(length(boxes2spread) - 1)]
    } else {
      list()
    }
  }
  
  # ---- apply margin for explicit .from/.to too ----
  # (viewport already included margin in start/end)
  if (!is.null(.from) && !is.null(.to)) {
    # shrink span on both ends by margin
    start_pos <- start_pos + dist_sign * margin_npc
    end_pos   <- end_pos   - dist_sign * margin_npc
    dist      <- dist - 2 * margin_npc
  }
  
  # ---- compute available space in npc ----
  extra_space <- unit(0, "npc")
  if (type == "center") {
    extra_space <- to_npc(prConvert2Coords(first)[[type_half_size_key]] + prConvert2Coords(last)[[type_half_size_key]])
  }
  
  span_npc <- to_npc(dist) + extra_space
  
  # Must not be negative after margin/size adjustments
  if (convertWidth(span_npc, "npc", valueOnly = TRUE) < 0) {
    stop(
      "No space left to spread boxes: span collapsed (from/to too close or margin too large).",
      call. = FALSE
    )
  }
  
  available_space <- span_npc
  
  if (type == "between" && length(boxes_in_between) > 0) {
    for (b in boxes_in_between) {
      available_space <- available_space - to_npc(prConvert2Coords(b)[[type_size_key]])
    }
    if (convertWidth(available_space, "npc", valueOnly = TRUE) < 0) {
      stop(
        "No space left to spread boxes: in-between boxes exceed the available span.",
        call. = FALSE
      )
    }
  }
  
  if (!include_first) first <- NULL
  if (!include_last)  last  <- NULL
  
  list(
    boxes_in_between = boxes_in_between,
    first = first,
    last = last,
    start_pos = start_pos,
    end_pos = end_pos,
    available_space = available_space, # npc
    sign = dist_sign
  )
}


prGetNewDistances <- function(span_info, type, convert_to_axis_fn,
                              orientation = c("vertical", "horizontal")) {
  orientation <- match.arg(orientation)
  type_size_key <- ifelse(orientation == "vertical", "height", "width")
  type_half_size_key <- paste0("half_", type_size_key)
  
  to_npc <- function(u) convert_to_axis_fn(u, unitTo = "npc")
  
  space_distance <- span_info$available_space / (length(span_info$boxes_in_between) + 1)
  
  new_coordinate <- NULL
  offset <- to_npc(span_info$start_pos)
  
  if (!is.null(span_info$first)) {
    new_coordinate <- to_npc(span_info$start_pos) - span_info$sign * to_npc(prConvert2Coords(span_info$first)[[type_half_size_key]])
    offset <- if (type == "center") new_coordinate else to_npc(span_info$start_pos)
  }
  
  for (b in span_info$boxes_in_between) {
    if (type == "between") {
      new_position <- offset + span_info$sign * (space_distance + to_npc(prConvert2Coords(b)[[type_half_size_key]]))
      offset <- offset + span_info$sign * (space_distance + to_npc(prConvert2Coords(b)[[type_size_key]]))
    } else {
      new_position <- offset + span_info$sign * space_distance
      offset <- offset + span_info$sign * space_distance
    }
    new_coordinate <- if (is.null(new_coordinate)) new_position else unit.c(new_coordinate, new_position)
  }
  
  if (!is.null(span_info$last)) {
    last_position <- to_npc(span_info$end_pos) + span_info$sign * to_npc(prConvert2Coords(span_info$last)[[type_half_size_key]])
    new_coordinate <- unit.c(new_coordinate, last_position)
  }
  
  new_coordinate
}


prApplyBoxSpread <- function(boxes2spread, distances, move_fn) {
  ret <- list()
  for (i in 1:length(boxes2spread)) {
    box <- move_fn(box = boxes2spread[[i]], pos = distances[i])

    if (is.null(names(boxes2spread))) {
      ret <- append(ret, list(box))
    } else {
      key <- names(boxes2spread)[i]
      if (key == "") {
        ret <- append(ret, list(box))
      } else {
        ret[[key]] <- box
      }
    }
  }

  structure(
    ret,
    class = c("Gmisc_list_of_boxes", class(ret)))
}

