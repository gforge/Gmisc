#' Spread boxes
#'
#' Spreads a set of [`boxGrob`]/[`boxPropGrob`] in either horizontal or vertical direction.
#'
#' @param ... A set of boxes to spread. Can also be a \code{list} of boxes.
#' @param .from A box that the spread originates from. If left empty the entire `viewport` will be used.
#' @param .to A box that the spread ends at. If left empty the entire `viewport` will be used.
#' @param .type If `between` the space between the boxes will be identical while
#'  `center` has each box's center is equally distributed.
#'
#' @return `list` with the boxes that have been spread
#'
#' @md
#' @export
#' @name spread
#' @family flowchart components
#' @example inst/examples/spreadBox_ex.R
#' @rdname spread
spreadVertical <- function(..., .from = NULL, .to = NULL, .type = c('between', 'center')) {
  type = match.arg(.type)
  boxes2spread <- list(...)
  if (length(boxes2spread) == 1 && is.list(boxes2spread) && !inherits(boxes2spread, "box")) {
    boxes2spread <- boxes2spread[[1]]
  }
  assert_list(boxes2spread, min.len = 1)
  for (box in boxes2spread) {
    assert_class(box, 'box')
  }

  span_info <- prGetSpanSpace(
    boxes2spread = boxes2spread,
    .from = .from,
    .to = .to,
    type = type,
    orientation = "vertical")

  new_y_distances <- prGetNewDistances(
    span_info = span_info,
    type = type,
    convert_to_axis_fn = convertY,
    orientation = "vertical")


  prApplyBoxSpread(boxes2spread = boxes2spread,
                   distances = new_y_distances,
                   move_fn = function(box, pos) {
                     moveBox(box, y = pos, space = "absolute", just = c(NA, "center"))
                   })
}


#' @rdname spread
#' @export
spreadHorizontal <- function(..., .from = NULL, .to = NULL, .type = c('between', 'center')) {
  type = match.arg(.type)
  boxes2spread <- list(...)
  if (length(boxes2spread) == 1 && is.list(boxes2spread) && !inherits(boxes2spread, "box")) {
    boxes2spread <- boxes2spread[[1]]
  }
  assert_list(boxes2spread, min.len = 1)
  for (box in boxes2spread) {
    assert_class(box, 'box')
  }

  span_info <- prGetSpanSpace(
    boxes2spread = boxes2spread,
    .from = .from,
    .to = .to,
    type = type,
    orientation = "horizontal")

  new_x_distances <- prGetNewDistances(
    span_info = span_info,
    type = type,
    convert_to_axis_fn = convertX,
    orientation = "horizontal")


  prApplyBoxSpread(boxes2spread = boxes2spread,
                   distances = new_x_distances,
                   move_fn = function(box, pos) {
                     moveBox(box, x = pos, space = "absolute", just = "center")
                   })
}

prGetSpanSpace <- function(boxes2spread, .from, .to, type, orientation = c("vertical", "horizontal")) {
  orientation <- match.arg(orientation)
  type_size_key <- ifelse(orientation == "vertical", "height", "width")
  type_half_size_key <- paste0("half_", type_size_key)

  extra_space <- unit(0, units = "mm")
  dist_sign <- NA
  include_first <- TRUE
  include_last <- TRUE
  if (!is.null(.from)) {
    if (is.null(.to)) {
      stop("If you provide a .from box then the to has to exist")
    }
    dist <- distance(box1 = .from, box2 = .to, type = orientation)
    dist_sign <- ifelse(attr(dist, "positive"), 1, -1)
    start_pos <- attr(dist, "from")
    end_pos <- attr(dist, "to")

    if (inherits(.from, 'box')) {
      first <- .from
      include_first <- FALSE
      if (type == "center") {
        start_pos <- start_pos - dist_sign * coords(first)[[type_half_size_key]]
      }
    } else {
      first <- boxes2spread[[1]]
      if (length(boxes2spread) == 1) {
        boxes2spread <- list()
      } else {
        boxes2spread <- boxes2spread[2:length(boxes2spread)]
      }
      start_pos <- start_pos + dist_sign * coords(first)[[type_size_key]]
      dist <- dist - coords(first)[[type_size_key]]
    }

    if (inherits(.to, 'box')) {
      last <- .to
      include_last <- FALSE
      if (type == "center") {
        end_pos <- end_pos + dist_sign * coords(last)[[type_half_size_key]]
      }
    } else {
      stopifnot(length(boxes2spread) > 0)
      last <- tail(boxes2spread, 1)[[1]]
      if (length(boxes2spread) == 1) {
        boxes2spread <- list()
      } else {
        boxes2spread <- boxes2spread[1:(length(boxes2spread) - 1)]
      }
      end_pos <- end_pos - dist_sign * coords(last)[[type_size_key]]
      dist <- dist - coords(last)[[type_size_key]]
    }

    boxes_in_between <- boxes2spread

  } else {
    if (length(boxes2spread) <= 1) {
      stop("Can't spread a single box")
    }

    first <- boxes2spread[[1]]
    last <- tail(boxes2spread, 1)[[1]]

    if (orientation == "vertical") {
      start_pos <- unit(1, units = 'npc') - coords(first)[[type_size_key]]
      end_pos <- unit(0, units = 'npc') + coords(last)[[type_size_key]]
    } else {
      start_pos <- unit(0, units = 'npc') + coords(first)[[type_size_key]]
      end_pos <- unit(1, units = 'npc') - coords(last)[[type_size_key]]
    }

    dist <- distance(box1 = start_pos, box2 = end_pos, type = orientation)
    dist_sign <- ifelse(attr(dist, "positive"), 1, -1)

    boxes_in_between <- c()
    if (length(boxes2spread) > 2) {
      boxes_in_between <- boxes2spread[2:(length(boxes2spread) - 1)]
    }
  }

  if (type == "center") {
    extra_space <- coords(first)[[type_half_size_key]] + coords(last)[[type_half_size_key]]
  }

  if (orientation == "horizontal") {
    available_space <- prCnvrtX(dist + extra_space) %>% unit(units = "mm")
  } else {
    available_space <- prCnvrtY(dist + extra_space) %>% unit(units = "mm")
  }

  if (type == "between") {
    for (b in boxes_in_between) {
      available_space <- available_space - coords(b)[[type_size_key]]
    }
  }

  if (!include_first) {
    first <- NULL
  }

  if (!include_last) {
    last <- NULL
  }

  list(
    boxes_in_between = boxes_in_between,
    first = first,
    last = last,
    start_pos = start_pos,
    end_pos = end_pos,
    available_space = available_space,
    sign = dist_sign
  )
}

prGetNewDistances <- function(span_info,
                              type,
                              convert_to_axis_fn,
                              orientation = c("vertical", "horizontal")) {
  orientation <- match.arg(orientation)
  type_size_key <- ifelse(orientation == "vertical", "height", "width")
  type_half_size_key <- paste0("half_", type_size_key)

  space_distance <- (convert_to_axis_fn(span_info$available_space, unitTo = "npc", valueOnly = TRUE) /
                       (length(span_info$boxes_in_between) + 1)) %>%
    unit(units = "npc") # NPC should scale a little better than mm when resizing the window

  new_coordinate <- NULL
  offset <- span_info$start_pos
  if (!is.null(span_info$first)) {
    new_coordinate <- convert_to_axis_fn(span_info$start_pos, unitTo = "npc") - span_info$sign * coords(span_info$first)[[type_half_size_key]]
    if (type == "center") {
      offset <- new_coordinate
    } else {
      offset <- convert_to_axis_fn(span_info$start_pos, unitTo = "npc")
    }
  }

  for (b in span_info$boxes_in_between) {
    if (type == "between") {
      new_position <- convert_to_axis_fn(offset + span_info$sign * (space_distance + coords(b)[[type_half_size_key]]), unitTo = "npc")
      offset <- offset + span_info$sign * (space_distance + coords(b)[[type_size_key]])
    } else if (type == "center") {
      new_position <- convert_to_axis_fn(offset + span_info$sign * space_distance, unitTo = "npc")
      offset <- offset + span_info$sign * space_distance
    }

    if (is.null(new_coordinate)) {
      new_coordinate <- new_position
    } else {
      new_coordinate <- unit.c(new_coordinate, new_position)
    }
  }

  if (!is.null(span_info$last)) {
    last_position <- convert_to_axis_fn(span_info$end_pos, unitTo = "npc") + span_info$sign * coords(span_info$last)[[type_half_size_key]]
    new_coordinate <- unit.c(new_coordinate, last_position)
  }

  return(new_coordinate)
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

