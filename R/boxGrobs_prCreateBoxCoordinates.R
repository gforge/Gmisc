#' Creates coordinates for box
#'
#' @param viewport_data The arguments that will be used for generating the \code{viewport}
#' @param extra_coordinate_functions A list with named functions if we want additional
#'  parameters
#' @param box_fn_bounds Optional named list with `left`, `right`, `bottom`, and `top`
#'  proportions (each in `[0, 1]`, with `left < right` and `bottom < top`) describing the
#'  visible bounds of a non-rectangular box shape within its viewport. When supplied the
#'  coordinates are shrunk to these bounds so connectors anchor to the visible shape edge.
#' @return \code{list} of class \code{coords}
#' @importFrom checkmate assert_list
prCreateBoxCoordinates <- function(viewport_data, extra_coordinate_functions = NULL, box_fn_bounds = NULL) {
  # Adjust center depending on the viewport position
  x <- prAdjustPos(viewport_data$just, viewport_data$x, viewport_data$width, axis = "x")
  y <- prAdjustPos(viewport_data$just, viewport_data$y, viewport_data$height, axis = "y")

  height <- viewport_data$height
  if (!is.unit(height)) height <- unit(height, "npc")

  width <- viewport_data$width
  if (!is.unit(width)) width <- unit(width, "npc")

  half_height <- height * 0.5
  half_width <- width * 0.5

  coordinates <- list(
    left = x - half_width,
    right = x + half_width,
    bottom = y - half_height,
    top = y + half_height,
    x = x,
    y = y,
    width = viewport_data$width,
    height = viewport_data$height,
    half_height = half_height,
    half_width = half_width
  )

  if (!is.null(box_fn_bounds)) {
    if (!is.list(box_fn_bounds) ||
        !all(c("left", "right", "bottom", "top") %in% names(box_fn_bounds))) {
      stop("`box_fn_bounds` must be a list with left, right, bottom, and top.", call. = FALSE)
    }

    bounds <- unlist(box_fn_bounds[c("left", "right", "bottom", "top")], use.names = TRUE)
    if (!is.numeric(bounds) ||
        any(!is.finite(bounds)) ||
        bounds[["left"]] < 0 ||
        bounds[["right"]] > 1 ||
        bounds[["bottom"]] < 0 ||
        bounds[["top"]] > 1 ||
        bounds[["left"]] >= bounds[["right"]] ||
        bounds[["bottom"]] >= bounds[["top"]]) {
      stop("`box_fn_bounds` values must be finite proportions with left < right and bottom < top.", call. = FALSE)
    }

    full_left <- x - half_width
    full_bottom <- y - half_height
    coordinates$left <- full_left + width * bounds[["left"]]
    coordinates$right <- full_left + width * bounds[["right"]]
    coordinates$bottom <- full_bottom + height * bounds[["bottom"]]
    coordinates$top <- full_bottom + height * bounds[["top"]]
    coordinates$x <- (coordinates$left + coordinates$right) / 2
    coordinates$y <- (coordinates$bottom + coordinates$top) / 2
    coordinates$width <- coordinates$right - coordinates$left
    coordinates$height <- coordinates$top - coordinates$bottom
    coordinates$half_width <- coordinates$width * 0.5
    coordinates$half_height <- coordinates$height * 0.5
  }

  if (!is.null(extra_coordinate_functions)) {
    assert_list(extra_coordinate_functions, names = "strict")


    available_inputs <- coordinates
    available_inputs$half_width <- half_width
    available_inputs$half_height <- half_height

    for (n in names(extra_coordinate_functions)) {
      required <- formals(extra_coordinate_functions[[n]]) %>% names()
      missing <- !(required %in% names(available_inputs))
      if (any(missing)) {
        stop(
          "The extra coordinate generated from function '", n, "'",
          " requires '", paste(required[missing], collapse = "', '"), "'",
          " but it is not among the available '", paste(sort(names(available_inputs)), collapse = "', '"), "'"
        )
      }

      args <- list()
      for (argname in required) {
        args[[argname]] <- available_inputs[[argname]]
      }
      coordinates[[n]] <- do.call(extra_coordinate_functions[[n]], args)
    }
  }

  class(coordinates) <- c("coords", class(coordinates))
  return(coordinates)
}
