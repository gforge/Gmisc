#' Creates coordinates for box
#'
#' @param viewport_data The arguments that will be used for generating the \code{viewport}
#' @param extra_coordinate_functions A list with named functions if we want additional
#'  parameters
#' @return \code{list} of class \code{coords}
#' @importFrom checkmate assert_list
prCreateBoxCoordinates <- function(viewport_data, extra_coordinate_functions = NULL) {
  # Adjust center depending on the viewport position
  x <- prAdjustPos(viewport_data$just, viewport_data$x, viewport_data$width, axis = "x")
  y <- prAdjustPos(viewport_data$just, viewport_data$y, viewport_data$height, axis = "y")
  half_height <- unit(prGetNpcSize(viewport_data$height, "y") / 2, "npc")
  half_width <- unit(prGetNpcSize(viewport_data$width, "x") / 2, "npc")

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
