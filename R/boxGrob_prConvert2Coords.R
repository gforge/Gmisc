#' Converts an object to coordinates
#'
#' Sometimes we have an object that can be either a box,
#' a coordinate, a unit or a numerical value and all we
#' want is a `list` of coordinates that we can use for
#' calculating distance, alignment and other things.
#'
#' @param obj A single or a `list` of `[`boxGrob`], `[`boxPropGrob`], `[`coords`] output, `[`unit`][grid::unit]
#'   or a numeric value that can be converted to an `npc` `[`unit`][grid::unit].
#'   If a `list` is provided the function recursively converts each element and returns bounding coordinates
#'   that encompass all elements; the returned units are in `npc` so the coordinates resize with the viewport.
#' @return A `list` with all the points that [`coords`] returns. For list inputs the function returns merged
#'   bounding coordinates (class `box_coords`) with units in `npc`.
#' @details When given a list, `prConvert2Coords` computes the min/max of edges (or uses center +/- half sizes
#'   if edges are missing) to create a merged bounding box. Any additional coordinate names present on elements
#'   are merged heuristically (e.g., `left*` -> min, `right*` -> max, else averaged).
#' @examples
#' box1 <- boxGrob("A", x = .2, y = .8)
#' box2 <- boxGrob("B", x = .6, y = .4)
#' Gmisc:::prConvert2Coords(list(box1, box2))
#' @md
prConvert2Coords <- function(obj) {
  if (inherits(obj, "coords")) {
    return(obj)
  }

  # If a list is provided, recursively convert each element and merge edges
  if (inherits(obj, "list") && !is.grob(obj) && !is.unit(obj)) {
    if (length(obj) == 0) stop("Expected a non-empty list of boxes/coords/units")

    coords_list <- lapply(obj, prConvert2Coords)

    # Helper to extract numeric npc values for a given name and orientation
    get_vals <- function(name, orient) {
      vals <- sapply(coords_list, function(c) {
        if (is.null(c[[name]])) {
          return(NA_real_)
        }

        if (orient == "x") {
          convertWidth(c[[name]], "npc", valueOnly = TRUE)
        } else {
          convertHeight(c[[name]], "npc", valueOnly = TRUE)
        }
      })
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0) {
        return(NULL)
      }
      return(vals)
    }

    # Primary edges (in npc)
    left_vals <- get_vals("left", "x")
    right_vals <- get_vals("right", "x")
    top_vals <- get_vals("top", "y")
    bottom_vals <- get_vals("bottom", "y")

    # If some primary edges are missing, try to compute from center +/- half sizes (all in npc)
    if (is.null(left_vals) || is.null(right_vals)) {
      x_vals <- get_vals("x", "x")
      half_w_vals <- get_vals("half_width", "x")
      if (is.null(left_vals) && !is.null(x_vals) && !is.null(half_w_vals)) left_vals <- x_vals - half_w_vals
      if (is.null(right_vals) && !is.null(x_vals) && !is.null(half_w_vals)) right_vals <- x_vals + half_w_vals
    }
    if (is.null(top_vals) || is.null(bottom_vals)) {
      y_vals <- get_vals("y", "y")
      half_h_vals <- get_vals("half_height", "y")
      if (is.null(top_vals) && !is.null(y_vals) && !is.null(half_h_vals)) top_vals <- y_vals + half_h_vals
      if (is.null(bottom_vals) && !is.null(y_vals) && !is.null(half_h_vals)) bottom_vals <- y_vals - half_h_vals
    }

    if (is.null(left_vals) || is.null(right_vals) || is.null(top_vals) || is.null(bottom_vals)) {
      stop("Could not determine bounding edges for provided list")
    }

    left_npc <- min(left_vals)
    right_npc <- max(right_vals)
    top_npc <- max(top_vals)
    bottom_npc <- min(bottom_vals)

    x_npc <- (left_npc + right_npc) / 2
    y_npc <- (top_npc + bottom_npc) / 2
    width_npc <- right_npc - left_npc
    height_npc <- top_npc - bottom_npc

    out <- list(
      left = unit(left_npc, "npc"),
      right = unit(right_npc, "npc"),
      top = unit(top_npc, "npc"),
      bottom = unit(bottom_npc, "npc"),
      x = unit(x_npc, "npc"),
      y = unit(y_npc, "npc"),
      width = unit(width_npc, "npc"),
      height = unit(height_npc, "npc"),
      half_width = unit(width_npc / 2, "npc"),
      half_height = unit(height_npc / 2, "npc")
    )

    # Merge any additional coordinates (e.g., left_x, right_x, prop_x) using heuristics
    all_names <- unique(unlist(lapply(coords_list, names)))
    extra_names <- setdiff(all_names, names(out))
    for (nm in extra_names) {
      orient <- ifelse(grepl("left|right|x|width", nm), "x", "y")
      vals <- sapply(coords_list, function(c) {
        if (!is.null(c[[nm]])) {
          if (orient == "x") convertWidth(c[[nm]], "npc", valueOnly = TRUE) else convertHeight(c[[nm]], "npc", valueOnly = TRUE)
        } else {
          NA_real_
        }
      })
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0) next

      if (grepl("left", nm)) {
        v <- min(vals)
      } else if (grepl("right", nm)) {
        v <- max(vals)
      } else if (grepl("top", nm)) {
        v <- max(vals)
      } else if (grepl("bottom", nm)) {
        v <- min(vals)
      } else {
        v <- mean(vals)
      }

      out[[nm]] <- unit(v, "npc")
    }

    return(structure(out, class = c("box_coords", "list")))
  }

  if (inherits(obj, "box")) {
    return(coords(obj))
  }

  if (!is.unit(obj)) {
    if (!is.numeric(obj)) {
      stop("Expected a box object, coords, unit or number input, the variable '", substitute(obj), "' has value: ", obj)
    }
    obj <- unit(obj, "npc")
  }

  list(
    y = obj,
    x = obj,
    top = obj,
    bottom = obj,
    left = obj,
    right = obj,
    height = unit(0, "npc"),
    half_height = unit(0, "npc"),
    width = unit(0, "npc"),
    half_width = unit(0, "npc")
  ) |>
    structure(class = c("box_coords", "list"))
}
