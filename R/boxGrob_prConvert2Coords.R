#' Converts an object to coordinates
#'
#' Sometimes we have an object that can be either a box,
#' a coordinate, a unit or a numerical value and all we
#' want is a `list` of coordinates that we can use for
#' calculating distance, alignment and other things.
#'
#' @param obj A single or a `list` of objects. Supported element types include:
#'   - `boxGrob` or `boxPropGrob`
#'   - output from `coords`
#'   - a `unit` (grid) or a numeric value coercible to an `npc` unit
#'
#'   When a `list` is provided the function recursively converts each element and
#'   returns bounding coordinates that encompass all elements. The returned
#'   units are in `npc` so the coordinates resize with the viewport.
#' @return A `list` with the points produced by `coords`. For list inputs the
#'   function returns merged bounding coordinates (class `box_coords`) with
#'   units in `npc`.
#' @details When given a list, `prConvert2Coords` computes the min/max of
#'   edges (or uses center +/- half sizes when edges are missing) to create a
#'   merged bounding box. Any additional coordinate names present on elements
#'   are merged heuristically (for example, `left*` -> min, `right*` -> max,
#'   otherwise averaged).
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
    if (length(obj) == 0) {
      stop("Expected a non-empty list of boxes/coords/units")
    }

    return(prConvertMultiple2Coords(obj))
  }

  if (inherits(obj, "box")) {
    return(coords(obj))
  }

  if (!is.unit(obj)) {
    if (!is.numeric(obj)) {
      stop(sprintf(
        "Expected a box object, coords, unit or number input; variable '%s' has value: %s",
        deparse(substitute(obj)), as.character(obj)
      ))
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
    height = 0,
    half_height = 0,
    width = 0,
    half_width = 0
  ) |>
    sapply(\(v) prAsUnit(v, units = "npc"), simplify = FALSE) |>
    structure(class = c("box_coords", "list"))
}


#' Helper to convert multiple boxes to merged bounding coordinates
#' for `prConvert2Coords()`.
#' @param obj A `list` of box-like objects.
#' @return A `list` of merged bounding coordinates with units in `npc`.
#' @noRd
prConvertMultiple2Coords <- function(obj) {
  coords_list <- lapply(obj, prConvert2Coords)

  # Per-element extractor: return numeric NPC value/size or NA_real_
  extractBoxGrobValue <- function(item, name, orient) {
    val <- item[[name]]
    if (is.null(val)) return(NA_real_)

    if (grepl("width|height|half", name)) {
      if (orient == "x") {
        return(prGetNpcSize(val, "x"))
      }
      return(prGetNpcSize(val, "y"))
    }

    if (orient == "x") return(prGetNpcValue(val, "x"))
    return(prGetNpcValue(val, "y"))
  }

  # Helper to extract numeric npc values for a given name and orientation
  getValues <- function(name, orient) {
    vals <- vapply(coords_list, function(c) extractBoxGrobValue(c, name, orient), numeric(1))
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return(NULL)
    vals
  }

  # Primary edges (in npc)
  left_vals <- getValues("left", "x")
  right_vals <- getValues("right", "x")
  top_vals <- getValues("top", "y")
  bottom_vals <- getValues("bottom", "y")

  # If some primary edges are missing, try to compute from center +/- half sizes (all in npc)
  if (is.null(left_vals) || is.null(right_vals)) {
    x_vals <- getValues("x", "x")
    half_w_vals <- getValues("half_width", "x")
    if (is.null(left_vals) && !is.null(x_vals) && !is.null(half_w_vals)) left_vals <- x_vals - half_w_vals
    if (is.null(right_vals) && !is.null(x_vals) && !is.null(half_w_vals)) right_vals <- x_vals + half_w_vals
  }
  if (is.null(top_vals) || is.null(bottom_vals)) {
    y_vals <- getValues("y", "y")
    half_h_vals <- getValues("half_height", "y")
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
    left = left_npc,
    right = right_npc,
    top = top_npc,
    bottom = bottom_npc,
    x = x_npc,
    y = y_npc,
    width = width_npc,
    height = height_npc,
    half_width = width_npc / 2,
    half_height = height_npc / 2
  ) |>
    sapply(\(v) prAsUnit(v, units = "npc"), simplify = FALSE)

  # Merge any additional coordinates (e.g., left_x, right_x, prop_x) using heuristics
  all_names <- unique(unlist(lapply(coords_list, names)))
  extra_names <- setdiff(all_names, names(out))
  for (nm in extra_names) {
    orient <- ifelse(grepl("left|right|x|width", nm), "x", "y")
    # Reuse the `get_vals()` helper to extract numeric NPC values or sizes
    # for the extra coordinate name. `get_vals()` already handles width/height
    # detection and returns NULL when no values are present.
    vals <- getValues(nm, orient)
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
