#' @title Coerce value to a unit
#' @description Ensure a value is returned as a grid `unit`. If `val` is
#'   already a `unit` it is returned unchanged; otherwise it's wrapped as
#'   `unit(..., "npc")`.
#' @param val A numeric or `unit` object.
#' @return A `unit` object.
#' @keywords internal
#' @noRd
prAsUnit <- function(val) {
  if (is.unit(val)) {
    return(val)
  }

  return(unit(val, "npc"))
}

#' @title Convert height to millimetres
#' @description Convert a grid height `unit` (or numeric) to millimetres.
#' @param val A `unit` or numeric value representing a height.
#' @return Numeric value in millimetres.
#' @keywords internal
#' @noRd
prConvertHeightToMm <- function(val) {
  convertHeight(val, unitTo = "mm", valueOnly = TRUE)
}

#' @title Convert width to millimetres
#' @description Convert a grid width `unit` (or numeric) to millimetres.
#' @param val A `unit` or numeric value representing a width.
#' @return Numeric value in millimetres.
#' @keywords internal
#' @noRd
prConvertWidthToMm <- function(val) {
  convertWidth(val, unitTo = "mm", valueOnly = TRUE)
}

#' @title Extract numeric NPC position
#' @description Obtain a numeric NPC position from a `unit` or numeric-like
#'   input. Attempts `convertX/convertY` first and falls back to numeric
#'   coercion when conversion fails. Returns `NA_real_` on failure.
#' @param u A `unit` or numeric-like value.
#' @param axis Character, either "x" or "y" to choose `convertX` or
#'   `convertY` respectively.
#' @return Numeric in NPC units or `NA_real_`.
#' @keywords internal
#' @noRd
prGetNpcValue <- function(u, axis = c("x", "y")) {
  axis <- match.arg(axis)
  conv <- if (axis == "x") convertX else convertY
  v <- tryCatch(conv(u, "npc", valueOnly = TRUE), error = function(e) NA_real_)
  if (!is.na(v)) {
    return(v)
  }

  v2 <- suppressWarnings(as.numeric(u))
  if (!is.na(v2) && length(v2) == 1 && v2 >= -1 && v2 <= 2) {
    return(v2)
  }
  NA_real_
}

#' @title Obtain size in NPC units
#' @description Convert a width/height `unit` or numeric-like value to a
#'   numeric expressed in NPC. Uses `convertWidth/convertHeight` and falls
#'   back to numeric coercion if needed.
#' @param u A `unit` or numeric-like value representing a size.
#' @param axis Character, either "x" (width) or "y" (height).
#' @return Numeric size in NPC or `NA_real_`.
#' @keywords internal
#' @noRd
prGetNpcSize <- function(u, axis = c("x", "y")) {
  axis <- match.arg(axis)
  conv <- if (axis == "x") convertWidth else convertHeight
  v <- tryCatch(conv(u, "npc", valueOnly = TRUE), error = function(e) NA_real_)
  if (!is.na(v)) {
    return(v)
  }
  v2 <- suppressWarnings(as.numeric(u))
  if (!is.na(v2) && length(v2) == 1 && v2 >= -1 && v2 <= 2) {
    return(v2)
  }
  NA_real_
}

#' @title Normalize box justification
#' @description Convert a variety of `bjust` specifications (character
#'   tokens like "left", "top", "center" or numeric vectors) into a
#'   numeric vector of length 2 representing x and y justification in
#'   [0,1]. Unspecified axis positions are returned as `NA_real_`.
#' @param bjust Character or numeric justification token(s) or `NULL`.
#' @return Numeric vector length 2: c(x, y) justification (values in [0,1]
#'   or `NA_real_` for unspecified axes).
#' @keywords internal
#' @noRd
prJustToNumeric <- function(bjust) {
  map_one <- function(s) {
    if (is.na(s)) {
      return(NA_real_)
    }
    if (is.character(s)) s <- tolower(s)
    if (is.character(s)) {
      if (s %in% c("left", "bottom")) {
        return(0)
      }
      if (s %in% c("center", "centre")) {
        return(0.5)
      }
      if (s %in% c("right", "top")) {
        return(1)
      }
      num <- suppressWarnings(as.numeric(s))
      if (!is.na(num)) {
        return(num)
      }
      return(NA_real_)
    }
    if (is.numeric(s)) {
      return(as.numeric(s))
    }
    NA_real_
  }

  if (is.null(bjust)) {
    return(c(0.5, 0.5))
  }
  if (is.numeric(bjust)) {
    if (length(bjust) == 1) {
      return(pmin(pmax(c(bjust, NA_real_), 0), 1))
    }
    vals <- as.numeric(bjust)
    if (length(vals) >= 2) vals <- vals[1:2]
    if (length(vals) == 1) vals <- c(vals, NA_real_)
    return(pmin(pmax(vals, 0), 1))
  }
  if (is.character(bjust)) {
    if (length(bjust) == 1) {
      s <- tolower(bjust)
      if (s %in% c("left", "right", "centre", "center")) {
        return(pmin(pmax(c(map_one(bjust), NA_real_), 0), 1))
      }
      if (s %in% c("top", "bottom")) {
        return(pmin(pmax(c(NA_real_, map_one(bjust)), 0), 1))
      }
      # generic single token (e.g. "center") -> apply to both
      v <- map_one(bjust)
      return(pmin(pmax(c(v, v), 0), 1))
    }
    # length >=2
    v1 <- map_one(bjust[1])
    v2 <- map_one(bjust[2])
    vals <- c(v1, v2)
    return(pmin(pmax(vals, 0), 1))
  }

  c(0.5, 0.5)
}

#' @title Fraction of a unit in NPC
#' @description Return a `unit(..., "npc")` that represents `x` divided by
#'   `divide_by`. `divide_by` may be numeric or a `unit` (in which case the
#'   unit is converted to NPC first). Useful for computing half/third
#'   fractions of a size in NPC units.
#' @param x A `unit` or numeric-like size to be expressed as a fraction.
#' @param divide_by Numeric or `unit` specifying the divisor.
#' @param axis Character, either "x" or "y" to indicate width/height.
#' @return A `unit` in NPC representing `x/divide_by`.
#' @keywords internal
#' @noRd
prUnitFraction <- function(x, divide_by, axis = c("x", "y")) {
  axis <- match.arg(axis)
  if (is.unit(divide_by)) {
    divide_by_val <- prGetNpcValue(divide_by, axis)
  } else if (is.numeric(divide_by)) {
    divide_by_val <- divide_by
  } else {
    stop("divide_by must be a numeric or unit object", call. = FALSE)
  }

  if (is.na(divide_by_val) || divide_by_val == 0) {
    stop("Cannot divide by NA or zero", call. = FALSE)
  }

  unit(prGetNpcSize(x, axis) / divide_by_val, "npc")
}

#' @title Axis-specific justification
#' @description Extract the numeric justification for a single axis from a
#'   `bjust` specification. Returns `NA_real_` when the axis is unspecified.
#' @param bjust Character or numeric justification token(s).
#' @param axis Character, either "x" or "y".
#' @return Numeric in [0,1] or `NA_real_`.
#' @keywords internal
#' @noRd
prJustForAxis <- function(bjust, axis = c("x", "y")) {
  axis <- match.arg(axis)
  bnum <- prJustToNumeric(bjust)
  if (axis == "x") {
    return(bnum[1])
  }
  bnum[2]
}

#' @title Adjust position by justification
#' @description Adjust a `pos` (grid `unit`) by the half-size offset implied
#'   by `bjust` for one axis. If the axis justification is unspecified
#'   (`NA`) the original `pos` is returned unchanged.
#' @param bjust Character or numeric justification token(s).
#' @param pos A grid `unit` representing the position to adjust.
#' @param size A grid `unit` or numeric-like size used to compute the offset.
#' @param axis Character, either "x" or "y".
#' @return Adjusted `unit` position.
#' @keywords internal
#' @noRd
prAdjustPos <- function(bjust, pos, size, axis = c("x", "y")) {
  axis <- match.arg(axis)
  just_val <- prJustForAxis(bjust, axis)
  if (is.na(just_val)) {
    return(pos)
  }
  offset <- prGetNpcSize(size, axis) * (0.5 - just_val)
  pos + unit(offset, "npc")
}

#' @title Compute x position for text within a box
#' @description Helper to compute an x position for text inside a box given a
#'   simple `just` token and a `txt_padding` unit.
#' @param just Character like "left", "right", or other.
#' @param txt_padding A `unit` used as padding from the box edge.
#' @return A `unit` representing the x coordinate.
#' @keywords internal
#' @noRd
prGetX4Txt <- function(just, txt_padding) {
  x <- .5
  if (just == "left") {
    x <- txt_padding
  } else if (just == "right") {
    x <- unit(1, "npc") - txt_padding
  }
  return(x)
}

#' @title Convert text to height (mm)
#' @description Compute the height in millimetres of a multi-line text
#'   string, using the number of lines and converting `lines` to mm.
#' @param str Character string (may contain newlines).
#' @return Numeric height in millimetres.
#' @keywords internal
#' @noRd
prConvTxt2Height <- function(str) {
  if (missing(str)) {
    return(0)
  }

  length(strsplit(as.character(str), "\n")[[1]]) |>
    unit("lines") |>
    prConvertHeightToMm()
}

# Helpers for deep-path retrieval / assignment in nested box lists
#' @title Safe deep-list retrieval
#' @description Retrieve a nested element from a list by a path specified as
#'   a vector/list of names or numeric indices. Returns `NULL` when the path
#'   is invalid or not applicable.
#' @param x A list-like object to index into.
#' @param path Character or numeric vector (or list) specifying the path.
#' @return The located element or `NULL` if not found/invalid.
#' @keywords internal
#' @noRd
get_list_element_by_path <- function(x, path) {
  if (is.null(path)) {
    return(NULL)
  }
  # Only atomic vectors (character/numeric) or lists are valid paths. For
  # other object types (e.g., a `unit`), return NULL rather than attempting to
  # coerce and index, which can produce confusing errors.
  if (!is.list(path)) {
    # If the path is a unit (or similar), treat as not a path.
    if (exists("is.unit", mode = "function") && is.unit(path)) {
      return(NULL)
    }
    if (!is.atomic(path)) {
      return(NULL)
    }
    path <- as.list(path)
  }

  for (seg in path) {
    # Accept numeric indices (including numeric-strings) or character names
    if (is.numeric(seg) || (is.character(seg) && grepl("^[0-9]+$", seg))) {
      idx <- as.integer(seg)
      # Numeric indices must be valid 1-based indices into the current list
      if (!is.list(x) || length(idx) == 0 || idx < 1 || idx > length(x)) {
        return(NULL)
      }
    } else if (is.character(seg)) {
      idx <- seg
      # If the name isn't present, treat as not-found
      if (!is.list(x) || is.null(names(x)) || !(idx %in% names(x))) {
        return(NULL)
      }
    } else {
      # Non-character, non-numeric segment (e.g., a unit object) is not a valid path
      return(NULL)
    }

    if (is.null(x)) {
      return(NULL)
    }
    x <- x[[idx]]
  }
  x
}

#' @title Safe deep-list assignment
#' @description Set a nested element inside a list by path, creating
#'   intermediate lists as needed.
#' @param x A list to modify.
#' @param path Character or numeric vector (or list) specifying the path.
#' @param value Value to assign at the specified path.
#' @return Modified list with `value` inserted.
#' @keywords internal
#' @noRd
set_list_element_by_path <- function(x, path, value) {
  if (!is.list(path)) path <- as.list(path)
  if (length(path) == 0) {
    return(x)
  }
  seg <- path[[1]]
  if (is.numeric(seg) || (is.character(seg) && grepl("^[0-9]+$", seg))) seg <- as.integer(seg)
  if (length(path) == 1) {
    x[[seg]] <- value
    return(x)
  }
  if (is.null(x[[seg]])) x[[seg]] <- list()
  x[[seg]] <- set_list_element_by_path(x[[seg]], path[-1], value)
  x
}

#' @title Extend object class
#' @description Prepend `new_class` to an object's class attribute, ensuring
#'   uniqueness.
#' @param obj An R object.
#' @param new_class Single string with the class name to add.
#' @return The object with updated class attribute.
#' @keywords internal
#' @noRd
prExtendClass <- function(obj, new_class) {
  if (!is.character(new_class) || length(new_class) != 1) {
    stop("new_class must be a single string")
  }

  class(obj) <- unique(c(new_class, class(obj)))
  obj
}
