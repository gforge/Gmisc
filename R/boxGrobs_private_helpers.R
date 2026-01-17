prAsUnit <- function(val) {
  if (is.unit(val)) {
    return(val)
  }

  return(unit(val, "npc"))
}

prCnvrtY <- function(val) {
  convertHeight(val, unitTo = "mm", valueOnly = TRUE)
}

prCnvrtX <- function(val) {
  convertWidth(val, unitTo = "mm", valueOnly = TRUE)
}

prAdjustXPos <- function(bjust, x, width) {
  width <- prCnvrtX(width)
  if (any(grepl("left", bjust))) {
    x <- x + unit(width / 2, "mm")
  } else if (any(grepl("right", bjust))) {
    x <- x - unit(width / 2, "mm")
  } else if (is.numeric(bjust)) {
    x <- x + unit(width * (0.5 - bjust[1]), "mm")
  }
  return(x)
}

prAdjustYPos <- function(bjust, y, height) {
  height <- prCnvrtY(height)
  if (any(grepl("top", bjust))) {
    y <- y - unit(height / 2, "mm")
  } else if (any(grepl("bottom", bjust))) {
    y <- y + unit(height / 2, "mm")
  } else if (is.numeric(bjust) && length(bjust) == 2) {
    y <- y + unit(height * (0.5 - bjust[2]), "mm")
  }
  return(y)
}

prGetX4Txt <- function(just, txt_padding) {
  x <- .5
  if (just == "left") {
    x <- txt_padding
  } else if (just == "right") {
    x <- unit(1, "npc") - txt_padding
  }
  return(x)
}

prConvTxt2Height <- function(str) {
  if (missing(str)) {
    return(0)
  }

  length(strsplit(as.character(str), "\n")[[1]]) %>%
    unit("lines") %>%
    prCnvrtY()
}

# Helpers for deep-path retrieval / assignment in nested box lists
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

prExtendClass <- function(obj, new_class) {
  if (!is.character(new_class) || length(new_class) != 1) {
    stop("new_class must be a single string")
  }

  class(obj) <- unique(c(new_class, class(obj)))
  obj
}
