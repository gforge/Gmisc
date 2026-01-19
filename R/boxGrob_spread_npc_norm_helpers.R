#' Get default from/to values for an axis
#' Get default from/to values for an axis
#'
#' When axis is "x", the default is from 0 to 1 npc, i.e. left to right.
#' When axis is "y", the default is from 1 to 0 npc, i.e. top to bottom.
#'
#' @param axis Axis, either "x" or "y"
#' @return A list with `from` and `to` values as `unit` objects
prGetBoxAxisDefaults <- function(axis = c("x", "y")) {
  axis <- match.arg(axis)
  if (axis == "x") {
    list(from = unit(0, "npc"), to = unit(1, "npc"))
  } else {
    list(from = unit(1, "npc"), to = unit(0, "npc"))
  }
}

prNormalizeFromTo <- function(.from, .to, axis = c("x", "y")) {
  axis <- match.arg(axis)
  defaults <- prGetBoxAxisDefaults(axis)

  prAsNPC <- function(u) {
    if (is.null(u)) {
      return(NULL)
    }

    if (inherits(u, "unit")) {
      return(u)
    }

    if (is.numeric(u) && length(u) == 1) {
      return(prAsUnit(u, units = "npc"))
    }

    u
  }
  .from <- prAsNPC(.from)
  .to <- prAsNPC(.to)

  if (!is.null(.from) && is.null(.to)) {
    .to <- defaults$to
  } else if (is.null(.from) && !is.null(.to)) {
    .from <- defaults$from
  } else if (is.null(.from) && is.null(.to)) {
    .from <- defaults$from
    .to <- defaults$to
  }

  list(from = .from, to = .to)
}
