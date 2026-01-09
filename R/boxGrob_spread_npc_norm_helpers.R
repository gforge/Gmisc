prAsNpc <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "unit")) return(x)
  if (is.numeric(x) && length(x) == 1) return(unit(x, "npc"))
  x
}

#' Get default from/to values for an axis
#' 
#' When axis is "x", the default is from 0 to 1 npc, i.e. left to right.
#' When axis is "y", the default is from 1 to 0 npc, i.e. top to bottom.
#' 
#' @param axis Axis, either "x" or "y"
#' #' @return A list with `from` and `to` values as `unit` objects
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
  defauls <- prGetBoxAxisDefaults(axis)
  
  .from <- prAsNpc(.from)
  .to   <- prAsNpc(.to)
  
  if (!is.null(.from) && is.null(.to)) {
    .to <- defauls$to
  } else if (is.null(.from) && !is.null(.to)) {
    .from <- defauls$from
  } else if (is.null(.from) && is.null(.to)) {  
    .from <- defauls$from
    .to   <- defauls$to
  } 
  
  list(from = .from, to = .to)
}
