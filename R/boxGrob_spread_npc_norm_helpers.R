prAsNpc <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "unit")) return(x)
  if (is.numeric(x) && length(x) == 1) return(unit(x, "npc"))
  x
}

prNormalizeFromTo <- function(.from, .to) {
  .from <- prAsNpc(.from)
  .to   <- prAsNpc(.to)
  
  if (!is.null(.from) && is.null(.to)) {
    .to <- unit(1, "npc")
  } else if (is.null(.from) && !is.null(.to)) {
    .from <- unit(0, "npc")
  }
  
  list(from = .from, to = .to)
}
