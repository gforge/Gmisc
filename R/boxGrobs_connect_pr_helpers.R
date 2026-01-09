prIsBoxList <- function(x) {
  is.list(x) && length(x) > 0 && all(vapply(x, inherits, logical(1), "box"))
}

prEdgeSlots <- function(left, right, n, margin = unit(0, "mm")) {
  if (is.numeric(margin)) margin <- unit(margin, "mm")
  stopifnot(inherits(margin, "unit"))
  stopifnot(n >= 1)
  
  L <- left + margin
  R <- right - margin
  if (n == 1) return(unit.c((L + R) / 2))
  
  step <- (R - L) / (n + 1)
  xs <- L + step * seq_len(n)
  unit.c(xs)
}
