prIsBoxList <- function(x) {
  is.list(x) && length(x) > 0 && all(vapply(x, inherits, logical(1), "box"))
}

